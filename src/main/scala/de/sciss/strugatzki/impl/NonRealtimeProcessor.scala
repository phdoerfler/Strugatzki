package de.sciss.strugatzki
package impl

import de.sciss.synth.io.{AudioFile, AudioFileSpec, SampleFormat, AudioFileType}
import de.sciss.synth.{ControlSetMap, ugen, GE, Buffer, Synth, SynthDef, Server}
import de.sciss.osc.{PacketCodec, Bundle}
import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer
import sys.process.{ProcessLogger, Process}
import concurrent.{ExecutionContext, Future, blocking, future}

object NonRealtimeProcessor {
  final case class BufferSpec(control: String, numFrames: Int, numChannels: Int = 1)
  final case class RenderConfig(inFile: File, inSpec: AudioFileSpec,
                                numFeatures: Int, stepSize: Int,
                                blockSize: Int = 64, buffers: List[BufferSpec] = Nil,
                                outFile: Option[File] = None, fixNaNs: Boolean = true,
                                progress: Float => Unit = _ => (),
                                checkAborted: () => Unit = () => ())

  def render[A](config: RenderConfig)(extract: GE => GE)
               (implicit exec: ExecutionContext): Future[File] = future { blocking {

    val inChannels        = config.inSpec.numChannels
    val sampleRate        = config.inSpec.sampleRate
    val stepSize          = config.stepSize
    val inFrames          = config.inSpec.numFrames
    val outFile           = config.outFile getOrElse IOUtil.createTempFile("tmp", ".aif")

    val so                = Server.Config()
    val oscF              = IOUtil.createTempFile("tmp", ".osc")
    val dummyOutput       = IOUtil.createTempFile("tmp", ".aif")
    so.inputBusChannels   = inChannels
    so.sampleRate         = sampleRate.toInt
    so.outputBusChannels  = 1
    so.nrtCommandPath     = oscF.getAbsolutePath
    so.nrtInputPath       = Some(config.inFile.getAbsolutePath)
    so.nrtOutputPath      = dummyOutput.getAbsolutePath
    so.nrtHeaderFormat    = AudioFileType.AIFF
    so.nrtSampleFormat    = SampleFormat.Int16

    val s                 = Server.dummy("nrt", so)
    val featBufSize       = 1024
    val featBuf           = new Buffer(s)
    val featRate          = sampleRate / stepSize

    val df = SynthDef("nrt") {
      import ugen._
      val chanOff = NumOutputBuses.ir
      val in      = In.ar(chanOff, inChannels)
      val feat    = extract(in)
      val trig    = Impulse.kr(featRate) // - Impulse.kr( 0 )
      val phaseHi = featBufSize - 1
      val phase   = Stepper.kr(trig, 0, 0, phaseHi, 1, phaseHi)
      BufWr.kr(in = feat, buf = featBuf.id, index = phase)
    }

    val syn       = Synth(s)
    val outFrames = ((inFrames + stepSize - 1) / stepSize).toInt // + 1
    val numWrites = (outFrames + featBufSize - 1) / featBufSize

    val tmpNames  = Array.fill(numWrites)(IOUtil.createTempFile("feat_part", ".aif"))

    val bufBndls  = Array.tabulate(numWrites) { i =>
      val startFrame  = i * featBufSize
      val stopFrame   = math.min(outFrames, startFrame + featBufSize)
      val numFrames   = stopFrame - startFrame

      val msg         = featBuf.writeMsg(
        path          = tmpNames(i).getAbsolutePath,
        fileType      = AudioFileType.AIFF,
        sampleFormat  = SampleFormat.Float,
        numFrames     = if (i == 0) numFrames - 1 else numFrames,
        startFrame    = if (i == 0) 1 else 0,
        leaveOpen     = false
      )

      // i don't know... in theory i should be writing the half a frame before or after,
      // but that causes trouble. so just write it exactly at the Stepper boundaries
      // and hope it scales up to long durations :-(
      val time = (stopFrame - 0.0) / featRate
      Bundle.secs(time, msg)
    }

    val (userMsgs, userArgs) = config.buffers.map { case BufferSpec(bufName, bufFrames, bufChans) =>
      val buf = Buffer(s)
      val msg = buf.allocMsg(numFrames = bufFrames, numChannels = bufChans)
      val set = ControlSetMap.Single(key = bufName, value = buf.id)
      msg -> set
    } .unzip

    val renderMsgs = featBuf.allocMsg(featBufSize, config.numFeatures) :: df.recvMsg ::
      syn.newMsg(df.name, s.rootNode, args = userArgs) :: Nil

    val initBndl = Bundle.secs(0.0, (userMsgs ::: renderMsgs): _*)

    val bndls = initBndl +: bufBndls // don't bother about n_free and b_free

    val c   = PacketCodec().scsynth().build
    val sz  = bndls.map(_.encodedSize(c)).max
    val raf = new RandomAccessFile(oscF, "rw")
    val bb  = ByteBuffer.allocate(sz)
    val fch = raf.getChannel
    bndls.foreach { bndl =>
      bndl.encode(c, bb)
      bb.flip
      raf.writeInt(bb.limit)
      fch.write(bb)
      bb.clear
    }
    raf.close()

    // val dur = Bundle.timetagToSecs( bufBndls.last.timetag )
    // val dur = bufBndls.last.timetag.toSecs
    val dur = bndls.last.timetag.toSecs

    val procArgs    = so.toNonRealtimeArgs
    val procBuilder = Process(procArgs, Some(new File(so.programPath).getParentFile))
    lazy val log: ProcessLogger = new ProcessLogger {
      def buffer[T](f: => T): T = f

      // ???
      def out(line: => String) {
        if (line.startsWith("nextOSCPacket")) {
          val time = line.substring(14).toFloat
          val prog = time / dur * 0.8 // first 80%
          config.progress(prog.toFloat)
          try {
            config.checkAborted()
          } catch {
            case Processor.Aborted() =>
              proc.destroy()
          }
        } else if (line != "start time 0") {
          Console.out.println(line)
        }
      }

      def err(line: => String) {
        Console.err.println(line)
      }
    }
    lazy val proc: Process = procBuilder.run(log)

    val res = proc.exitValue() // blocks
    oscF.delete()
    dummyOutput.delete()
    config.checkAborted()
    if (res != 0) throw new RuntimeException("scsynth failed with exit code " + res)

    val afOutS  = AudioFileSpec(AudioFileType.AIFF, SampleFormat.Float, config.numFeatures, featRate)
    val afOut   = AudioFile.openWrite(outFile, afOutS)
    try {
      var iw = 0; while(iw < numWrites) {
        config.checkAborted()
        val afIn  = AudioFile.openRead(tmpNames(iw))
        val b     = try {
          val _b = afIn.buffer(afIn.numFrames.toInt)
          afIn.read(_b)
          _b
        } finally {
          afIn.close()
        }

        if (config.fixNaNs) {
          val lasts = new Array[Float](afIn.numChannels)
          // deal with NaNs
          var ch = 0; while(ch < afIn.numChannels) {
            val cb    = b(ch)
            var last  = lasts(ch)
            var ib = 0; while(ib < cb.length) {
              val f = cb(ib)
              if (f.isNaN) cb(ib) = last else last = f
            ib += 1 }
            lasts(ch) = last
          ch += 1 }
        }

        afOut.write(b)
        afIn.file.foreach(_.delete())
        val prog = ((iw + 1).toFloat / numWrites * 0.2f) + 0.8f  // last 20%
        config.progress(prog)
      iw += 1 }
    } finally {
      afOut.close()
    }

    outFile
  }}
}