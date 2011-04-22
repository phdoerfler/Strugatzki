/*
 *  Similarity.scala
 *  (InterPlay)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.utopia

import actors.Actor
//import InterPlay._
//import SoundProcesses._
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq, Map => IMap}
import java.io.{FileFilter, File}

object Similarity {
   var verbose = true
   val name = "Similarity"

   sealed trait Type

   // principal component analysis through singular value decomposition
   case object PCA extends Type

   // normalized cross correlation ( http://en.wikipedia.org/wiki/Cross-correlation )
   case object CC extends Type

//   private val sync = new AnyRef

   // ---- actor messages ----
   private case class Search( temp: Template, minThresh: Float, maxNum: Int, integWin: Float, result: IIdxSeq[ (Int, Float) ] => Unit )

//   private lazy val actor = (new Actor { def act = loop { react {
//      case s: Search => s.result( searchAct( s ))
//      case x => println( name + ": Unknown message " + x )
//   }}}).start

//   lazy val templates: IMap[ String, Template ] = TEMPLATE_PATH.listFiles( new FileFilter {
//      def accept( f: File ) = try {
//         val spec = AudioFile.readSpec( f )
//         spec.numChannels == anaClientBuf.numChannels
//      } catch { case _ => false }
//   }).map( f => {
//      val af   = AudioFile.openRead( f )
//      val mat  = Mat( af.numFrames.toInt, anaClientBuf.numChannels )
//      af.readFrames( mat.arr )
//      af.close
//      val name0= f.getName()
//      val name = name0.substring( 0, name0.lastIndexOf( '.' ))
//      name -> Template( name, mat )
//   })( breakOut )

//   def search( temp: Template, minThresh: Float, maxNum: Int, integWin: Float )( result: IIdxSeq[ (Int, Float) ] => Unit ) {
//      actor ! Search( temp, minThresh, maxNum, integWin, result )
//   }

//   def test( off: Int, svnFrames: Int, normalize: Boolean = false, tpe: Type = CC ) {
//      Actor.actor( testAct( off, svnFrames, normalize, tpe ))
//   }

//   private def searchAct( s: Search ) : IIdxSeq[ (Int, Float) ] = {
//      if( verbose ) println( name + ": Start search : " + s )
//
//      val buf           = anaClientBuf
//      val numFrames     = buf.framesWritten
//println( "framesWritten: " + numFrames )
//
//      val integFrames   = (s.integWin * buf.sampleRate + 0.5).toInt
////      if( buf.framesWritten < integFrames ) return IIdxSeq.empty
//      val srcMat        = s.temp.mat
//      val n             = srcMat.numFrames
//      val dstMat        = Mat( n, buf.numChannels )
//      var lastHitOff    = -integFrames
//      var lastHitCorr   = 0f
//      var res           = IIdxSeq.empty[ (Int, Float) ]
//
//      def addLastHit() = if( lastHitOff >= 0 ) res :+= (lastHitOff, lastHitCorr)
//
//      var off = 0; while( off < numFrames - n ) {
//         prepare( off, dstMat )
//         val c = xcorr( srcMat )( dstMat )
//         if( c > s.minThresh ) {
//            if( off - lastHitOff < integFrames ) {
//               if( c > lastHitCorr ) {
//                  lastHitCorr = c
//                  lastHitOff  = off
//               }
//            } else {
//               addLastHit()
//               lastHitCorr = c
//               lastHitOff  = off
//            }
//         }
//      off += 1 }
//
//      addLastHit()
//      res = res.sortWith( (a, b) => a._2 > b._2 ).take( s.maxNum )
//      if( verbose ) println( name + ": Done search : " + res )
//      res
//   }

//   private def testAct( off0: Int, svnFrames0: Int, normalize: Boolean, tpe: Type ) {
//      println( "SVD STARTED" )
//
//      val buf        = anaClientBuf
//      val path       = File.createTempFile( "svd", ".aif", new File( System.getProperty( "user.home" ), "Desktop" ))
//      val af         = AudioFile.openWrite( path, AudioFileSpec( numChannels = 1, sampleRate = buf.sampleRate ))
//      val afBuf      = af.frameBuffer( 1024 )
//      val afChan     = afBuf( 0 )
//      var bufPos     = 0
//
//      val numFrames  = buf.framesWritten
//      val srcFrame   = buf.emptyFrame
//      val dstFrame   = buf.emptyFrame
//      val m          = buf.numChannels
//
//      // ---- normalize ----
//      if( normalize ) {
//         val dmin = new Array[Double]( m )
//         val dmax = new Array[Double]( m )
//         buf.getFrame( 0, srcFrame )
//         var y = 0; while( y < m ) {
//             dmin(y)=srcFrame(y); dmax(y)=srcFrame(y)
//         y += 1 }
//         var x = 1; while( x < numFrames ) {
//             buf.getFrame( x, srcFrame )
//             var y = 0; while( y < m ) {
//                 dmin(y)=math.min(dmin(y),srcFrame(y)); dmax(y)=math.max(dmax(y),srcFrame(y))
//             y += 1 }
//         x += 1 }
//         x = 0; while( x < numFrames ) {
//             buf.getFrame( x, srcFrame )
//             var y = 0; while( y < m ) {
//                 srcFrame(y) = ((srcFrame(y) - dmin(y)) / (dmax(y) - dmin(y))).toFloat
//             y += 1 }
//             buf.setFrame( x, srcFrame )
//         x += 1 }
//      }
//
//      val off        = math.max( 0, math.min( numFrames - svnFrames0, off0 ))
//      val n          = math.min( svnFrames0, numFrames - off )
//
//      val process: Int => Float = tpe match {
//         case PCA =>
//            val mat        = Array.ofDim[ Float ]( m, n )
//            val ns         = math.min( m + 1, n )
//            val s          = new Array[ Float ]( ns )
//            val u          = Array.ofDim[ Float ]( m, math.min( m, n ))
//
//            def readAndDecompose( off: Int, frame: AnalysisBuffer.Frame ) {
//               var idx = off; var x = 0; while( x < n ) {
//                  buf.getFrame( idx, frame )
//                  var y = 0; while( y < m ) {
//                     mat( y )( x ) = frame( y )
//                  y += 1 }
//               idx += 1; x += 1 }
//
//               // result vector is (0 until m).map( u => u( i )( 0 ) * s( 0 ))
//               SVD.svd( mat, s, u, null )
//               val gain = s( 0 )
//               var y = 0; while( y < m ) {
//                  frame( y ) = u( y )( 0 ) * gain
//               y += 1 }
//            }
//
//            readAndDecompose( off, srcFrame )
//
//            (off: Int) => {
//               readAndDecompose( off, dstFrame )
//               var y = 0; var difsum = 0.0; while( y < m ) {
//                  val src = srcFrame( y )
//                  val dst = dstFrame( y )
//                  if( src != 0 ) {
//                     val d = (dst - src) / src
//                     difsum += d * d
//                  }
//               y += 1 }
//               (difsum / m).toFloat
//            }
//
//         case CC =>
//            val srcMat  = Mat( n, buf.numChannels )
//            val dstMat  = Mat( n, buf.numChannels )
//
//            prepare( off, srcMat )
//            val corrFun = xcorr( srcMat )( _ )
//
//            (off: Int) => {
//               prepare( off, dstMat )
//               corrFun( dstMat )
//            }
//      }
//
//      var x = 0; while( x < numFrames - n ) {
//         afChan( bufPos ) = process( x )
//         bufPos += 1
//         if( bufPos == 1024 ) {
//            af.writeFrames( afBuf )
//            println( "---- " + af.numFrames )
//            bufPos = 0
//         }
//      x += 1 }
//
//      af.writeFrames( afBuf, 0, bufPos )
//      println( "---- " + af.numFrames + " : DONE!" )
//      af.close
//   }
//
//   private def readMat( off: Int, mat: Mat ) {
//      var idx = off; var x = 0; while( x < mat.numFrames ) {
//         anaClientBuf.getFrame( idx, mat.arr( x ))
//      x += 1; idx += 1 }
//   }

   private def stat( mat: Mat ) : (Double, Double) = {
      var sum = 0.0
      var x = 0; while( x < mat.numFrames ) {
         val frame = mat.arr( x )
         var y = 0; while( y < mat.numChannels ) {
            sum += frame( y )
         y += 1 }
      x += 1 }
      val mean = sum / mat.size
      sum = 0.0
      x = 0; while( x < mat.numFrames ) {
         val frame = mat.arr( x )
         var y = 0; while( y < mat.numChannels ) {
            val d = frame( y ) - mean
            sum += d * d
         y += 1 }
      x += 1 }
      val stddev = math.sqrt( sum / mat.size )
      (mean, stddev)
   }

   def norm( mat: Mat ) {
      val (mean, stddev) = stat( mat )
      val add = -mean
      val mul = 1.0 / stddev
      var x = 0; while( x < mat.numFrames ) {
         val frame = mat.arr( x )
         var y = 0; while( y < mat.numChannels ) {
            frame( y ) = ((frame( y ) + add) * mul).toFloat
         y += 1 }
      x += 1 }
   }

//   private def prepare( off: Int, mat: Mat ) {
//      readMat( off, mat )
//      norm( mat )
//   }

   def xcorr( a: Similarity.Mat )( b: Similarity.Mat ) : Float = {
      var sum = 0.0
      var x = 0; while( x < a.numFrames ) {
         val af = a.arr( x )
         val df = b.arr( x )
         var y = 0; while( y < a.numChannels ) {
            sum += af( y ) * df( y )
         y += 1 }
      x += 1 }
      (sum / (a.size - 1)).toFloat
   }

//   private def xcorr( a: Mat, b: Mat ) : Double = {
//      var sum = 0.0
//      var x = 0; while( x < a.numFrames ) {
//         val af = a.arr( x )
//         val df = b.arr( x )
//         var y = 0; while( y < a.numChannels ) {
//            sum += af( y ) * df( y )
//         y += 1 }
//      x += 1 }
//      sum / (a.size - 1)
//   }

//   def saveTemplate( off: Int, numFrames: Int, name: String ) {
//      val mat = Mat( numFrames, anaClientBuf.numChannels )
//      prepare( off, mat )
//      val f = new File( TEMPLATE_PATH, name + ".aif" )
//      val af = AudioFile.openWrite( f, AudioFileSpec( numChannels = mat.numChannels, sampleRate = anaClientBuf.sampleRate ))
//      af.writeFrames( mat.arr )
//      af.close
//   }

   object Mat {
//      def apply( buf: AnalysisBuffer, numFrames: Int ) : Mat =
//         apply( buf, Array.ofDim[ Float ]( numFrames, buf.numChannels ), numFrames, buf.numChannels )
      def apply( numFrames: Int, numChannels: Int ) : Mat =
         apply( Array.ofDim[ Float ]( numFrames, numChannels ), numFrames, numChannels )
   }
   case class Mat( arr: Array[ Array[ Float ]], numFrames: Int, numChannels: Int ) {
      val size = numFrames * numChannels

      def getFrame( idx: Int, holder: Array[ Float ]) {
         System.arraycopy( arr( idx ), 0, holder, 0, numChannels )
      }
   }

   case class Template( name: String, mat: Mat )
}