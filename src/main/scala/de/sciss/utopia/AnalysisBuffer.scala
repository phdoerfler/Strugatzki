/*
 *  AnalysisBuffer.scala
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

import java.nio.{ByteBuffer, FloatBuffer}
import de.sciss.synth.Model

object AnalysisBuffer {
   case class FrameUpdated( idx: Int, lastFrame: Boolean )
   type Frame = Array[ Float ]

   val anaFFTSize       = 1024
   val anaFFTOver       = 2
   val anaWinStep       = anaFFTSize / anaFFTOver
   val numMelCoeffs     = 13
   val anaChans         = numMelCoeffs

   private val normMins = Array(
      -0.47940505f, -0.5093739f,  -0.22388703f, -0.14356878f,  -0.057697684f, -0.10735649f,
      -0.052598f,   -0.060314894f, 0.0f,        -0.043890893f, -0.028240174f, -0.010011315f, -0.07498413f )
   private val normMaxs = Array(
       1.9825932f,   1.085732f,    0.9282071f,   0.76045084f,   0.79747903f,   0.6042967f,
       0.631527f,    0.6167193f,   0.6120175f,   0.61750406f,   0.62154025f,   0.5441396f, 0.5421591f )
   val normAdd = normMins.map( d => -d )
   val normMul = normMins.zip( normMaxs ).map( tup => 1.0f / (tup._2 - tup._1) )

   assert( normMins.size == numMelCoeffs && normMaxs.size == numMelCoeffs )
}

class AnalysisBuffer( val numFrames: Int, val numChannels: Int, val sampleRate: Double ) extends Model {
   import AnalysisBuffer._

   private val buf = ByteBuffer.allocateDirect( numFrames * numChannels * 4 ).asFloatBuffer
   private var framesWrittenVar = 0

   def emptyFrame : Frame = new Array[ Float ]( numChannels )
//   def view : FloatBuffer = buf.duplicate
   def setFrame( idx: Int, content: Frame ) {
      val lastFrame = buf.synchronized {
         if( idx > framesWrittenVar ) {
            println( "!!! skipped frames. written = " + framesWrittenVar + " ; set = " + idx )
            var i = framesWrittenVar; while( i < idx ) {
            buf.position( i * numChannels )
            buf.put( content )
            i += 1 }
         }
         buf.position( idx * numChannels )
         buf.put( content )
         val res = idx >= framesWrittenVar
         if( res ) {
            framesWrittenVar = idx + 1
         }
         res
      }
      dispatch( FrameUpdated( idx, lastFrame ))
   }

   def framesWritten : Int = buf.synchronized( framesWrittenVar )

   def getFrame( idx: Int, holder: Frame = emptyFrame ) : Frame = {
      buf.synchronized {
         buf.position( idx * numChannels )
         buf.get( holder )
      }
      holder
   }
}