/*
 *  FeatureSegmentationImpl.scala
 *  (Strugatzki)
 *
 *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
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
 */

package de.sciss.strugatzki
package impl

import xml.XML
import de.sciss.synth.io.AudioFile
import java.io.File
import collection.immutable.{SortedSet => ISortedSet}
import de.sciss.span.Span
import de.sciss.processor.impl.ProcessorImpl

private[strugatzki] final class FeatureSegmentationImpl(val config: FeatureSegmentation.Config)
  extends FeatureSegmentation with ProcessorImpl[FeatureSegmentation.Product, FeatureSegmentation] {
   import FeatureSegmentation._

   protected def body(): Product = {
      import FeatureExtraction.{ Config => ExtrConfig }

      val extr       = ExtrConfig.fromXML( XML.loadFile( config.metaInput ))
      val stepSize   = extr.fftSize / extr.fftOverlap

      def fullToFeat( n: Long ) = ((n + (stepSize >> 1)) / stepSize).toInt
      def featToFull( i: Int )  = i.toLong * stepSize

      val normBuf = if( config.normalize ) {
         val afNorm = AudioFile.openRead( new File( config.databaseFolder, Strugatzki.NORMALIZE_NAME ))
         try {
            require( (afNorm.numChannels == extr.numCoeffs + 1) && afNorm.numFrames == 2L )
            val b = afNorm.buffer( 2 )
            afNorm.read( b )
            b
         } finally {
            afNorm.close()
         }
      } else null // None

      val halfWinLen = fullToFeat( config.corrLen )
      val tempWeight = config.temporalWeight

      var prio     = ISortedSet.empty[ Break ]( BreakMaxOrd )
      var lastBreak : Break = null

      def entryHasSpace = {
         prio.size < config.numBreaks
      }

      def highestSim = {
         if( prio.nonEmpty ) prio.last.sim
         else 0f // Float.NegativeInfinity
      }

      // adds a break to the entry's priority queue. if the queue grows beyond numBreaks,
      // truncates the queue. if the match collides with a previous match that is closer
      // than minSpacing, it is either dropped (if the similarity is equal or smaller) or replaces
      // the previous match (if the similarity is greater).
      def addBreak( b: Break ) {
         if( (lastBreak != null) && ((b.pos - lastBreak.pos) < config.minSpacing) ) {
            // gotta collapse them
            if( lastBreak.sim > b.sim ) {  // ok, replace previous match
               prio     -= lastBreak
               prio     += b
               lastBreak = b
            } // otherwise ignore the new match
         } else {
            prio     += b
            if( prio.size > config.numBreaks ) {
               prio -= prio.last   // faster than dropRight( 1 ) ?
            }
            lastBreak = b
         }
      }

      val winLen  = halfWinLen * 2
      val eInBuf  = Array.ofDim[ Float ]( extr.numCoeffs + 1, winLen )

      val afExtr = AudioFile.openRead( extr.featureOutput )
      try {
        val afStart = config.span match {
          case Span.HasStart(s) => math.max(0, fullToFeat(s))
          case _ => 0
        }
        val afStop = config.span match {
          case Span.HasStop(s) => math.min(afExtr.numFrames.toInt, fullToFeat(s))
          case _ => afExtr.numFrames.toInt
        }
        val afLen = afStop - afStart

         if( afStart > 0 ) afExtr.seek( afStart )
         var left       = afLen // afExtr.numFrames
         var readSz     = winLen   // read full buffer in first round
         var readOff    = 0
         var logicalOff = 0
         var progBlock  = 0

         while( left > 0 ) {
            checkAborted()

            val chunkLen   = math.min( left, readSz ) // .toInt
            afExtr.read( eInBuf, readOff, chunkLen )
            val eInBufOff = logicalOff % winLen
            MathUtil.normalize( normBuf, eInBuf, readOff, chunkLen )
            val temporal = if( tempWeight > 0f ) {
               MathUtil.correlateHalf( 1, halfWinLen, eInBuf, eInBufOff, 0 )
            } else 0f
            val spectral = if( tempWeight < 1f ) {
               MathUtil.correlateHalf( extr.numCoeffs, halfWinLen, eInBuf, eInBufOff, 1 )
            } else 0f
            val sim = temporal * tempWeight + spectral * (1f - tempWeight)
            if( entryHasSpace || sim < highestSim ) {
               val pos     = featToFull( afStart + logicalOff + halfWinLen )
               val b       = Break( sim, pos )
               addBreak( b )
            }
            left   -= chunkLen
            readOff = (readOff + chunkLen) % winLen
            logicalOff += 1
            readSz  = 1 // read single frames in successive round (and rotate buffer)

            progBlock = (progBlock + 1) % 128
            if( progBlock == 0 ) progress( left.toFloat / afLen )
         }
         progress( 1f )

      } finally {
         afExtr.close()
      }

      val pay = prio.toIndexedSeq
      pay
   }
}