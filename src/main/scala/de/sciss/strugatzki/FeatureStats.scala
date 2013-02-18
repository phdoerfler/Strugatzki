///*
// *  FeatureStats.scala
// *  (Strugatzki)
// *
// *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is free software; you can redistribute it and/or
// *  modify it under the terms of the GNU General Public License
// *  as published by the Free Software Foundation; either
// *  version 2, june 1991 of the License, or (at your option) any later version.
// *
// *  This software is distributed in the hope that it will be useful,
// *  but WITHOUT ANY WARRANTY; without even the implied warranty of
// *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// *  General Public License for more details.
// *
// *  You should have received a copy of the GNU General Public
// *  License (gpl.txt) along with this software; if not, write to the Free Software
// *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.strugatzki
//
//import de.sciss.synth.io.AudioFile
//import java.io.File
//import actors.Actor
//
//object FeatureStats extends ProcessorCompanion {
//   private val log05 = math.log( 0.5 )
//
//   def apply( paths: IndexedSeq[ File ])( observer: PartialFunction[ ProgressOrResult, Unit ]) : FeatureStats = {
//      new FeatureStats( paths, observer )
//   }
//
//   type PayLoad = IndexedSeq[ (Double, Double) ]
//}
//final class FeatureStats private ( paths: IndexedSeq[ File ],
//                                   observer: PartialFunction[ FeatureStats.ProgressOrResult, Unit ]) {
//   import FeatureStats._
//
////   Act.start()
//
//   def abort() { Act ! Abort }
//   def start() { Act.start() }
//
//   private object Abort
//
//   private object Act extends Actor {
//      def act() {
//         ProcT.start()
//         var result : Result = null
//         loopWhile( result == null ) {
//            react {
//               case Abort =>
//                  ProcT.aborted = true
//               case res @ Progress( _ ) =>
//                  observer( res )
//               case res @ Aborted =>
//                  result = res
//               case res @ Failure( _ ) =>
//                  result = res
//               case res @ Success( _ ) =>
//                  result = res
//            }
//         } andThen { observer( result )}
//      }
//   }
//
//   private object ProcT extends Thread {
//      @volatile var aborted: Boolean = false
//      override def run() {
//         var allMins: Array[ Double ] = null
//         var allMaxs: Array[ Double ] = null
//         Act ! (try {
//            var i = 0; while( i < paths.size && !aborted ) {
//               val path = paths( i )
//               val (mins, maxs) = body( path )
//               if( i == 0 ) {
//                  require( mins.size == maxs.size )
//                  allMins  = mins
//                  allMaxs  = maxs
//               } else {
//                  val numCh = allMins.size
//                  require( mins.size == numCh && maxs.size == numCh )
//                  for( ch <- 0 until numCh ) {
//                     allMins( ch ) = math.min( allMins( ch ), mins( ch ))
//                     allMaxs( ch ) = math.max( allMaxs( ch ), maxs( ch ))
//                  }
//               }
//               val prog = ((i + 1).toFloat / paths.size * 100).toInt
//               Act ! Progress( prog )
//            i += 1 }
//            if( aborted ) Aborted else Success( allMins zip allMaxs )
//         } catch {
//            case e: Throwable => Failure( e )
//         })
//      }
//
//      def body( path: File ) : (Array[ Double], Array[ Double ]) = {
//         val af      = AudioFile.openRead( path )
//         try {
//            val bufSz   = math.min( 8192, af.numFrames ).toInt
//            val numCh   = af.numChannels
//            val maxs    = Array.fill( numCh )( Float.NegativeInfinity )
//            val mins    = Array.fill( numCh )( Float.PositiveInfinity )
//            val sums    = new Array[ Double ]( numCh )
//            val skews   = new Array[ Double ]( numCh )
//            val p01     = new Array[ Double ]( numCh )
//            val p99     = new Array[ Double ]( numCh )
//            val b       = af.buffer( bufSz )
//            var left    = af.numFrames
//            val chans   = 0 until numCh
//            while( left > 0 ) {
//               val chunkLen = math.min( left, bufSz ).toInt
//               af.read( b, 0, chunkLen )
//               for( ch <- chans ) {
//                  val cb = b( ch )
//                  for( i <- 0 until chunkLen ) {
//                     val f = cb( i )
//                     if( f < mins( ch )) mins( ch ) = f
//                     if( f > maxs( ch )) maxs( ch ) = f
//                     sums( ch ) += f
//                  }
//               }
//               left -= chunkLen
//            }
//            for( ch <- chans ) {
//               val mean    = sums( ch ) / af.numFrames
//               val d       = maxs( ch ) - mins( ch )
//               val mn      = (mean - mins( ch )) / d
//               skews( ch ) = log05 / math.log( mn )
//            }
//
//            // second pass
//            val pctils  = Array.ofDim[ Int ]( numCh, 2048 )
//            af.seek( 0L )
//            left = af.numFrames
//            while( left > 0 ) {
//               val chunkLen = math.min( left, bufSz ).toInt
//               af.read( b, 0, chunkLen )
//               for( ch <- chans ) {
//                  val cb   = b( ch )
//                  val cp   = pctils( ch )
//                  val min  = mins( ch )
//                  val d    = maxs( ch ) - min
//                  val skew = skews( ch )
//                  for( i <- 0 until chunkLen ) {
//                     val f    = cb( i )
//                     val norm = (math.pow( (f - min) / d, skew ) * 2047 + 0.5).toInt
//                     cp( norm ) += 1
//                  }
//               }
//               left -= chunkLen
//            }
//            af.close()
//
//            for( ch <- chans ) {
//               val cp = pctils( ch )
//               val p01n = (af.numFrames * 0.01).toInt
//               val p99n = (af.numFrames * 0.99).toInt
//               val skewr = 1.0 / skews( ch )
//               val min  = mins( ch )
//               val d    = maxs( ch ) - min
//               var cnt = 0; var i = 0; while( cnt < p01n ) {
//                  cnt += cp( i )
//                  i += 1
//               }
//               p01( ch ) = math.pow( i.toDouble / 2048, skewr ) * d + min
//               while( cnt < p99n ) {
//                  cnt += cp( i )
//                  i += 1
//               }
//               p99( ch ) = math.pow( i.toDouble / 2048, skewr ) * d + min
//
//   //            println("i = "+i+"; min = " )
//            }
//
//            (p01, p99)
//
//         } finally {
//            if( af.isOpen ) af.close()
//         }
//      }
//   }
//}