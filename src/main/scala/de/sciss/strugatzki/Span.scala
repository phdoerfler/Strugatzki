//package de.sciss.strugatzki
//
//case class Span(start: Long, stop: Long) {
//  def length: Long = stop - start
//
//  // where overlapping results in negative spacing
//  def spacing(b: Span): Long = {
//    val bStart = b.start
//    if (start < bStart) {
//      bStart - stop
//    } else {
//      start - b.stop
//    }
//  }
//}