package de.sciss.utopia

case class Span( start: Long, stop: Long ) {
   def length: Long = stop - start
}