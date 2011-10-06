/*
 *  ProcessorCompanion.scala
 *  (Strugatzki)
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

package de.sciss.strugatzki
package aux

trait ProcessorCompanion {
   // --- abstract stuff ---
   type PayLoad

   // --- concrete stuff ---

   var verbose = false

   type Observer = PartialFunction[ ProgressOrResult, Unit ]

   sealed trait ProgressOrResult
   final case class Progress( percent: Int ) extends ProgressOrResult
   sealed trait Result extends ProgressOrResult
   case class Success( result: PayLoad ) extends Result
   final case class Failure( t: Throwable ) extends Result
   case object Aborted extends Result
}