/*
 *  ProcessorCompanion.scala
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

import scala.util.Try
import concurrent.{ExecutionContext, Promise}

trait ProcessorCompanion {
  companion =>

  // --- abstract stuff ---
  type Config
  type PayLoad

  // --- concrete stuff ---

  final def apply(config: Config = defaultConfig)(observer: Observer)
                 (implicit exec: ExecutionContext): Processor[PayLoad, Config] =
    create(config, observer, Promise[PayLoad]())

  protected def defaultConfig: Config

  protected def create(config: Config, observer: Observer, promise: Promise[PayLoad])
                      (implicit exec: ExecutionContext): Processor[PayLoad, Config]

  var verbose = false

  type Observer = PartialFunction[Update, Unit]

  sealed trait Update
  final case class Progress(percent: Int)       extends Update
  final case class Result(value: Try[PayLoad])  extends Update
}