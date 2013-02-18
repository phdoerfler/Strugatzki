/*
 *  ProcessorImpl.scala
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

import scala.util.control.NonFatal
import concurrent.{ExecutionContext, Future, Promise, future}
import scala.util.Try

trait ProcessorImpl[_PayLoad, Config] extends Processor[_PayLoad, Config] with FutureProxy[_PayLoad] {
  /**
   * The processor's companion object
   */
  protected val companion: ProcessorCompanion { type PayLoad = _PayLoad }

  protected def executionContext: ExecutionContext

  @volatile private var _aborted = false
  private var lastProg = -1

  // ---- constructor ----
  {
    implicit val exec = executionContext
    val res = future { body() }
    res.onComplete(t => dispatch(companion.Result(t)))
    promise.completeWith(res)
  }

  /**
   * A promise provided by the peer processor, which is used to implement the future interface
   */
  protected def promise: Promise[_PayLoad]

  final protected def peerFuture: Future[_PayLoad] = promise.future

  /**
   * Subclasses may override this to be informed about an abort request
   */
  protected def notifyAborted() {}

  final def abort() {
    _aborted = true
    notifyAborted()
  }

  /**
   * The processor's observer
   */
  protected def observer: companion.Observer

  /**
   * The main processing body
   */
  protected def body(): _PayLoad

  /**
   * Subclasses may override this to perform further cleanup when the process is aborted.
   */
  protected def cleanUp() {}

  /**
   * Checks if the process was aborted. If so, throws an `Aborted` exception.
   */
  protected final def checkAborted() {
    if (_aborted) throw Processor.Aborted()
  }

  private def dispatch(u: companion.Update) {
    try {
      if (observer.isDefinedAt(u)) observer(u)
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }
  }

  /**
   * Invoke this to signalize progress
   *
   * @param f the processor's progress in percent (0 to 1)
   */
  protected final def progress(f: Float) {
    val i = (f * 100 + 0.5f).toInt
    if (i > lastProg) {
      lastProg = i
      dispatch(companion.Progress(i))
    }
  }
}