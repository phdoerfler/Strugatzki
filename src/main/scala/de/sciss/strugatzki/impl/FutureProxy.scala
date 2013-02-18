package de.sciss.strugatzki.impl

import concurrent.{CanAwait, ExecutionContext, Future}
import util.Try
import concurrent.duration.Duration

trait FutureProxy[A] extends Future[A] {
  protected def peerFuture: Future[A]

  def value: Option[Try[A]] = peerFuture.value
  def isCompleted: Boolean  = peerFuture.isCompleted

  def onComplete[U](func: (Try[A]) => U)(implicit executor: ExecutionContext) {
    peerFuture.onComplete(func)
  }

  def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    peerFuture.ready(atMost)
    this
  }

  def result(atMost: Duration)(implicit permit: CanAwait): A = peerFuture.result(atMost)
}