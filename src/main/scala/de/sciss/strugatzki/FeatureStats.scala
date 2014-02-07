/*
 *  FeatureStats.scala
 *  (Strugatzki)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.strugatzki

import java.io.File
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.processor.{Processor, ProcessorFactory}

object FeatureStats extends ProcessorFactory.WithDefaults {
  type Product  = IIdxSeq[(Double, Double)]
  type Config   = IIdxSeq[File]
  type Repr     = FeatureStats

  protected def defaultConfig: IIdxSeq[File] = Vector.empty

  protected def prepare(config: FeatureStats.Config): Prepared =
    new impl.FeatureStatsImpl(config)
}
trait FeatureStats extends Processor[FeatureStats.Product, FeatureStats] {
  def config: FeatureStats.Config
}