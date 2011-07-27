//package de.sciss.utopia.view
//
//import swing.Component
//import java.awt.{Color, Graphics2D}
//import java.awt.image.ImageObserver
//import de.sciss.sonogram.{OverviewComplete, SonogramPaintController, SonogramOverview}
//
//class SonoView
//extends Component with SonogramPaintController {
//   private var sonoO: Option[ SonogramOverview ] = None
//   private var boostVar: Float = 1f
//
//   override def paintComponent( g2: Graphics2D ) {
//      super.paintComponent( g2 )
////      val g2   = g.asInstanceOf[ Graphics2D ]
//      val i    = peer.getInsets
//      val x    = i.left
//      val y    = i.top
//      val w    = peer.getWidth - (i.left + i.right)
//      val h    = peer.getHeight - (i.top + i.bottom)
//      g2.setColor( Color.gray )
//      g2.fillRect( x, y, w, h )
//      g2.setColor( Color.white )
//      g2.drawString( "Calculating...", 8, 20 )
//      sonoO.foreach( sono => {
//         sono.paint( 0L, sono.fileSpec.numFrames, g2, x, y, w, h, this )
//      })
//   }
//
//   def sono = sonoO
//   def sono_=( newSono: Option[ SonogramOverview ]) {
//      sonoO.foreach( _.removeListener( listener ))
//      sonoO = newSono
//      sonoO.foreach( _.addListener( listener ))
//      repaint()
//   }
//
//   def boost = boostVar
//   def boost_=( newBoost: Float ) {
//      boostVar = newBoost
//      repaint()
//   }
//
//   // ---- SonogramPaintController ----
//   def adjustGain( amp: Float, pos: Double ) = amp * boostVar
//   def imageObserver: ImageObserver = peer
//
//   // ---- SonogramOverviewListener ----
//
//   private val listener = (msg: AnyRef) => msg match {
//      case OverviewComplete( ov ) => repaint()
//      case _ =>
//   }
//}