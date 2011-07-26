package de.sciss.utopia.view

import java.io.File
import javax.swing.{WindowConstants, JFrame, SwingUtilities}
import de.sciss.sonogram.{SimpleSonogramOverviewManager, SimpleSonogramView}
import java.awt.{BorderLayout, Dimension, FileDialog}
import de.sciss.gui.Axis
import de.sciss.synth.swing.ServerStatusPanel
import swing.{FlowPanel, ProgressBar, Component, Button, BorderPanel, Frame, Panel}

object FeatureCorrelationPane {
   def makeWindow() = new Frame {
      title       = "Feature Correlation"
      resizable   = false
      contents    = new FeatureCorrelationPane
      pack().centerOnScreen()
      peer.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE ) // sucky scala swing doesn't provide this?!
      open()
   }
}

class FeatureCorrelationPane extends BorderPanel {
   panel =>

   import BorderPanel.Position._

   private def findFrame : JFrame = SwingUtilities.getWindowAncestor( peer ) match {
      case f: JFrame => f
      case _ => null
   }

   private val butOpenInput = NiceButton( "Open input..." ) { b =>
      val fDlg = new FileDialog( findFrame, b.text )
      fDlg.setVisible( true )
      val fileName   = fDlg.getFile
      val dirName    = fDlg.getDirectory
      if( fileName != null && dirName != null ) {
         openInput( new File( dirName, fileName ))
      }
   }

   private val sonoView = new SonoView {
      preferredSize = new Dimension( 400, 128 )
      boost = 8f
   }

   private val sonoAxis = new Axis {
      format = Axis.Format.Time()
      preferredSize = { val d = preferredSize; d.width = 960; d }
      minimum= 0.0
   }

   private val serverPanel = new ServerStatusPanel( ServerStatusPanel.COUNTS )

   private val topPane = new FlowPanel {
      contents += butOpenInput
      contents += Component.wrap( serverPanel )
   }

   add( topPane, North )

   private val centerPane = new BorderPanel {
      add( sonoView, Center )
      add( sonoAxis, North )
   }

   private val ggProgress = new ProgressBar

   add( centerPane, Center )
   add( ggProgress, South )

   private def openInput( f: File ) {
      val mgr     = new SimpleSonogramOverviewManager
      val ov      = mgr.fromPath( f )
//      val view    = new SimpleSonogramView
//      view.boost  = 4f
//      view.sono   = Some( ov )
      val dur     = ov.fileSpec.numFrames / ov.fileSpec.sampleRate
      sonoAxis.maximum = dur
      sonoView.sono = Some( ov )
   }

//   private class InputPane extends Component {
//      preferredSize = new Dimension( 400, 200 )
//      peer.setLayout( new BorderLayout )
//
//      private var sonoView = Option.empty[ Sonogram]
//   }
}