/*
 *  NiceGUI.scala
 *  (Utopia)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.utopia.view

import swing.event.ButtonClicked
import swing.{ToggleButton, TextField, ComboBox, Label, Button}

case class NiceButton( label: String )( reaction: Button => Unit ) extends Button( label ) {
   peer.putClientProperty( "JButton.buttonType", "bevel" )
   peer.putClientProperty( "JComponent.sizeVariant", "small" )
   focusable = false
   listenTo( this )
   reactions += {
      case ButtonClicked( _ ) => reaction( this )
   }
}

case class NiceToggle( label: String )( reaction: ToggleButton => Unit ) extends ToggleButton( label ) {
   peer.putClientProperty( "JButton.buttonType", "bevel" )
   peer.putClientProperty( "JComponent.sizeVariant", "small" )
   focusable = false
   listenTo( this )
   reactions += {
      case ButtonClicked( _ ) => reaction( this )
   }
}

case class NiceLabel( label: String ) extends Label {
   text = label
   peer.putClientProperty( "JComponent.sizeVariant", "small" )
}

case class NiceTextField( initial: String )( columns: Int = initial.length ) extends TextField( initial, columns ) {
   peer.putClientProperty( "JComponent.sizeVariant", "small" )
}

case class NiceCombo[ A ]( items: Seq[ A ], default: A ) extends ComboBox[ A ]( items ) {
   peer.putClientProperty( "JComponent.sizeVariant", "small" )
   peer.putClientProperty( "JComboBox.isSquare", java.lang.Boolean.TRUE )
   focusable = false
   selection.item = default
}