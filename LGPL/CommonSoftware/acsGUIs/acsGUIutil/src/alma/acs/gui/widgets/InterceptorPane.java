/*
 Created on April 25, 2014 by msekoranja

 ALMA - Atacama Large Millimiter Array
 (c) European Southern Observatory, 2011

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */
package alma.acs.gui.widgets;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.Window;

/**
 * A JComponent based Pane that intercepts mouse clicks and only makes owner visible.
 */
public final class InterceptorPane extends javax.swing.JComponent {

	private static final long serialVersionUID = -5456601850968802653L;

	protected Window window;

	protected final class ClickListener extends MouseAdapter {

		private final InterceptorPane getOwner() {
			return InterceptorPane.this;
		}

		private final void shouldIgnoreEvent(MouseEvent e) {
			e.consume();
			getOwner().setVisible(false);

		}

		public final void mouseClicked(MouseEvent e) {
			shouldIgnoreEvent(e);
		}

		public final void mousePressed(MouseEvent e) {
			shouldIgnoreEvent(e);
		}

		public final void mouseReleased(MouseEvent e) {
			shouldIgnoreEvent(e);
		}

	}

	private final class FocusMonitor extends WindowAdapter {

		public final void windowDeactivated(WindowEvent e) {
			if (window == null)
				return;
			InterceptorPane owner = InterceptorPane.this;

			owner.setSize(owner.window.getSize());
			owner.setVisible(true);
		}
	}

	public InterceptorPane(Window window) {
		super();
		initialize(window);

	}

	private void initialize(Window ownerWindow) {
		window = ownerWindow;

		if (window == null) {
			System.out
					.println("InterceptorPane error: cannot create interceptor pane for window==null");
			return;
		}

		addMouseListener(new ClickListener());
		window.addWindowListener(new FocusMonitor());

	}
}
