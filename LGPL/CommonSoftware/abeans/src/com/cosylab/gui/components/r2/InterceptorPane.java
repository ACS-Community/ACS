package com.cosylab.gui.components.r2;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.Window;
/**
 * Insert the type's description here.
 * Creation date: (4/8/2002 11:51:40)
 * @author: 
 */
public final class InterceptorPane extends javax.swing.JComponent {

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
/**
 * Insert the method's description here.
 * Creation date: (10.4.2002 15:15:26)
 * @param window java.awt.Window
 */
public InterceptorPane(Window window) {
	super();
	initialize(window);
	
}
/**
 * Insert the method's description here.
 * Creation date: (10.4.2002 15:16:34)
 * @param ownerWindow java.awt.Window
 */
private void initialize(Window ownerWindow) {
	window = ownerWindow;

	if (window == null) {
		System.out.println("InterceptorPane error: cannot create interceptor pane for window==null");
		return;
	}
	
	addMouseListener(new ClickListener());
	window.addWindowListener(new FocusMonitor());
		
}
}
