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

/**
 * A "smart" panel that supports to dialog feature.
 */
public class SmartPanel extends javax.swing.JPanel {
	private static final long serialVersionUID = 7578469200711029416L;
	private java.awt.Container oldParent = null;
	private javax.swing.JFrame frame = null;
	IvjEventHandler ivjEventHandler = new IvjEventHandler();
	private javax.swing.JMenuItem ivjJMenuItem1 = null;
	private javax.swing.JPopupMenu ivjJPopupMenu1 = null;
	private String shortName = null;
	private boolean isDialog = false;

	class IvjEventHandler implements java.awt.event.ActionListener,
			java.awt.event.MouseListener {
		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == SmartPanel.this.getJMenuItem1())
				connEtoC1(e);
		};

		public void mouseClicked(java.awt.event.MouseEvent e) {
			if (e.getSource() == SmartPanel.this)
				connEtoC2(e);
		};

		public void mouseEntered(java.awt.event.MouseEvent e) {
		};

		public void mouseExited(java.awt.event.MouseEvent e) {
		};

		public void mousePressed(java.awt.event.MouseEvent e) {
		};

		public void mouseReleased(java.awt.event.MouseEvent e) {
		};
	};

	/**
	 * SmartPanel constructor comment.
	 */
	public SmartPanel() {
		super();
		initialize();
	}

	/**
	 * SmartPanel constructor comment.
	 * @param layout java.awt.LayoutManager
	 */
	public SmartPanel(java.awt.LayoutManager layout) {
		super(layout);
		initialize();
	}

	/**
	 * SmartPanel constructor comment.
	 * @param layout java.awt.LayoutManager
	 * @param isDoubleBuffered boolean
	 */
	public SmartPanel(java.awt.LayoutManager layout, boolean isDoubleBuffered) {
		super(layout, isDoubleBuffered);
		initialize();
	}

	/**
	 * SmartPanel constructor comment.
	 * @param isDoubleBuffered boolean
	 */
	public SmartPanel(boolean isDoubleBuffered) {
		super(isDoubleBuffered);
		initialize();
	}

	/**
	 * connEtoC1:  (JMenuItem1.action.actionPerformed(java.awt.event.ActionEvent) --> SmartPanel.toDialog()V)
	 * @param arg1 java.awt.event.ActionEvent
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private void connEtoC1(java.awt.event.ActionEvent arg1) {
		try {
			// user code begin {1}
			// user code end
			this.toDialog();
			// user code begin {2}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {3}
			// user code end
			handleException(ivjExc);
		}
	}

	/**
	 * connEtoC2:  (SmartPanel.mouse.mouseClicked(java.awt.event.MouseEvent) --> SmartPanel.showPopup(Ljava.awt.event.MouseEvent;)V)
	 * @param arg1 java.awt.event.MouseEvent
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private void connEtoC2(java.awt.event.MouseEvent arg1) {
		try {
			// user code begin {1}
			// user code end
			this.showPopup(arg1);
			// user code begin {2}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {3}
			// user code end
			handleException(ivjExc);
		}
	}

	/**
	 * Return the JMenuItem1 property value.
	 * @return javax.swing.JMenuItem
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private javax.swing.JMenuItem getJMenuItem1() {
		if (ivjJMenuItem1 == null) {
			try {
				ivjJMenuItem1 = new javax.swing.JMenuItem();
				ivjJMenuItem1.setName("JMenuItem1");
				ivjJMenuItem1.setText("Expand to dialog");
				// user code begin {1}
				// user code end
			} catch (java.lang.Throwable ivjExc) {
				// user code begin {2}
				// user code end
				handleException(ivjExc);
			}
		}
		return ivjJMenuItem1;
	}

	/**
	 * Return the JPopupMenu1 property value.
	 * @return javax.swing.JPopupMenu
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private javax.swing.JPopupMenu getJPopupMenu1() {
		if (ivjJPopupMenu1 == null) {
			try {
				ivjJPopupMenu1 = new javax.swing.JPopupMenu();
				ivjJPopupMenu1.setName("JPopupMenu1");
				ivjJPopupMenu1.add(getJMenuItem1());
				// user code begin {1}
				// user code end
			} catch (java.lang.Throwable ivjExc) {
				// user code begin {2}
				// user code end
				handleException(ivjExc);
			}
		}
		return ivjJPopupMenu1;
	}

	/**
	 * Insert the method's description here.
	 * Creation date: (7.2.2002 18:52:53)
	 * @return java.lang.String
	 */
	public java.lang.String getName() {
		if (!isDialog)
			return getShortName();
		return super.getName();
	}

	/**
	 * Return the JPopupMenu1 property value.
	 * @return javax.swing.JPopupMenu
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	public javax.swing.JMenuItem[] getNewMenuItems() {
		javax.swing.JMenuItem item = getJMenuItem1();
		return new javax.swing.JMenuItem[] { item };
	}

	/**
	 * Insert the method's description here.
	 * Creation date: (7.2.2002 18:50:47)
	 * @return java.lang.String
	 */
	public java.lang.String getShortName() {
		if (shortName == null)
			return super.getName();
		return shortName;
	}

	/**
	 * Called whenever the part throws an exception.
	 * @param exception java.lang.Throwable
	 */
	private void handleException(java.lang.Throwable exception) {

		/* Uncomment the following lines to print uncaught exceptions to stdout */
		System.out.println("--------- UNCAUGHT EXCEPTION ---------");
		exception.printStackTrace(System.out);
	}

	/**
	 * Initializes connections
	 * @exception java.lang.Exception The exception description.
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private void initConnections() throws java.lang.Exception {
		// user code begin {1}
		// user code end
		getJMenuItem1().addActionListener(ivjEventHandler);
		this.addMouseListener(ivjEventHandler);
	}

	/**
	 * Initialize the class.
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private void initialize() {
		try {
			// user code begin {1}
			// user code end
			setName("SmartPanel");
			setLayout(null);
			setSize(160, 120);
			initConnections();
		} catch (java.lang.Throwable ivjExc) {
			handleException(ivjExc);
		}
		// user code begin {2}
		// user code end
	}

	/**
	 * Insert the method's description here.
	 * Creation date: (7.2.2002 18:50:47)
	 * @param newShortName java.lang.String
	 */
	public void setShortName(java.lang.String newShortName) {
		shortName = newShortName;
	}

	/**
	 * Comment
	 */
	public void showPopup(java.awt.event.MouseEvent mouseEvent) {
		if (java.awt.event.MouseEvent.META_MASK == mouseEvent.getModifiers()) {
			if (getJPopupMenu1().getComponents().length < 1)
				getJPopupMenu1().add(getJMenuItem1());
			getJPopupMenu1().show(this, mouseEvent.getX(), mouseEvent.getY());
		}
		return;
	}

	/**
	 * Comment
	 */
	public void toDialog() {
		if (oldParent == null) {
			isDialog = true;
			oldParent = getParent();
			if (oldParent instanceof javax.swing.JTabbedPane) {
				oldParent = null;
				isDialog = false;
				return;
				/*			((javax.swing.JTabbedPane)oldParent).setDoubleBuffered(false);
							((javax.swing.JTabbedPane)oldParent).removeTabAt(((javax.swing.JTabbedPane)oldParent).indexOfComponent(this));
							((javax.swing.JTabbedPane)oldParent).revalidate();
							((javax.swing.JTabbedPane)oldParent).repaint();
							this.doLayout();
							setVisible(true);
							setEnabled(true);
							setOpaque(true);
							setDoubleBuffered(false);
							
							setRequestFocusEnabled(false);
							setSize(200,200);*/
			} else {
				getParent().remove(this);
			}

			frame = new javax.swing.JFrame(getName());
			frame.setSize(getWidth(), getHeight());
			frame.setContentPane(this);
			frame.setDefaultCloseOperation(javax.swing.JFrame.DISPOSE_ON_CLOSE);
			frame.addWindowListener(new java.awt.event.WindowListener() {
				public void windowClosing(java.awt.event.WindowEvent event) {
					toDialog();
				};

				public void windowDeiconified(java.awt.event.WindowEvent e) {
				};

				public void windowIconified(java.awt.event.WindowEvent e) {
				};

				public void windowClosed(java.awt.event.WindowEvent e) {
				};

				public void windowOpened(java.awt.event.WindowEvent e) {
				};

				public void windowDeactivated(java.awt.event.WindowEvent e) {
				};

				public void windowActivated(java.awt.event.WindowEvent e) {
				};
			});
			frame.setVisible(true);
			getJMenuItem1().setText("Dock");
		} else {
			isDialog = false;
			getParent().remove(this);
			frame.dispose();
			oldParent.add(this);
			oldParent = null;
			getJMenuItem1().setText("Expand to dialog");
		}
		return;
	}
}
