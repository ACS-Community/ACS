/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package dartboard;

/**
 * Insert the type's description here.
 * Creation date: (11/12/00 3:08:23 PM)
 * @author: Administrator
 */
public class PlanetPanel extends javax.swing.JPanel {
	private javax.swing.JLabel ivjJLabel1 = null;
	private javax.swing.JLabel ivjJLabel2 = null;
	private javax.swing.JLabel ivjJLabel3 = null;
	private javax.swing.JLabel ivjJLabel4 = null;
	private javax.swing.JLabel ivjJLabel7 = null;
	private javax.swing.JLabel ivjJLabel8 = null;
	private javax.swing.JLabel ivjMoonDeclinationLabel = null;
	private javax.swing.JLabel ivjMoonRightAscensionLabel = null;
	private javax.swing.JLabel ivjSunDeclinationLabel = null;
	private javax.swing.JLabel ivjSunRightAscensionLabel = null;
/**
 * PlanetPanel constructor comment.
 */
public PlanetPanel() {
	super();
	initialize();
}
/**
 * PlanetPanel constructor comment.
 * @param layout java.awt.LayoutManager
 */
public PlanetPanel(java.awt.LayoutManager layout) {
	super(layout);
}
/**
 * PlanetPanel constructor comment.
 * @param layout java.awt.LayoutManager
 * @param isDoubleBuffered boolean
 */
public PlanetPanel(java.awt.LayoutManager layout, boolean isDoubleBuffered) {
	super(layout, isDoubleBuffered);
}
/**
 * PlanetPanel constructor comment.
 * @param isDoubleBuffered boolean
 */
public PlanetPanel(boolean isDoubleBuffered) {
	super(isDoubleBuffered);
}
/**
 * Return the JLabel1 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel1() {
	if (ivjJLabel1 == null) {
		try {
			ivjJLabel1 = new javax.swing.JLabel();
			ivjJLabel1.setName("JLabel1");
			ivjJLabel1.setFont(new java.awt.Font("dialog", 1, 14));
			ivjJLabel1.setText("Sun");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel1;
}
/**
 * Return the JLabel2 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel2() {
	if (ivjJLabel2 == null) {
		try {
			ivjJLabel2 = new javax.swing.JLabel();
			ivjJLabel2.setName("JLabel2");
			ivjJLabel2.setFont(new java.awt.Font("dialog", 1, 14));
			ivjJLabel2.setText("Moon");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel2;
}
/**
 * Return the JLabel3 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel3() {
	if (ivjJLabel3 == null) {
		try {
			ivjJLabel3 = new javax.swing.JLabel();
			ivjJLabel3.setName("JLabel3");
			ivjJLabel3.setText("RA:");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel3;
}
/**
 * Return the JLabel4 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel4() {
	if (ivjJLabel4 == null) {
		try {
			ivjJLabel4 = new javax.swing.JLabel();
			ivjJLabel4.setName("JLabel4");
			ivjJLabel4.setText("RA:");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel4;
}
/**
 * Return the JLabel7 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel7() {
	if (ivjJLabel7 == null) {
		try {
			ivjJLabel7 = new javax.swing.JLabel();
			ivjJLabel7.setName("JLabel7");
			ivjJLabel7.setText("DEC:");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel7;
}
/**
 * Return the JLabel8 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel8() {
	if (ivjJLabel8 == null) {
		try {
			ivjJLabel8 = new javax.swing.JLabel();
			ivjJLabel8.setName("JLabel8");
			ivjJLabel8.setText("DEC:");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel8;
}
/**
 * Return the MoonDeclinationLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
public javax.swing.JLabel getMoonDeclinationLabel() {
	if (ivjMoonDeclinationLabel == null) {
		try {
			ivjMoonDeclinationLabel = new javax.swing.JLabel();
			ivjMoonDeclinationLabel.setName("MoonDeclinationLabel");
			ivjMoonDeclinationLabel.setText("<moon DEC>");
			ivjMoonDeclinationLabel.setForeground(java.awt.Color.black);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjMoonDeclinationLabel;
}
/**
 * Return the MoonRightAscensionLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
public javax.swing.JLabel getMoonRightAscensionLabel() {
	if (ivjMoonRightAscensionLabel == null) {
		try {
			ivjMoonRightAscensionLabel = new javax.swing.JLabel();
			ivjMoonRightAscensionLabel.setName("MoonRightAscensionLabel");
			ivjMoonRightAscensionLabel.setText("<moon RA>");
			ivjMoonRightAscensionLabel.setForeground(java.awt.Color.black);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjMoonRightAscensionLabel;
}
/**
 * Return the SunDeclinationLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
public javax.swing.JLabel getSunDeclinationLabel() {
	if (ivjSunDeclinationLabel == null) {
		try {
			ivjSunDeclinationLabel = new javax.swing.JLabel();
			ivjSunDeclinationLabel.setName("SunDeclinationLabel");
			ivjSunDeclinationLabel.setText("<sun DEC>");
			ivjSunDeclinationLabel.setForeground(java.awt.Color.black);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjSunDeclinationLabel;
}
/**
 * Return the SunRightAscensionLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
public javax.swing.JLabel getSunRightAscensionLabel() {
	if (ivjSunRightAscensionLabel == null) {
		try {
			ivjSunRightAscensionLabel = new javax.swing.JLabel();
			ivjSunRightAscensionLabel.setName("SunRightAscensionLabel");
			ivjSunRightAscensionLabel.setText("<sun RA>");
			ivjSunRightAscensionLabel.setForeground(java.awt.Color.black);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjSunRightAscensionLabel;
}
/**
 * Called whenever the part throws an exception.
 * @param exception java.lang.Throwable
 */
private void handleException(Throwable exception) {

	/* Uncomment the following lines to print uncaught exceptions to stdout */
	// System.out.println("--------- UNCAUGHT EXCEPTION ---------");
	// exception.printStackTrace(System.out);
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		// user code end
		setName("PlanetPanel");
		setLayout(new java.awt.GridBagLayout());
		setSize(379, 127);

		java.awt.GridBagConstraints constraintsJLabel1 = new java.awt.GridBagConstraints();
		constraintsJLabel1.gridx = 0; constraintsJLabel1.gridy = 0;
		constraintsJLabel1.weightx = 1.0;
		constraintsJLabel1.weighty = 1.0;
		constraintsJLabel1.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getJLabel1(), constraintsJLabel1);

		java.awt.GridBagConstraints constraintsJLabel2 = new java.awt.GridBagConstraints();
		constraintsJLabel2.gridx = 0; constraintsJLabel2.gridy = 1;
		constraintsJLabel2.weightx = 1.0;
		constraintsJLabel2.weighty = 1.0;
		constraintsJLabel2.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getJLabel2(), constraintsJLabel2);

		java.awt.GridBagConstraints constraintsJLabel3 = new java.awt.GridBagConstraints();
		constraintsJLabel3.gridx = 1; constraintsJLabel3.gridy = 0;
		constraintsJLabel3.anchor = java.awt.GridBagConstraints.EAST;
		constraintsJLabel3.weightx = 1.0;
		constraintsJLabel3.weighty = 1.0;
		constraintsJLabel3.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getJLabel3(), constraintsJLabel3);

		java.awt.GridBagConstraints constraintsJLabel4 = new java.awt.GridBagConstraints();
		constraintsJLabel4.gridx = 1; constraintsJLabel4.gridy = 1;
		constraintsJLabel4.anchor = java.awt.GridBagConstraints.EAST;
		constraintsJLabel4.weightx = 1.0;
		constraintsJLabel4.weighty = 1.0;
		constraintsJLabel4.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getJLabel4(), constraintsJLabel4);

		java.awt.GridBagConstraints constraintsSunRightAscensionLabel = new java.awt.GridBagConstraints();
		constraintsSunRightAscensionLabel.gridx = 2; constraintsSunRightAscensionLabel.gridy = 0;
		constraintsSunRightAscensionLabel.fill = java.awt.GridBagConstraints.HORIZONTAL;
		constraintsSunRightAscensionLabel.anchor = java.awt.GridBagConstraints.WEST;
		constraintsSunRightAscensionLabel.weightx = 1.0;
		constraintsSunRightAscensionLabel.weighty = 1.0;
		constraintsSunRightAscensionLabel.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getSunRightAscensionLabel(), constraintsSunRightAscensionLabel);

		java.awt.GridBagConstraints constraintsMoonRightAscensionLabel = new java.awt.GridBagConstraints();
		constraintsMoonRightAscensionLabel.gridx = 2; constraintsMoonRightAscensionLabel.gridy = 1;
		constraintsMoonRightAscensionLabel.fill = java.awt.GridBagConstraints.HORIZONTAL;
		constraintsMoonRightAscensionLabel.anchor = java.awt.GridBagConstraints.WEST;
		constraintsMoonRightAscensionLabel.weightx = 1.0;
		constraintsMoonRightAscensionLabel.weighty = 1.0;
		constraintsMoonRightAscensionLabel.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getMoonRightAscensionLabel(), constraintsMoonRightAscensionLabel);

		java.awt.GridBagConstraints constraintsJLabel7 = new java.awt.GridBagConstraints();
		constraintsJLabel7.gridx = 3; constraintsJLabel7.gridy = 0;
		constraintsJLabel7.anchor = java.awt.GridBagConstraints.EAST;
		constraintsJLabel7.weightx = 1.0;
		constraintsJLabel7.weighty = 1.0;
		constraintsJLabel7.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getJLabel7(), constraintsJLabel7);

		java.awt.GridBagConstraints constraintsJLabel8 = new java.awt.GridBagConstraints();
		constraintsJLabel8.gridx = 3; constraintsJLabel8.gridy = 1;
		constraintsJLabel8.anchor = java.awt.GridBagConstraints.EAST;
		constraintsJLabel8.weightx = 1.0;
		constraintsJLabel8.weighty = 1.0;
		constraintsJLabel8.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getJLabel8(), constraintsJLabel8);

		java.awt.GridBagConstraints constraintsSunDeclinationLabel = new java.awt.GridBagConstraints();
		constraintsSunDeclinationLabel.gridx = 4; constraintsSunDeclinationLabel.gridy = 0;
		constraintsSunDeclinationLabel.fill = java.awt.GridBagConstraints.HORIZONTAL;
		constraintsSunDeclinationLabel.anchor = java.awt.GridBagConstraints.WEST;
		constraintsSunDeclinationLabel.weightx = 1.0;
		constraintsSunDeclinationLabel.weighty = 1.0;
		constraintsSunDeclinationLabel.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getSunDeclinationLabel(), constraintsSunDeclinationLabel);

		java.awt.GridBagConstraints constraintsMoonDeclinationLabel = new java.awt.GridBagConstraints();
		constraintsMoonDeclinationLabel.gridx = 4; constraintsMoonDeclinationLabel.gridy = 1;
		constraintsMoonDeclinationLabel.fill = java.awt.GridBagConstraints.HORIZONTAL;
		constraintsMoonDeclinationLabel.anchor = java.awt.GridBagConstraints.WEST;
		constraintsMoonDeclinationLabel.weightx = 1.0;
		constraintsMoonDeclinationLabel.weighty = 1.0;
		constraintsMoonDeclinationLabel.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getMoonDeclinationLabel(), constraintsMoonDeclinationLabel);
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	}
	// user code begin {2}
	// user code end
}
/**
 * main entrypoint - starts the part when it is run as an application
 * @param args java.lang.String[]
 */
public static void main(java.lang.String[] args) {
	try {
		javax.swing.JFrame frame = new javax.swing.JFrame();
		PlanetPanel aPlanetPanel;
		aPlanetPanel = new PlanetPanel();
		frame.setContentPane(aPlanetPanel);
		frame.setSize(aPlanetPanel.getSize());
		frame.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				System.exit(0);
			};
		});
		frame.setVisible(true);
	} catch (Throwable exception) {
		System.err.println("Exception occurred in main() of javax.swing.JPanel");
		exception.printStackTrace(System.out);
	}
}
}
