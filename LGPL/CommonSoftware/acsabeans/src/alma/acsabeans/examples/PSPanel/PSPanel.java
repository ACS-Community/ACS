/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2002
 * Copyright by ESO (in the framework of the ALMA collaboration)
 * and Cosylab 2002, All rights reserved
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package alma.acsabeans.examples.PSPanel;

import java.awt.GridLayout;

import javax.swing.JApplet;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JPanel;

import alma.PS.abeans.PowerSupply;

import com.cosylab.abeans.AbeansLaunchable;
import com.cosylab.abeans.SingleAbeanSelector;
import com.cosylab.abeans.displayers.Gauger;
import com.cosylab.abeans.displayers.Ledder;
import com.cosylab.abeans.displayers.Slider;
import com.cosylab.abeans.vep.VisualACSAbeansApplication;
import com.cosylab.gui.framework.Desktop;
import com.cosylab.gui.framework.Launcher;
import com.cosylab.gui.framework.LauncherEnvironment;

/**
 * Example demonstrating PowerSupply GUI.
 *
 * Run with arguments (replace environment variables to reflect your local
environment):
 * 	-Dabeans.home=$ACSDATA/config/abeans/Config
-DManager.defaultReference=corbaloc::$HOST:3000/Manager
 *
 * <b>NOTE</b>: Manager port should be obtained using <i>getManagerPort()</i>
function
 * defined in <i>acsstartupAcsPorts<i> script.<br/>
 *
 * It can be also obtained dinamically by
<code>alma.acs.util.ACSPorts.getManagerPort()</code> method,
 * which requires <i>-DACS.baseport=$ACS_INSTANCE</i> to be passed as JVM
parameter.<br/>
 *
 * All configuration management is automatically done by <i>acsStartJava</i> script,
 * so it is <b>strongly recommended</b> to run your applications (in production env.)
using<br/>
 * <pre>acsStartJava full.class.name.of.your.java.application</pre>.
 *
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		$Id: PSPanel.java,v 1.2 2005/11/07 09:49:34 msekoran Exp $
 */
public class PSPanel extends VisualACSAbeansApplication {

	private SingleAbeanSelector jSingleAbeanSelector = null;
	private Gauger jGauger = null;
	private Slider jSlider = null;
	private Ledder jLedder = null;
	private JPanel jPanel = null;
	private JButton onButton = null;
	private JButton offButton = null;
	private JButton resetButton = null;
	private PowerSupply powerSupply = null;  //  @jve:decl-index=0:visual-constraint="28,107"

	/**
	 * Creates a new instance of this class. The default no-arg constructor
	 * may be used only by visual builders to instantiate an instance of
	 * launchable panel. During run-time spcific constructor must be used.
	 * @see com.cosylab.abeans.AbeansLaunchable
	 */	
	public PSPanel() {
		super();
		initialize();
	}

	/**
	 * Creates a new instance of this class that will reside inside a <code>JInternalFrame</code>.
	 * @see com.cosylab.abeans.AbeansLaunchable
	 */
	public PSPanel(Launcher arg0, LauncherEnvironment arg1, Desktop arg2,
			JInternalFrame arg3) {
		super(arg0, arg1, arg2, arg3);
		initialize();
	}

	/**
	 * Creates a new instance of this class that will reside inside an applet in a web browser.
	 * @see com.cosylab.abeans.AbeansLaunchable
	 */
	public PSPanel(Launcher arg0, LauncherEnvironment arg1, JApplet arg2) {
		super(arg0, arg1, arg2);
		initialize();
	}

	/**
	 * Creates a new instance of this class that will reside in a <code>JFrame</code> container.
	 * @see com.cosylab.abeans.AbeansLaunchable
	 */
	public PSPanel(Launcher arg0, LauncherEnvironment arg1, JFrame arg2) {
		super(arg0, arg1, arg2);
		initialize();
	}

	/**
	 * This method initializes the PSPanel.
	 * 
	 */
	private void initialize() {
        GridLayout gridLayout1 = new GridLayout();
        this.setLayout(gridLayout1);
        this.setSize(420, 420);
        gridLayout1.setColumns(1);
        gridLayout1.setRows(0);
        this.add(getJSingleAbeanSelector(), null);
        this.add(getJGauger(), null);
        this.add(getJSlider(), null);
        this.add(getJLedder(), null);
        this.add(getJPanel(), null);
			
	}
	/**
	 * This method initializes jSingleAbeanSelector	
	 * 	
	 * @return com.cosylab.abeans.SingleAbeanSelector	
	 */    
	private SingleAbeanSelector getJSingleAbeanSelector() {
		if (jSingleAbeanSelector == null) {
			jSingleAbeanSelector = new SingleAbeanSelector();
			jSingleAbeanSelector.setAbean(getPowerSupply());
		}
		return jSingleAbeanSelector;
	}
	/**
	 * This method initializes jGauger	
	 * 	
	 * @return com.cosylab.abeans.displayers.Gauger	
	 */    
	private Gauger getJGauger() {
		if (jGauger == null) {
			jGauger = new Gauger();
			jGauger.setDoubleProperty(getPowerSupply().getReadback());		}
		return jGauger;
	}
	/**
	 * This method initializes jSlider	
	 * 	
	 * @return com.cosylab.abeans.displayers.Slider	
	 */    
	private Slider getJSlider() {
		if (jSlider == null) {
			jSlider = new Slider();
			jSlider.setDoubleProperty(getPowerSupply().getCurrent());		}
		return jSlider;
	}
	/**
	 * This method initializes jLedder	
	 * 	
	 * @return com.cosylab.abeans.displayers.Ledder	
	 */    
	private Ledder getJLedder() {
		if (jLedder == null) {
			jLedder = new Ledder();
			jLedder.setPatternProperty(getPowerSupply().getStatus());		}
		return jLedder;
	}
	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */    
	private JPanel getJPanel() {
		if (jPanel == null) {
			jPanel = new JPanel();
			jPanel.add(getOnButton(), null);
			jPanel.add(getOffButton(), null);
			jPanel.add(getResetButton(), null);
		}
		return jPanel;
	}
	/**
	 * This method initializes jButton	
	 * 	
	 * @return javax.swing.JButton	
	 */    
	private JButton getOnButton() {
		if (onButton == null) {
			onButton = new JButton();
			onButton.setText("ON");
			onButton.addActionListener(new java.awt.event.ActionListener() { 
				public void actionPerformed(java.awt.event.ActionEvent e) {    
					try {
						powerSupply.on();
					} catch (Exception ex) {
						ex.printStackTrace();
					}				}
			});
		}
		return onButton;
	}
	/**
	 * This method initializes jButton1	
	 * 	
	 * @return javax.swing.JButton	
	 */    
	private JButton getOffButton() {
		if (offButton == null) {
			offButton = new JButton();
			offButton.setText("OFF");
			offButton.addActionListener(new java.awt.event.ActionListener() { 
				public void actionPerformed(java.awt.event.ActionEvent e) {    
					try {
						powerSupply.off();
					} catch (Exception ex) {
						ex.printStackTrace();
					}				}
			});
		}
		return offButton;
	}
	/**
	 * This method initializes jButton2	
	 * 	
	 * @return javax.swing.JButton	
	 */    
	private JButton getResetButton() {
		if (resetButton == null) {
			resetButton = new JButton();
			resetButton.setText("RESET");
			resetButton.addActionListener(new java.awt.event.ActionListener() { 
				public void actionPerformed(java.awt.event.ActionEvent e) {    
					try {
						powerSupply.reset();
					} catch (Exception ex) {
						ex.printStackTrace();
					}				}
			});
		}
		return resetButton;
	}
	/**
	 * This method initializes powerSupply	
	 * 	
	 * @return alma.PS.abeans.PowerSupply	
	 */    
	private PowerSupply getPowerSupply() {
		if (powerSupply == null) {
			powerSupply = new PowerSupply();
			powerSupply.addLinkListener(new abeans.models.LinkListener() {   
				public void linkLost(abeans.models.LinkEvent e) {    
					onButton.setEnabled(false);
					offButton.setEnabled(false);
					resetButton.setEnabled(false);
				} 
				public void linkEstablished(abeans.models.LinkEvent e) {    
					onButton.setEnabled(true);
					offButton.setEnabled(true);
					resetButton.setEnabled(true);
				}
				public void linkableResumed(abeans.models.LinkEvent e) {} 
				public void linkableSuspended(abeans.models.LinkEvent e) {} 
			 
			});
		}
		return powerSupply;
	}

	public static void main(String[] args) {
		AbeansLaunchable.launch(PSPanel.class, args);
	}
}  //  @jve:decl-index=0:visual-constraint="92,11"
