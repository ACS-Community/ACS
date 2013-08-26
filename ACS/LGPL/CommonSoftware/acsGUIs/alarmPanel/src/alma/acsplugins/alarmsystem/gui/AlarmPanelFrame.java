/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2011, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acsplugins.alarmsystem.gui;

import java.awt.BorderLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;

/**
 * The frame to run the alarm panel in stand alone mode
 * 
 * @author acaproni
 *
 */
public class AlarmPanelFrame extends JFrame {
	/**
	 * The ACS client
	 */
	private final AcsHelper acsHelper;
	
	/**
	 * The alarm panel
	 */
	private AlarmPanel alarmPanel;
	
	/**
	 * Constructor 
	 * 
	 * @param acsHelper
	 * @throws Exception in case of error instantiating the ACS client
	 */
	public AlarmPanelFrame() throws Exception {
		try {
				acsHelper = new AcsHelper("AlarmPanel");
		} catch (Throwable t) {
			// Something went wrong connecting the ACS client
			throw new Exception("Error connecting the ACS client: "+t.getMessage(),t);
		}
		
		initialize();
		
		alarmPanel.setServices(acsHelper.getOrb(), acsHelper.getLogger());
		alarmPanel.start();
	}
	
	/**
	 * Initialize the GUI
	 */
	private void initialize() {
		setTitle("AlarmPanel");
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		setLayout(new BorderLayout());
		alarmPanel=new AlarmPanel(this);
		add(alarmPanel,BorderLayout.CENTER);
		pack();
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent e) {
				super.windowClosed(e);
				System.out.println("Window closed");
				try {
					System.out.println("Stopping panel");
					alarmPanel.stop();
				} catch (Throwable t) {
					System.err.println("Error stopping the alarmPanel: "+t.getMessage());
				}
				if (acsHelper!=null) {
					System.out.println("Stopping acsHelper");
					acsHelper.done();
					
				}
			}
		});
		setVisible(true);
	}
	
	/**
	 * Starting point of the stand alone version of the panel
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		
		AlarmPanelFrame frame;
		try {
			frame= new AlarmPanelFrame();
		} catch (Throwable t) {
			System.err.println("Error building the panel: "+t.getMessage());
			t.printStackTrace();
			System.out.println("\nIs ACS running?\n");
		}
	}
}
