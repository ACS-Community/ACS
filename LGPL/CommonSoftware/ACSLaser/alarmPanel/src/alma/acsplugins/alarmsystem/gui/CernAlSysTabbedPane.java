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

import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;

import org.omg.CORBA.ORB;

import cern.laser.client.data.Alarm;

import alma.acs.logging.AcsLogger;
import alma.acsplugins.alarmsystem.gui.undocumented.UndocumentedAlarmsPnl;

/**
 * The tabbed pane fo rthe CERN alarm system.
 * <P>
 * It contains one tab for the alarms and another one with undocumnted
 * alarms
 *  
 * @author acaproni
 * @since ACS 10.0.0
 *
 */
public class CernAlSysTabbedPane extends JTabbedPane {
	
	/**
	 * The panel showing the normal alarms
	 */
	private final CernSysPanel cernPnl;
	
	/**
	 * The panel for undocumented alarms
	 */
	private final UndocumentedAlarmsPnl undocAlarmsPnl;
	
	/**
	 * The tile of the tab of undocumented alarms
	 */
	private final String undocTabTitle="Undocumented";

	/**
	 * Constructor 
	 * 
	 * @param owner The panel showing this container
	 * @param notAvaiPnl The panel when the AS is not available
	 */
	public CernAlSysTabbedPane(AlarmPanel owner, AlSysNotAvailPanel notAvaiPnl) {
		super(JTabbedPane.TOP);
		undocAlarmsPnl = new UndocumentedAlarmsPnl(this);
		cernPnl = new CernSysPanel(owner, notAvaiPnl,undocAlarmsPnl.getUndocModel());
		
		addTab("Alarms", cernPnl);
	}
	
	/**
	 * @see CernSysPanel
	 */
	public void showMessage(String mesg, boolean red) {
		cernPnl.showMessage(mesg, red);
	}

	/**
	 * @see CernSysPanel
	 */
	public void showAlarmDetails(Alarm alarm) {
		cernPnl.showAlarmDetails(alarm);
	}

	/**
	 * @see CernSysPanel
	 */
	public boolean isConnecting() {
		return cernPnl.isConnecting();
	}

	/**
	 * @see CernSysPanel
	 */
	public void setServices(ORB orb, AcsLogger logger) {
		cernPnl.setServices(orb, logger);
	}

	/**
	 * @see CernSysPanel
	 */
	public void start() throws Exception {
		cernPnl.start();
	}

	/**
	 * @see CernSysPanel
	 */
	public void stop() throws Exception {
		cernPnl.stop();
		undocAlarmsPnl.stop();
	}
	
	/**
	 * Show/hide the tab with the undocumented alarms
	 * 
	 * @param show if <code>true</code> show the tab; otherwise hide the tab
	 */
	public void undocTabVisible(final boolean show) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				if (show) {
					// Check if the tab is already visible
					boolean found=false;
					for (int t=0; t<getTabCount(); t++) {
						if (getTitleAt(t).equals(undocTabTitle)) {
							found=true;
							break;
						}
					}
					if (!found) {
						addTab(undocTabTitle,undocAlarmsPnl);
					}
				} else {
					for (int t=0; t<getTabCount(); t++) {
							if (getTitleAt(t).equals(undocTabTitle)) {
								removeTabAt(t);
								break;
							}
						}
					}
				}		
		});
	}
}
