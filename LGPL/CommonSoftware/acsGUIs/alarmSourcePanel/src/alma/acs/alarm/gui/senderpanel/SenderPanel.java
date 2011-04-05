/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2011
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarm.gui.senderpanel;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Collection;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import alma.acs.alarm.gui.senderpanel.table.AlarmsSentTable;
import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.logging.ClientLogManager;

/** 
 * A panel to send alarms.
 * 
 * The panel allows the user to send alarms with a simple GUI
 * 
 * @author acaproni
 * 
 * TODO: add the properties
 */
public class SenderPanel extends JFrame implements ActionListener, DocumentListener {
	
	/**
	 * ACS component client
	 */
	private AdvancedComponentClient acsClient;
	
	/**
	 * The logger
	 */
	private Logger logger;
	
	/**
	 * ContainerServices
	 */
	private ContainerServices contSvcs;
	
	/**
	 * The text file to insert the triplet
	 */
	private final JTextField tripletTF = new JTextField();
	
	/**
	 * The text file to insert the properties
	 */
	private final JTextField propsTF = new JTextField();
	
	/**
	 * The state of the alarm to send
	 */
	private final JCheckBox activeCB = new JCheckBox();
	
	/**
	 * The button to send the alarm
	 */
	private final JButton sendBtn = new JButton("Send");
	
	/**
	 * The list of the active alarms: the user can select
	 * and clear them
	 */
	private final AlarmsSentTable alarmsSent = new AlarmsSentTable();
	
	/**
	 * The button to clear the alarm selected int he list
	 */
	private final JButton clearSelectedAlarmsBtn = new JButton("Clear selected");
	
	/**
	 * The button to clear all the alarms
	 */
	private final JButton clearAllBtn = new JButton("Clear all");
	
	/**
	 * The button to close the application
	 */
	private final JButton closeBtn = new JButton("Done");
	
	/**
	 * The obkect to send alarms
	 */
	private AlarmSender alarmSender;

	/**
	 * Constructor
	 */
	public SenderPanel() {
		super("Alarm sender panel");
		try {
			initACS();
		} catch (Throwable t) {
			JOptionPane.showMessageDialog(null, t.getMessage(), "Error initializing ACS", JOptionPane.ERROR_MESSAGE);	
		}
		alarmSender=null;
		try {
			alarmSender = new AlarmSender(logger);
		} catch (Throwable t) {
			alarmSender=null;
			JOptionPane.showMessageDialog(null, t.getMessage(), "Error initializing AlarmSender", JOptionPane.ERROR_MESSAGE);
		} 
		initGUI();
	}
	
	/**
	 * Initialize the GUI
	 */
	private void initGUI() {
		super.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent e) {
				super.windowClosed(e);
				close();
			}
		});
		
		JPanel alrmSendPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		alrmSendPnl.add(new JLabel("Triplet: "));
		alrmSendPnl.add(tripletTF);
		tripletTF.setColumns(40);
		tripletTF.getDocument().addDocumentListener(this);
		tripletTF.setToolTipText("Insert triplet: FaultFamily,FaultMember,1");
		alrmSendPnl.add(new JLabel("Active: "));
		alrmSendPnl.add(activeCB);
		alrmSendPnl.add(sendBtn);
		sendBtn.setEnabled(false);
		sendBtn.addActionListener(this);
		add(alrmSendPnl,BorderLayout.NORTH);
		
		JPanel alarmClearingPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		alarmClearingPnl.setBorder(BorderFactory.createTitledBorder("Fast clearing"));
		JScrollPane scrollPane = new JScrollPane(alarmsSent);
		alarmsSent.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		alarmClearingPnl.add(scrollPane);
		JPanel clearingBtnsPnl = new JPanel();
		BoxLayout layout = new BoxLayout(clearingBtnsPnl, BoxLayout.Y_AXIS);
		clearingBtnsPnl.setLayout(layout);
		clearingBtnsPnl.add(clearAllBtn);
		Dimension minSize = new Dimension(5, 10);
		Dimension prefSize = new Dimension(5, 10);
		Dimension maxSize = new Dimension(Short.MAX_VALUE, 10);
		clearingBtnsPnl.add(new Box.Filler(minSize, prefSize, maxSize));
		clearingBtnsPnl.add(clearSelectedAlarmsBtn);
		clearAllBtn.setEnabled(false);
		clearAllBtn.addActionListener(this);
		clearSelectedAlarmsBtn.setEnabled(false);
		clearSelectedAlarmsBtn.addActionListener(this);
		alarmClearingPnl.add(clearingBtnsPnl);
		add(alarmClearingPnl,BorderLayout.CENTER);
		
		JPanel donePnl = new JPanel(new FlowLayout(FlowLayout.CENTER));
		donePnl.add(closeBtn);
		closeBtn.addActionListener(this);
		add(donePnl,BorderLayout.SOUTH);
		
		pack();
		setVisible(true);
	}
	
	private void initACS() throws Exception {
		// Connect to ACS
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("SourcePanel",true);
		String managerLoc = System.getProperty("ACS.manager");
        if (managerLoc == null) {
                System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
                System.exit(-1);
        }
       	acsClient = new AdvancedComponentClient(logger,managerLoc,"SourcePanel");
        contSvcs=acsClient.getContainerServices();
	}
	
	private void close() {
		if (alarmSender!=null) {
			alarmSender.close();
			alarmSender=null;
		}
		try {
			if (acsClient!=null) {
				acsClient.tearDown();
			}
		} catch (Throwable t) {
			System.err.println("Error closing ComponentClient: "+t.getMessage());
		}
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==closeBtn) {
			setVisible(false);
			this.dispose();
		} else if (e.getSource()==sendBtn) {
			sendAlarm(tripletTF.getText().trim(), activeCB.isSelected());
		} else if (e.getSource()==clearAllBtn) {
			clearAllAlarms();
		} else if (e.getSource()==clearSelectedAlarmsBtn) {
			clearSelectedAlarms();
		}
	}
	
	public static void main(String[] args) {
		SenderPanel panel = new SenderPanel();
		System.out.println("Done.");
	}

	@Override
	public void insertUpdate(DocumentEvent e) {
		sendBtn.setEnabled(e.getDocument().getLength()>0 && alarmSender!=null);
	}

	@Override
	public void removeUpdate(DocumentEvent e) {
		sendBtn.setEnabled(e.getDocument().getLength()>0 && alarmSender!=null);
	}

	@Override
	public void changedUpdate(DocumentEvent e) {
		sendBtn.setEnabled(e.getDocument().getLength()>0 && alarmSender!=null);
	}
	
	/**
	 * Send an alarm by delegating to the {@link AlarmSender}.
	 * <P>
	 * This method must not be executed inside the swing thread.
	 * 
	 * @param triplet The triplet in the format FF,FM,FC
	 * @param active active <code>true</code> meanc ACTIVE, <code>false</code> means TERMINATE
	 * @throws Exception
	 */
	private void sendAlarm(final String triplet, final boolean active) {
		Runnable r = new Runnable() {
			public void run() {
				try {
					System.out.println("Sending "+triplet+", "+active);
					alarmSender.send(triplet, active);
					final int nAlarms=alarmsSent.alarmSent(triplet, active);
					SwingUtilities.invokeLater(new Runnable() {
						public void run() {
							clearAllBtn.setEnabled(nAlarms>0);
							clearSelectedAlarmsBtn.setEnabled(nAlarms>0);
						}
					});
				} catch (Throwable t) {
					t.printStackTrace();
					JOptionPane.showMessageDialog(null, t.getMessage(), "Error sending alarm "+triplet, JOptionPane.ERROR_MESSAGE);
				}
			}
		};
		Thread t = new Thread(r);
		t.setDaemon(true);
		t.start();
	}
	
	private void clearAllAlarms() {
		Collection<String> triplets= alarmsSent.getAlarms();
		System.out.println("Alarms to terminate "+triplets.size());
		for (String triplet: triplets) {
			sendAlarm(triplet, false);
		}
	}
	
	private void clearSelectedAlarms() {
		Collection<String> triplets= alarmsSent.getSelectedAlarms();
		System.out.println("Alarms to terminate "+triplets.size());
		for (String triplet: triplets) {
			sendAlarm(triplet, false);
		}
	}
}
