/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
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
package alma.acs.alarm.source.gui;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.logging.ClientLogManager;
import alma.alarmsystem.clients.SourceClient;

/**
 * A very simple panel showing alarms published in the source channel.
 * 
 * This panel shows the alarms published by sources in the notification channel.
 * These are the same alarms the alarm service listens to.
 * Alarms published in the source NC can be considered as RAW alarms because 
 * they do not contain any info for operators (that is added by the alarm service 
 * component and published in the category NCs).
 * This panel can be very useful for debugging purposes when the
 * intention of the developer is to check the generation of the alarms.
 * 
 * <EM>This GUI is not intended for operators because the information it provides
 * is not complete. <B>Operators shall use the alarmPanel instead.</B></EM>
 *  
 * @author acaproni
 *
 * @since ACS7.0
 */
public class SourcePanel extends JFrame implements ActionListener {
	
	/**
	 * The client to listen to alarms from the source channel
	 */
	private SourceClient sourceClient;
	
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
	 * The table of alarms
	 */
	private SourceTable table=new SourceTable();
	
	/**
	 * The button to clear the table
	 */
	private final JButton clearButton = new JButton("Clear");
	
	/**
	 * 
	 */
	private final JToolBar toolBar = new JToolBar(JToolBar.HORIZONTAL);
	
	/**
	 * The button to show a compact view of the table
	 */
	private final JToggleButton compactTB = new JToggleButton("Compact",false);
	
	/**
	 * Constructor
	 */
	public SourcePanel() throws Exception {
		setTitle("AlarmSourcePanel");
		initializeGUI();
		try {
			initializeACS();
		} catch (Exception e) {
			System.err.println("Error connecting "+e.getMessage());
			e.printStackTrace(System.err);
			JOptionPane.showMessageDialog(null, "Error connecting", "Error", JOptionPane.ERROR_MESSAGE);
			throw e;
		}
		setVisible(true);
		// Connect to the source NC
		sourceClient.addAlarmListener(table);
		sourceClient.connect();
	}
	
	/**
	 * Init the GUI
	 */
	private void initializeGUI() {
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		addWindowListener(new WindowAdapter() {

			@Override
			public void windowClosing(WindowEvent e) {
				sourceClient.close();
				try {
					acsClient.tearDown();
				} catch (Throwable t) {
					System.err.println("Exception while closing the ComponentClient: "+t.getMessage());
				}
				super.windowClosing(e);
			}
		});
		JScrollPane scrollPane = new JScrollPane(table);
		setLayout(new BorderLayout());
		toolBar.add(clearButton);
		clearButton.addActionListener(this);
		toolBar.add(compactTB);
		compactTB.addActionListener(this);
		add(toolBar,BorderLayout.PAGE_START);
		add(scrollPane,BorderLayout.CENTER);
		setBounds(20, 20, 100, 100);
		pack();
	}
	
	/**
	 * Connect to ACS and to the source NC
	 */
	private void initializeACS() throws Exception {
		// Connect to ACS
		Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("SourcePanel",true);
		String managerLoc = System.getProperty("ACS.manager");
        if (managerLoc == null) {
                System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
                System.exit(-1);
        }
       	acsClient = new AdvancedComponentClient(logger,managerLoc,"SourcePanel");
        contSvcs=acsClient.getContainerServices();
        // Connect to the source channel
		sourceClient = new SourceClient(contSvcs);
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			new SourcePanel();
		} catch (Throwable t) {
			System.exit(-1);
		}

	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==clearButton) {
			table.clear();
		} else if (e.getSource()==compactTB) {
			table.compactTable(compactTB.isSelected());
		} else{
			System.out.print("Unknow source of events "+e);
		}
	}

}
