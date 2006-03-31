package com.cosylab.logging.stats;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import java.awt.event.ActionListener;
import java.awt.event.WindowListener;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.BorderLayout;
import java.awt.FlowLayout;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.client.cache.LogCache;

/**
 * Shows statistics from loaded logs
 * 
 * @author acaproni
 *
 */
public class StatsDlg extends JDialog 
	implements ActionListener {
	
	private JLabel totNumOfLogsLbl = new JLabel("N/A");
	
	private JButton closeBtn = new JButton("Close");
	private JButton refreshBtn = new JButton("Refresh");
	
	// A reference to the LoggingClient
	private LoggingClient logging;;
	
	/** 
	 * Builds and show the dialog
	 * 
	 * @param logCache
	 */
	public StatsDlg(LoggingClient mainWin) {
		super();
		logging =mainWin;
		
		initialize();
        pack();
        // Start the thread
        refreshGUI();
        setVisible(true);
	}
	
	/**
	 * Setup the GUI
	 */
	private void initialize() {
		this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setTitle("Statistics");
		this.setBounds(50, 35, 100, 100);
		JPanel pnl = new JPanel(new BorderLayout());
		
		// Add the labels
		JPanel labelPanel = new JPanel(new FlowLayout());
		labelPanel.add(new JLabel("Total logs: "));
		labelPanel.add(totNumOfLogsLbl);
		pnl.add(labelPanel,BorderLayout.NORTH);
		
		// Add the refresh and the close buttons
		JPanel buttonPanel = new JPanel(new FlowLayout());
		refreshBtn.addActionListener(this);
		buttonPanel.add(refreshBtn);
		closeBtn.addActionListener(this);
		buttonPanel.add(closeBtn);
		pnl.add(buttonPanel,BorderLayout.SOUTH);
		
		setContentPane(pnl);
	}
	
	/**
	 * Refresh the values shown in the GUI
	 *
	 */
	private void refreshGUI() {
		totNumOfLogsLbl.setText(""+logging.getScrollPaneTable().getLCModel().totalLogNumber());
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==closeBtn) {
			setVisible(false);
		}if (e.getSource()==refreshBtn) {
			refreshGUI();
		}
	}
	
}
