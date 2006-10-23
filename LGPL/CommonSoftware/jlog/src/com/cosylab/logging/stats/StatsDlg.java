package com.cosylab.logging.stats;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.util.Calendar;

import com.cosylab.logging.LoggingClient;

/**
 * Shows statistics from loaded logs
 * 
 * @author acaproni
 *
 */
public class StatsDlg extends JDialog 
	implements ActionListener {
	
	private JLabel totNumOfLogsLbl = new JLabel("N/A");
	private JLabel availMemLbl = new JLabel("N/A");
	private JLabel usedMemLbl = new JLabel("N/A");
	private JLabel timeFrameLbl = new JLabel("N/A");
	
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
		JPanel mainPnl = new JPanel(new BorderLayout());
		
		JPanel valuesPnl = new JPanel(new GridLayout(4,1));
		
		// Add the num of logs
		JPanel numOfLogsPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		numOfLogsPnl.add(new JLabel("Total logs: "));
		numOfLogsPnl.add(totNumOfLogsLbl);
		valuesPnl.add(numOfLogsPnl);
		
		// Add the available memory
		JPanel availMemPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		availMemPnl.add(new JLabel("Memory available: "));
		availMemPnl.add(availMemLbl);
		valuesPnl.add(availMemPnl);
		
		// Add the used memory
		JPanel usedMemPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		usedMemPnl.add(new JLabel("Used memory: "));
		usedMemPnl.add(usedMemLbl);
		valuesPnl.add(usedMemPnl);
		
		// Add the time frame
		JPanel timeFramePnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		timeFramePnl.add(new JLabel("Time frame: "));
		timeFramePnl.add(timeFrameLbl);
		valuesPnl.add(timeFramePnl);
		
		mainPnl.add(valuesPnl,BorderLayout.CENTER);
		
		// Add the refresh and the close buttons
		JPanel buttonPanel = new JPanel(new FlowLayout());
		refreshBtn.addActionListener(this);
		buttonPanel.add(refreshBtn);
		closeBtn.addActionListener(this);
		buttonPanel.add(closeBtn);
		mainPnl.add(buttonPanel,BorderLayout.SOUTH);
		
		setContentPane(mainPnl);
	}
	
	/**
	 * Refresh the values shown in the GUI
	 *
	 */
	private void refreshGUI() {
		Runnable refresh = new Runnable() {
			public void run() {
				totNumOfLogsLbl.setText(""+logging.getLogEntryTable().getLCModel().totalLogNumber());
				Runtime rt = Runtime.getRuntime();
				long freeMem = rt.freeMemory();
				long totMem = rt.totalMemory();
				availMemLbl.setText(""+(freeMem/1024)+"Kb");
		        usedMemLbl.setText(""+((totMem-freeMem)/1024)+"Kb");
/**
 * @todo GCH 2006-10-23
 * Commented out because the getTimeFrame method appears to be missing.
		        Calendar cal = logging.getLogEntryTable().getLCModel().getTimeFrame();
		        StringBuilder str = new StringBuilder();
		        str.append(cal.get(Calendar.DAY_OF_YEAR)-1);
		        str.append("days - ");
		        str.append(cal.get(Calendar.HOUR_OF_DAY));
		        str.append(":");
		        str.append(cal.get(Calendar.MINUTE));
		        str.append(":");
		        str.append(cal.get(Calendar.SECOND));
		        str.append(".");
		        str.append(cal.get(Calendar.MILLISECOND));
		        timeFrameLbl.setText(str.toString());
*/
		        pack();
			}
		};
		SwingUtilities.invokeLater(refresh);
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==closeBtn) {
			setVisible(false);
		}if (e.getSource()==refreshBtn) {
			refreshGUI();
		}
	}
	
}
