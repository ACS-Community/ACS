/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2008 
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
package alma.acs.logging.archive.zoom;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Date;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.Timer;

import alma.acs.util.IsoDateFormat;

import com.cosylab.gui.components.r2.DateTimeChooser;
import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * The dialog to zoom giving a time interval.
 * <P>
 * <code>ManualZoomDlg</code> allows to read logs from a set of files even if
 * the table is empty or does not contain logs in the time interval the user wants 
 * to investigate.
 * <BR>
 * Zoom preferences are not modifiable from this dialog: the user must open the
 * zoom preferences dialog instead.
 * 
 * @author acaproni
 *
 */
public class ManualZoomDlg extends JDialog implements 
ActionListener,
ZoomProgressListener,
ACSRemoteLogListener,
Runnable {
	
	/**
	 * The logging client
	 */
	private final LoggingClient loggingClient;
	
	/**
	 * The <code>ZoomManager</code> to load logs.
	 */
	private final ZoomManager zoomer;
	
	/**
	 * The widget to get the start date of loading
	 */
	private DateTimeChooser fromDTC=new DateTimeChooser();
	
	/**
	 * The widget to get the end date of loading
	 */
	private DateTimeChooser toDTC=new DateTimeChooser();
	
	/**
	 * The button to load logs
	 */
	private final JButton loadBtn = new JButton("Load");
	
	/**
	 * The button stop current loading
	 */
	private final JButton stopBtn = new JButton("Stop");
	
	/**
	 * The label showing the progress of a loading
	 */
	private final JLabel statusLbl = new JLabel("Ready");
	
	/**
	 * The thread to load logs
	 */
	public Thread thread=null;
	
	/**
	 * Number of files to read
	 */
	private int numOfFiles=0;
	
	/**
	 * Number of logs read (shown in the status line)
	 */
	private int logsRead=0;
	
	/**
	 * The timer to clean the status label
	 */
	private final Timer timer = new Timer(5000,this);
	
	/**
	 * The button to close the dialog
	 */
	private final JButton doneBtn = new JButton("Done");
	
	public ManualZoomDlg(LoggingClient owner, ZoomManager manager) {
		if (owner==null) {
			throw new IllegalArgumentException("The LoggingClient can't be null");
		}
		if (manager==null) {
			throw new IllegalArgumentException("The ZoomManager can't be null");
		}
		loggingClient=owner;
		zoomer=manager;
		timer.setRepeats(false);
		initialize();
		pack();
		setVisible(true);
	}
	
	/**
	 * Init the GUI
	 */
	private void initialize() {
		// General setup
		setTitle("Zoom");
		ImageIcon zoomIcon = new ImageIcon(LogTypeHelper.class.getResource("/zoom.png"));
		setIconImage(zoomIcon.getImage());	
		setModal(false);
		setDefaultCloseOperation(HIDE_ON_CLOSE);
		
		JPanel contentPane = new JPanel(new BorderLayout());
		
		// Add the to/from dates
		JPanel datesPnl = new JPanel();
		datesPnl.setLayout(new BoxLayout(datesPnl,BoxLayout.Y_AXIS));
		
		JPanel fromPnl = new JPanel();
		fromPnl.setBorder(BorderFactory.createTitledBorder("From"));
		fromPnl.add(fromDTC);
		datesPnl.add(fromPnl);
		
		JPanel toPnl = new JPanel();
		toPnl.setBorder(BorderFactory.createTitledBorder("To"));
		toPnl.add(toDTC);
		datesPnl.add(toPnl);
		
		contentPane.add(datesPnl,BorderLayout.NORTH);
		
		// Add the buttons
		JPanel buttonPnl = new JPanel(new BorderLayout());
		JPanel donePnl = new JPanel();
		donePnl.add(doneBtn);
		buttonPnl.add(donePnl,BorderLayout.WEST);
		doneBtn.addActionListener(this);
		JPanel actionPnl = new JPanel();
		actionPnl.add(loadBtn);
		loadBtn.setEnabled(true);
		loadBtn.addActionListener(this);
		actionPnl.add(stopBtn);
		buttonPnl.add(actionPnl,BorderLayout.EAST);
		stopBtn.setEnabled(false);
		stopBtn.addActionListener(this);
		
		contentPane.add(buttonPnl,BorderLayout.CENTER);
		
		// Add the status line
		JPanel statusPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		statusPnl.setBorder(BorderFactory.createLoweredBevelBorder());
		statusPnl.add(statusLbl);
		contentPane.add(statusPnl,BorderLayout.SOUTH);
		
		rootPane.setContentPane(contentPane);
	}
	
	/**
	 * Override <code>setVisible()</code> to move the dialog
	 * over the logging client and in front of other windows
	 */
	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		// Move the statistic win on top of jlog
		if (visible && isShowing()) {
			Point loggingPos = loggingClient.getLocationOnScreen();
			setLocation(loggingPos);
			toFront();
		}
	}
	
	/**
	 * Close the dialog
	 */
	public void close() {
		setVisible(false);
		timer.stop();
		dispose();
	}

	/**
	 * @see {@link ActionListener}
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==doneBtn) {
			setVisible(false);
		} if (e.getSource()==loadBtn) {
			if (thread==null || !thread.isAlive()) {
				thread = new Thread(this);
				thread.setDaemon(true);
				thread.setName(this.getClass().getName());
				thread.start();
			}
		} else if (e.getSource()==stopBtn) {
			zoomer.stopZoom();
		} else if (e.getSource()==timer) {
			statusLbl.setText("Ready");
		} else {
			System.out.println("Unknown source of events: "+e.getSource());
		}
	}
	
	/**
	 * The thread to load the logs in the given interval
	 */
	public void run() {
		loadBtn.setEnabled(false);
		stopBtn.setEnabled(true);
		if (!zoomer.isAvailable()) {
			JOptionPane.showMessageDialog(
					this, 
					"Zoom feature is not available.\nFix by opening the zoom preference dialog.", 
					"Zoom error", 
					JOptionPane.ERROR_MESSAGE);
			loadBtn.setEnabled(true);
			stopBtn.setEnabled(false);
			return;
		}
		if (zoomer.isLoadingLogs()) {
			JOptionPane.showMessageDialog(
					this, 
					"Zoom is already loading logs", 
					"Zoom error", 
					JOptionPane.ERROR_MESSAGE);
			loadBtn.setEnabled(true);
			stopBtn.setEnabled(false);
			return;
		}
		logsRead=numOfFiles=0;
		Date from = fromDTC.getDate();
		Date to = toDTC.getDate();
		String fromStr=IsoDateFormat.formatDate(from);
		String toStr=IsoDateFormat.formatDate(to);
		try {
			zoomer.zoom(fromStr, toStr, this, this, loggingClient);
			statusLbl.setText("Done ("+logsRead+" logs read)");
		} catch (Exception e) {
			JOptionPane.showMessageDialog(
					this, 
					"Error while loading logs.\n"+e.getMessage(), 
					"Zoom error loading", 
					JOptionPane.ERROR_MESSAGE);
		}
		timer.start();
		loadBtn.setEnabled(true);
		stopBtn.setEnabled(false);
	}

	/**
	 * @see {@link ZoomProgressListener}
	 */
	@Override
	public void zoomReadingFile(int num) {
		statusLbl.setText("Reading file "+num+" of "+numOfFiles+" ("+logsRead+" logs)");
	}

	/**
	 * @see {@link ZoomProgressListener}
	 */
	@Override
	public void zoomTotalFileToRead(int num) {
		numOfFiles=num;
		statusLbl.setText(""+num+" files to read");
	}

	/**
	 * @see ACSRemoteLogListener
	 */
	@Override
	public void logEntryReceived(ILogEntry logEntry) {
		logsRead++;
		loggingClient.logEntryReceived(logEntry);
	}

}
