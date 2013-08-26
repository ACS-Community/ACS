/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
package alma.acs.logging.archive;

import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Calendar;
import java.util.Collection;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;

import alma.acs.logging.archive.ArchiveConnectionManager.DBState;
import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;
import alma.acs.logging.io.LoadSwitchesPanel;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.engine.LogMatcher;
import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.settings.LogTypeRenderer;

/**
 * A class to setup a query to submit to the DB
 * 
 * @author acaproni
 *
 */
public class QueryDlg extends JDialog implements ActionListener {
	
	//	The listener for the logs read from the DB
	private ACSRemoteLogListener logListener;
	
	// The listener for errors parsing logs
	private ACSRemoteErrorListener errorListener;
	
	private JButton submitBtn;
	private JButton doneBtn;
	
	// The archive
	private ArchiveConnectionManager archive;
	
	// The time limits for the query
	private JTextField fromYY, fromMM, fromDD, fromHr, fromMin, fromSec;
	private JTextField toYY, toMM, toDD, toHr, toMin, toSec;
	
	// The min and max log type
	private JComboBox minLogLevelCB, maxLogLevelCB;
	
	// The process name for the query
	private JTextField procName;
	
	// The name of the routine
	private JTextField sourceName;
	
	private JTextField routineName;
	
	// The max number of log to get from the DB
	private JTextField rowLimit;
	
	// The parser
	private ACSLogParser parser = null;
	
	// The logging client
	private LoggingClient loggingClient=null;
	
	// The switches to clear the table and disconnect from the NC 
	// before submitting a query
	private LoadSwitchesPanel guiSwitches;
	
	/**
	 * The label showing the status of a query to
	 * give feedback to users
	 */
	private JLabel statusLbl = new JLabel();
	
	/**
	 * Signal the thread that get logs from the DB that the user pressed cancel to abort
	 */
	private volatile boolean terminateThread;

	/**
	 * Objects of the <code>IntegerDocumentListener</code> checks that the user only inserts numbers
	 * in the text field and if it is not the case, removes invalid characters from the 
	 * document.
	 * 
	 * @author acaproni
	 * since ACS 10.2
	 */
	private static class IntegerDocumentListener implements DocumentListener {
		
		/**
		 * The matcher to check that the inserted string contains only numbers
		 */
		private final Pattern pattern = Pattern.compile("[0-9]+");
		
		/**
		 * This method is invoked when the user adds a new character in the text field.
		 * We check that the new string matches with the regular expression and,
		 * if it is not the case, remove the character from the text field.
		 * 
		 * @see DocumentListener#insertUpdate(DocumentEvent)
		 */
		@Override
		public void insertUpdate(final DocumentEvent e) {
			
			String insertedText = null;
			try {
				insertedText=e.getDocument().getText(e.getOffset(), e.getLength());
			} catch (BadLocationException ble) {
				ble.printStackTrace();
			}
			if (insertedText==null || insertedText.isEmpty()) {
				// Nothing to do in this rare case
				return;
			}
			Matcher m = pattern.matcher(insertedText);
			if (!m.matches()) {
				// The test does not match with the regular expression so 
				// we remove the string from the text field
				SwingUtilities.invokeLater(new Runnable() {
					public void run() {
						try {
							e.getDocument().remove(e.getOffset(), e.getLength());
						} catch (BadLocationException ble) {
							// In case something strange happened removing the character, we clear the whole text
							// to be sure we do not have spurious data in the text field.
							try {
								e.getDocument().remove(0, e.getDocument().getLength());
							} catch (BadLocationException ble2) {}
							
						}
					}
				});
			}
		}

		@Override
		public void removeUpdate(DocumentEvent e) {}

		@Override
		public void changedUpdate(DocumentEvent e) {}
		
	}
	
	/**
	 * Some of the inout fields whose value could be wrong.
	 *  
	 * @author acaproni
	 *
	 */
	public enum FieldState {
		OK("OK"), // All the inoput fields contain valid data
		FROMDATE_ERROR("Start date"), // At least one of the field of the starting date is wrong
		TODATE_ERROR("End date"),// At least one of the field of the ending date is wrong
		LOGNUM_ERROR("Max num. of logs to read"); // Max number of logs to retrieve is wrong
		
		/**
		 * The description
		 */
		public final String desc;
		
		/**
		 * Constructor
		 * 
		 * @param desc The description
		 */
		private FieldState(String desc) {
			this.desc=desc;
		}
	}

	/**
	 * Constructor
	 */
	public QueryDlg(
			ArchiveConnectionManager archiveConn, 
			ACSRemoteLogListener listener,
			ACSRemoteErrorListener errorListener,
			LoggingClient client) {
		super();
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null listener!");
		}
		if (errorListener==null) {
			throw new IllegalArgumentException("Invalid null error listener!");
		}
		if (client==null) {
			throw new IllegalArgumentException("Invalid null LoggingClient!");
		}
		loggingClient=client;
		try
		{
			parser = ACSLogParserFactory.getParser();
		}
		catch (Exception pce)
		{
			System.out.println("Exception in QueryDlg constructor: " + pce);
			pce.printStackTrace();
			parser=null;
		}
		this.logListener=listener;
		this.errorListener=errorListener;
		setTitle("Load from database");
		archive = archiveConn;
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		initGUI();
		setBounds(50,50,50,50);
		pack();
		setVisible(true);
	}
	
	/**
	 * @see java.awt.event.ActionListener
	 * @see java.awt.event.ActionEvent
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==submitBtn) {
			Thread t = new Thread() {
				public void run() {
					loggingClient.animateProgressBar("Loading from DB");
					terminateThread=false;
					enableInputFields(false);
					try {
						submitQuery();
					} catch (Throwable t) {
						t.printStackTrace();
					} finally {
						enableInputFields(true);
						loggingClient.freezeProgressBar();
						submitBtn.setEnabled(true);
					}
				}
			};
			t.setDaemon(false);
			t.setName("DatabaseQuery");
			t.start();
		} else if (e.getSource()==doneBtn) {
			terminateThread=true;
			setVisible(false);
			dispose();
		} else {
			System.err.println("Unknown event "+e);
		}
	}
	
	/**
	 * Build the GUI
	 *
	 */
	private void initGUI() {
		// The actual time/date used to fill the time fields 
		Calendar calendar = Calendar.getInstance();
		
		guiSwitches = new LoadSwitchesPanel(loggingClient);
		
		JRootPane mainPnl = this.getRootPane();
		mainPnl.setLayout(new BorderLayout());
		
		// The panel with the option of the query
		JPanel optionsPnl = new JPanel();
		GridBagLayout prefsLayout = new GridBagLayout();
		GridBagConstraints c = new GridBagConstraints();
		optionsPnl.setLayout(prefsLayout);
		// Add all the labels
		JLabel maxLogs = new JLabel("Max num of logs to load:");
		c.gridx=0; c.gridy=0; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(10,5,5,5);
		optionsPnl.add(maxLogs,c);
		JLabel fromLbl = new JLabel("From:");
		c.gridx=0; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(fromLbl,c);
		JLabel toLbl = new JLabel("To:");
		c.gridx=0; c.gridy=2; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(toLbl,c);
		JLabel routinNameLbl = new JLabel("Routine name:");
		c.gridx=0; c.gridy=3; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(routinNameLbl,c);
		JLabel procNameLbl = new JLabel("Process name:");
		c.gridx=0; c.gridy=4; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(procNameLbl,c);
		JLabel srcNameLbl = new JLabel("Source object:");
		c.gridx=0; c.gridy=5; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(srcNameLbl,c);
		JLabel minLogType = new JLabel("From type:");
		c.gridx=0; c.gridy=6; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(minLogType,c);
		JLabel maxLogType = new JLabel("To type:");
		c.gridx=0; c.gridy=7; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,10,5);
		optionsPnl.add(maxLogType,c);
		
		
		// Add the input widgets
		fromYY = new JTextField(Integer.toString(calendar.get(Calendar.YEAR)),4);
		fromYY.setName(fromLbl.getText());
		c.gridx=1; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,0);
		fromYY.getDocument().addDocumentListener(new IntegerDocumentListener());
		optionsPnl.add(fromYY,c);
		JLabel separatorF1 = new JLabel("-");
		c.gridx=2; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,0,5,0);
		optionsPnl.add(separatorF1,c);
		fromMM = new JTextField(Integer.toString(calendar.get(Calendar.MONTH)+1),2);
		fromMM.setName(fromLbl.getText());
		c.gridx=3; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START;
		fromMM.getDocument().addDocumentListener(new IntegerDocumentListener());
		optionsPnl.add(fromMM,c);
		JLabel separatorF2 = new JLabel("-");
		c.gridx=4; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(separatorF2,c);
		fromDD= new JTextField(Integer.toString(calendar.get(Calendar.DAY_OF_MONTH)),2);
		fromDD.setName(fromLbl.getText());
		c.gridx=5; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; 
		fromDD.getDocument().addDocumentListener(new IntegerDocumentListener());
		optionsPnl.add(fromDD,c);
		JLabel tlbl = new JLabel("T");
		c.gridx=6; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(tlbl,c);
		fromHr= new JTextField(Integer.toString(calendar.get(Calendar.HOUR_OF_DAY)),2);
		fromHr.setName(fromLbl.getText());
		c.gridx=7; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; 
		fromHr.getDocument().addDocumentListener(new IntegerDocumentListener());
		optionsPnl.add(fromHr,c);
		JLabel comaF1Lbl = new JLabel(":");
		c.gridx=8; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; 
		optionsPnl.add(comaF1Lbl,c);
		fromMin = new JTextField(Integer.toString(calendar.get(Calendar.MINUTE)),2);
		fromMin.setName(fromLbl.getText());
		c.gridx=9; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; 
		fromMin.getDocument().addDocumentListener(new IntegerDocumentListener());
		optionsPnl.add(fromMin,c);
		JLabel comaF2Lbl = new JLabel(":");
		c.gridx=10; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; 
		optionsPnl.add(comaF2Lbl,c);
		fromSec= new JTextField(Integer.toString(calendar.get(Calendar.SECOND)),2);
		fromSec.setName(fromLbl.getText());
		c.gridx=11; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; c.gridwidth=GridBagConstraints.REMAINDER; 
		fromSec.getDocument().addDocumentListener(new IntegerDocumentListener());
		optionsPnl.add(fromSec,c);
		
		toYY = new JTextField(Integer.toString(calendar.get(Calendar.YEAR)),4);
		toYY.setName(toLbl.getText());
		c.gridx=1; c.gridy=2; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,0);
		toYY.getDocument().addDocumentListener(new IntegerDocumentListener());
		optionsPnl.add(toYY,c);
		JLabel separatorTo1 = new JLabel("-");
		c.gridx=2; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,0,5,0);
		optionsPnl.add(separatorTo1,c);
		toMM = new JTextField(Integer.toString(calendar.get(Calendar.MONTH)+1),2);
		toMM.setName(toLbl.getText());
		c.gridx=3; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		toMM.getDocument().addDocumentListener(new IntegerDocumentListener());
		optionsPnl.add(toMM,c);
		JLabel separatorTo2 = new JLabel("-");
		c.gridx=4; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(separatorTo2,c);
		toDD= new JTextField(Integer.toString(calendar.get(Calendar.DAY_OF_MONTH)),2);
		toDD.setName(toLbl.getText());
		c.gridx=5; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		toDD.getDocument().addDocumentListener(new IntegerDocumentListener());
		optionsPnl.add(toDD,c);
		JLabel t2lbl = new JLabel("T");
		c.gridx=6; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(t2lbl,c);
		toHr= new JTextField(Integer.toString(calendar.get(Calendar.HOUR_OF_DAY)),2);
		toHr.setName(toLbl.getText());
		c.gridx=7; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		toHr.getDocument().addDocumentListener(new IntegerDocumentListener());
		optionsPnl.add(toHr,c);
		JLabel comaTo1Lbl = new JLabel(":");
		c.gridx=8; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(comaTo1Lbl,c);
		toMin = new JTextField(Integer.toString(calendar.get(Calendar.MINUTE)),2);
		toMin.setName(toLbl.getText());
		c.gridx=9; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		toMin.getDocument().addDocumentListener(new IntegerDocumentListener());
		optionsPnl.add(toMin,c);
		JLabel comaTo2Lbl = new JLabel(":");
		c.gridx=10; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(comaTo2Lbl,c);
		toSec= new JTextField(Integer.toString(calendar.get(Calendar.SECOND)),2);
		toSec.setName(toLbl.getText());
		c.gridx=11; c.gridy=2; c.anchor=GridBagConstraints.LAST_LINE_START; c.gridwidth=GridBagConstraints.REMAINDER;
		toSec.getDocument().addDocumentListener(new IntegerDocumentListener());
		optionsPnl.add(toSec,c);
		
		rowLimit = new JTextField("10000",20);
		rowLimit.setName(maxLogs.getText());
		rowLimit.setToolTipText("Valid range  is [0, "+(Integer.MAX_VALUE-1)+"]");
		rowLimit.getDocument().addDocumentListener(new IntegerDocumentListener());
		c.gridx=1; c.gridy=0; c.gridwidth=GridBagConstraints.REMAINDER; c.insets = new Insets(10,5,5,5);
		optionsPnl.add(rowLimit,c);
		routineName = new JTextField("*",20);
		c.gridx=1; c.gridy=3; c.gridwidth=GridBagConstraints.REMAINDER; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(routineName,c);
		procName = new JTextField("*",20);
		c.gridx=1; c.gridy=4; c.gridwidth=GridBagConstraints.REMAINDER; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(procName,c);
		sourceName = new JTextField("*",20);
		c.gridx=1; c.gridy=5; c.gridwidth=GridBagConstraints.REMAINDER; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(sourceName,c);
		minLogLevelCB = setupTypeCB(minLogLevelCB);
		minLogLevelCB.setSelectedIndex(LogTypeHelper.INFO.ordinal());
		c.gridx=1; c.gridy=6; c.gridwidth=GridBagConstraints.REMAINDER; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(minLogLevelCB,c);
		maxLogLevelCB= setupTypeCB(maxLogLevelCB);
		maxLogLevelCB.setSelectedIndex(LogTypeHelper.EMERGENCY.ordinal());
		c.gridx=1; c.gridy=7; c.gridwidth=GridBagConstraints.REMAINDER; c.insets = new Insets(5,5,10,5);
		optionsPnl.add(maxLogLevelCB,c);
		
		// Add the OK, CANCEL buttons
		JPanel bottomPanel = new JPanel(new BorderLayout());
		JPanel btnPnl = new JPanel();
		btnPnl.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
		BoxLayout boxLayout = new BoxLayout(btnPnl,BoxLayout.LINE_AXIS);
		btnPnl.setLayout(boxLayout);
		btnPnl.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10));
		submitBtn = new JButton("Submit");
		submitBtn.addActionListener(this);
		submitBtn.setEnabled(archive.getDBStatus()==DBState.DATABASE_OK);
		doneBtn = new JButton("Cancel");
		doneBtn.addActionListener(this);
		btnPnl.add(submitBtn,BorderLayout.WEST);
		btnPnl.add(Box.createRigidArea(new Dimension(10, 0)));
		btnPnl.add(doneBtn,BorderLayout.EAST);
		btnPnl.add(Box.createRigidArea(new Dimension(10, 0)));
		
		// Set the border and a smaller font for the label
		statusLbl.setBorder(BorderFactory.createLoweredBevelBorder());
		Font fnt = statusLbl.getFont();
		Font newFont = fnt.deriveFont(fnt.getSize()*2/3);
		statusLbl.setFont(newFont);
		bottomPanel.add(btnPnl,BorderLayout.EAST);
		bottomPanel.add(statusLbl,BorderLayout.CENTER);
		// Add the subpanels
		mainPnl.add(guiSwitches,BorderLayout.NORTH);
		mainPnl.add(optionsPnl,BorderLayout.CENTER);
		mainPnl.add(bottomPanel,BorderLayout.SOUTH);
	}
	
	private JComboBox setupTypeCB(JComboBox box) {
		String Descritpions[] = new String[LogTypeHelper.values().length];
		int t=0;
		for (LogTypeHelper log: LogTypeHelper.values()) {
			Descritpions[t++]=log.logEntryType;
		}
		JComboBox logLevelCB = new JComboBox(Descritpions);
        
        // Build the renderer for the combo boxesx
        LogTypeRenderer rendererCB = new LogTypeRenderer();
        
        logLevelCB.setEditable(false);
        logLevelCB.setMaximumRowCount(LogTypeHelper.values().length);
        logLevelCB.setRenderer(rendererCB);
		return logLevelCB;
	}
	
	/**
	 * Submit a query to the archive and insert the logs in the main window
	 *
	 */
	private void submitQuery() {
		FieldState chekResult=checkFields();
		if (chekResult!=FieldState.OK) {
			JOptionPane.showMessageDialog(this,"<HTML>Error getting values from the form!<BR>Check the values of "+chekResult.desc,"Input error!",JOptionPane.ERROR_MESSAGE);
			return;
		}
		loggingClient.reportStatus("Submitting a query");
		guiSwitches.execute(); // Clear the logs and disconnect from the NC
		StringBuilder from=new StringBuilder(fromYY.getText());
		from.append('-');
		if (fromMM.getText().length()==1) {
			from.append('0');
		}
		from.append(fromMM.getText());
		from.append('-');
		if (fromDD.getText().length()==1) {
			from.append('0');
		}
		from.append(fromDD.getText());
		from.append('T');
		if (fromHr.getText().length()==1) {
			from.append('0');
		}
		from.append(fromHr.getText());
		from.append(':');
		if (fromMin.getText().length()==1) {
			from.append('0');
		}
		from.append(fromMin.getText());
		from.append(':');
		if (fromSec.getText().length()==1) {
			from.append('0');
		}
		from.append(fromSec.getText());
		
		StringBuilder to=new StringBuilder(toYY.getText());
		to.append('-');
		if (toMM.getText().length()==1) {
			to.append('0');
		}
		to.append(toMM.getText());
		to.append('-');
		if (toDD.getText().length()==1) {
			to.append('0');
		}
		to.append(toDD.getText());
		to.append('T');
		if (toHr.getText().length()==1) {
			to.append('0');
		}
		to.append(toHr.getText());
		to.append(':');
		if (toMin.getText().length()==1) {
			to.append('0');
		}
		to.append(toMin.getText());
		to.append(':');
		if (toSec.getText().length()==1) {
			to.append('0');
		}
		to.append(toSec.getText());
		
		short minType = (short)LogTypeHelper.values()[minLogLevelCB.getSelectedIndex()].acsCoreLevel.value;
		short maxType = (short)LogTypeHelper.values()[maxLogLevelCB.getSelectedIndex()].acsCoreLevel.value;
		
		String routine = routineName.getText();
		if (routine.length()==0) {
			routine ="*";
		}
		String source= sourceName.getText();
		if (source.length()==0) {
			source ="*";
		}
		String process = procName.getText();
		if (process.length()==0) {
			process ="*";
		}
		int maxRows = Integer.parseInt(rowLimit.getText());
		
		// The collection where the logs read from the DB are stored
		Collection logs = null;
		updateStatusLbl("Submitting query");
		try {
			logs = archive.getLogs(from.toString()+".000",to.toString()+".000",minType,maxType,routine,source,process,maxRows);
		} catch (Throwable t) {
			System.err.println("Error executing the query: "+t.getMessage());
			t.printStackTrace(System.err);
			JOptionPane.showMessageDialog(this,formatErrorMsg("Error executing the query:\n"+t.getMessage()),"Database error!",JOptionPane.ERROR_MESSAGE);
			loggingClient.reportStatus("Query terminated with error");
		}
		if (logs!=null && !logs.isEmpty()) {
			loggingClient.reportStatus("Num. of logs read from DB: "+logs.size());
			LogMatcher matcher = new LogMatcher();
			matcher.setAudience(loggingClient.getEngine().getAudience());
			matcher.setFilters(loggingClient.getEngine().getFilters());
			matcher.setDiscardLevel(loggingClient.getEngine().getDiscardLevel());
			Iterator iter = logs.iterator();
			int count=0;
			while (iter.hasNext() && !terminateThread) {
				if ((++count)%1000==0) {
					updateStatusLbl("Flushing logs "+count+"/"+logs.size());
				}
				String str = (String)iter.next();
				ILogEntry logEntry=null;
				try {
					logEntry = parser.parse(str);
				} catch (Exception e) {
					errorListener.errorReceived(str);
					continue;
				}
				if (matcher.match(logEntry)) {
					logListener.logEntryReceived(logEntry);
				}
			}
			
			logs.clear();
			logs=null;
		}
		updateStatusLbl("");
		// Update the state of the switches
		guiSwitches.checkControlsState();
	}
	
	/**
	 * Check the fields in the GUI before executing a query.
	 * <P>
	 * <code>checkFields</code> makes only some basic checks.
	 * The state returned is OK or the first error found while checking.
	 * 
	 * @return The error state or OK if all the input fields are valid
	 */
	private FieldState checkFields() {
		
		boolean ret = 
			Pattern.matches("[0-9]+",fromYY.getText()) &&
			Pattern.matches("[0-9]+",fromMM.getText()) &&
			Pattern.matches("[0-9]+",fromMM.getText()) &&
			Pattern.matches("[0-9]+",fromHr.getText()) &&
			Pattern.matches("[0-9]+",fromMin.getText()) &&
			Pattern.matches("[0-9]+",fromSec.getText()); 
		if (!ret) {
			return FieldState.FROMDATE_ERROR;
		}
		ret=
			Pattern.matches("[0-9]+",toYY.getText()) &&
			Pattern.matches("[0-9]+",toMM.getText()) &&
			Pattern.matches("[0-9]+",toDD.getText()) &&
			Pattern.matches("[0-9]+",toHr.getText()) &&
			Pattern.matches("[0-9]+",toMin.getText()) &&
			Pattern.matches("[0-9]+",toSec.getText());
		if (!ret) {
			return FieldState.TODATE_ERROR;
		}
		if (!Pattern.matches("[0-9]+",rowLimit.getText())) {
			return FieldState.LOGNUM_ERROR;
		}
			
		int fromY, fromD,fromM, from_h,from_m,from_s;
		try {
			fromY = Integer.parseInt(fromYY.getText());
			fromM= Integer.parseInt(fromMM.getText());
			fromD=Integer.parseInt(fromDD.getText());
			from_h=Integer.parseInt(fromHr.getText());
			from_m=Integer.parseInt(fromMin.getText());
			from_s=Integer.parseInt(fromSec.getText());
		} catch (Exception e) {
			// An error parsing so at least one field contains a invalid number
			return FieldState.FROMDATE_ERROR;
		}
		int toY, toM, toD, to_h, to_m, to_s;
		try {
			toY=Integer.parseInt(toYY.getText());
			toM=Integer.parseInt(toMM.getText());
			toD=Integer.parseInt(toDD.getText());
			to_h=Integer.parseInt(toHr.getText());
			to_m=Integer.parseInt(toMin.getText());
			to_s=Integer.parseInt(toSec.getText());
		} catch (Exception e) {
			// An error parsing so at least one field contains a invalid number
			return FieldState.TODATE_ERROR;
		}	
		long rLimit;
		try {
			rLimit = Long.parseLong(rowLimit.getText());
		} catch (Exception e) {
			// An error parsing so at least one field contains a invalid number
			return FieldState.LOGNUM_ERROR;
		}
		
		ret = fromY>=2000 && fromY<2100;
		ret = ret && fromM>=1 && fromM<=12;
		ret = ret && fromD>=1 && fromD<=31;
		ret = ret && from_h>=0 && from_h<24;
		ret = ret && from_m>=0 && from_m<60;
		ret = ret && from_s>=0 && from_s<60;
		if (!ret) {
			return FieldState.FROMDATE_ERROR;
		}
		
		ret = toY>=2000 && toY<2100;
		ret = ret && toM>=1 && toM<=12;
		ret = ret && toD>=1 && toD<=31;
		ret = ret && to_h>=0 && to_h<24;
		ret = ret && to_m>=0 && to_m<60;
		ret = ret && to_s>=0 && to_s<60;
		if (!ret) {
			return FieldState.TODATE_ERROR;
		}
		if (rLimit<=0 || rLimit>=Integer.MAX_VALUE) {
			return FieldState.LOGNUM_ERROR;
		}
		
			
        return FieldState.OK;
	}
	
	/**
	 * Return a string formatted for JOptionPane making a word wrap
	 * 
	 * @param error The error i.e. the exception
	 * @param msg The message to show
	 * @return A formatted string
	 */
	private String formatErrorMsg(String msg) {
		StringBuilder sb = new StringBuilder();
		int count = 0;
		for (int t=0; t<msg.length(); t++) {
			char c = msg.charAt(t);
			sb.append(c);
			if (c=='\n') {
				count=0;
				continue;
			}
			if (++count >= 80 && c==' ') {
				count=0;
				sb.append('\n');
			}
		}
		return sb.toString();
	}
	
	/**
	 * Set a new message in the status label.
	 * 
	 * @param msg The message to show in the label
	 */
	private void updateStatusLbl(final String msg) {
		class NewMessage implements Runnable {
			public void run() {
				statusLbl.setText(msg);
			}
		}
		SwingUtilities.invokeLater(new NewMessage());
	}
	
	/**
	 * Override <code>setVisible()</code> to move the statistic window
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
		// Refresh the state of the switches to disconnect and clear the table
		guiSwitches.checkControlsState();
	}
	
	/**
	 * Close the dialog, terminate the thread a free all the resources.
	 * 
	 */
	public void close() {
		terminateThread=true;
		setVisible(false);
		dispose();
	}
	
	/**
	 * Enable/disable the input fields
	 * @param enable if <code>true</code> the widgets are enabled otherwise disabled
	 */
	private void enableInputFields(final boolean enable) {
		try {
			SwingUtilities.invokeAndWait(new Runnable() {
			@Override
			public void run() {
				fromYY.setEnabled(enable);
				fromDD.setEnabled(enable);
				fromMM.setEnabled(enable);
				fromHr.setEnabled(enable);
				fromMin.setEnabled(enable);
				fromSec.setEnabled(enable);
				
				toYY.setEnabled(enable);
				toMM.setEnabled(enable);
				toDD.setEnabled(enable);
				toHr.setEnabled(enable);
				toMin.setEnabled(enable);
				toSec.setEnabled(enable);
				
				rowLimit.setEnabled(enable);
				
				routineName.setEnabled(enable);
				procName.setEnabled(enable);
				sourceName.setEnabled(enable);
				minLogLevelCB.setEnabled(enable);
				maxLogLevelCB.setEnabled(enable);
				submitBtn.setEnabled(enable);
			}
		});
		} catch (Throwable t) {}
	}
}
