
package alma.acs.logging.archive;

import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Calendar;
import java.util.Collection;
import java.util.Iterator;
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
import javax.swing.KeyStroke;

import com.cosylab.logging.engine.ACS.ACSLogParser;
import com.cosylab.logging.engine.ACS.ACSLogParserDOM;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.settings.ErrorLogDialog;
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
	
	private ACSLogParser parser = null;
	
	/**
	 * A class with a thread to publish the logs to the listener
	 * @author acaproni
	 *
	 */
	private class LogsPublisher extends Thread {
		Collection logsToFlush;
		JButton subBtn;
		
		/**
		 * Constructor 
		 * 
		 * @param logs The logs to publish
		 * @param prsr The parser
		 * @param btn The submit button to enable at the end of the flush
		 */
		public LogsPublisher(Collection logs, ACSLogParser prsr, JButton btn) {
			if (logs==null || btn==null) {
				throw new IllegalArgumentException("Invalid null argument in the constructor");
			}
			logsToFlush =logs;
			subBtn=btn;
		}
		/**
		 * The thread to publish the logs to the listener
		 *
		 */
		public void run () {
			System.out.println("Flushing logs");
			if (logsToFlush==null || logsToFlush.size()==0) {
				logsToFlush.clear();
				logsToFlush=null;
				subBtn.setEnabled(true);
				return;
			}
			Iterator iter = logsToFlush.iterator();
			while (iter.hasNext()) {
				String str = (String)iter.next();
				ILogEntry logEntry=null;
				try {
					logEntry = parser.parse(str);
				} catch (Exception e) {
					System.err.println("Exception parsing a log: "+e.getMessage());
					System.out.println("Log Str: ["+str+"]");
					StringBuilder strBuilder = new StringBuilder("\nError parsing the following Log:\n");
					strBuilder.append(formatErrorMsg(str));
					strBuilder.append("\n");
					ErrorLogDialog.getErrorLogDlg(true).appendText(strBuilder.toString());
					continue;
				}
				logListener.logEntryReceived(logEntry);
			}
			logsToFlush.clear();
			logsToFlush=null;
			subBtn.setEnabled(true);
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
	}

	/**
	 * Empty constructor
	 */
	public QueryDlg(ArchiveConnectionManager archiveConn, ACSRemoteLogListener listener) {
		super();
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null listener!");
		}
		try
		{
			parser = new ACSLogParserDOM();
		}
		catch (javax.xml.parsers.ParserConfigurationException pce)
		{
			System.out.println("Exception in QueryDlg constructor: " + pce);
			pce.printStackTrace();
			parser=null;
		}
		logListener=listener;
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
			submitBtn.setEnabled(false);
			submitQuery();
		} else if (e.getSource()==doneBtn) {
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
		c.gridx=1; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,0);
		optionsPnl.add(fromYY,c);
		JLabel separatorF1 = new JLabel("-");
		c.gridx=2; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,0,5,0);
		optionsPnl.add(separatorF1,c);
		fromMM = new JTextField(Integer.toString(calendar.get(Calendar.MONTH)+1),2);
		c.gridx=3; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(fromMM,c);
		JLabel separatorF2 = new JLabel("-");
		c.gridx=4; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(separatorF2,c);
		fromDD= new JTextField(Integer.toString(calendar.get(Calendar.DAY_OF_MONTH)),2);
		c.gridx=5; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; 
		optionsPnl.add(fromDD,c);
		JLabel tlbl = new JLabel("T");
		c.gridx=6; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(tlbl,c);
		fromHr= new JTextField(Integer.toString(calendar.get(Calendar.HOUR_OF_DAY)),2);
		c.gridx=7; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; 
		optionsPnl.add(fromHr,c);
		JLabel comaF1Lbl = new JLabel(":");
		c.gridx=8; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; 
		optionsPnl.add(comaF1Lbl,c);
		fromMin = new JTextField(Integer.toString(calendar.get(Calendar.MINUTE)),2);
		c.gridx=9; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; 
		optionsPnl.add(fromMin,c);
		JLabel comaF2Lbl = new JLabel(":");
		c.gridx=10; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; 
		optionsPnl.add(comaF2Lbl,c);
		fromSec= new JTextField(Integer.toString(calendar.get(Calendar.SECOND)),2);
		c.gridx=11; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; c.gridwidth=GridBagConstraints.REMAINDER;  
		optionsPnl.add(fromSec,c);
		
		toYY = new JTextField(Integer.toString(calendar.get(Calendar.YEAR)),4);
		c.gridx=1; c.gridy=2; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,0);
		optionsPnl.add(toYY,c);
		JLabel separatorTo1 = new JLabel("-");
		c.gridx=2; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,0,5,0);
		optionsPnl.add(separatorTo1,c);
		toMM = new JTextField(Integer.toString(calendar.get(Calendar.MONTH)+1),2);
		c.gridx=3; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(toMM,c);
		JLabel separatorTo2 = new JLabel("-");
		c.gridx=4; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(separatorTo2,c);
		toDD= new JTextField(Integer.toString(calendar.get(Calendar.DAY_OF_MONTH)),2);
		c.gridx=5; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(toDD,c);
		JLabel t2lbl = new JLabel("T");
		c.gridx=6; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(t2lbl,c);
		toHr= new JTextField(Integer.toString(calendar.get(Calendar.HOUR_OF_DAY)),2);
		c.gridx=7; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(toHr,c);
		JLabel comaTo1Lbl = new JLabel(":");
		c.gridx=8; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(comaTo1Lbl,c);
		toMin = new JTextField(Integer.toString(calendar.get(Calendar.MINUTE)),2);
		c.gridx=9; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(toMin,c);
		JLabel comaTo2Lbl = new JLabel(":");
		c.gridx=10; c.gridy=2; c.gridwidth=GridBagConstraints.RELATIVE; c.anchor=GridBagConstraints.LAST_LINE_START;
		optionsPnl.add(comaTo2Lbl,c);
		toSec= new JTextField(Integer.toString(calendar.get(Calendar.SECOND)),2);
		c.gridx=11; c.gridy=2; c.anchor=GridBagConstraints.LAST_LINE_START; c.gridwidth=GridBagConstraints.REMAINDER;
		optionsPnl.add(toSec,c);
		
		rowLimit = new JTextField("10000",20);
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
		minLogLevelCB.setSelectedIndex(LogTypeHelper.ENTRYTYPE_INFO);
		c.gridx=1; c.gridy=6; c.gridwidth=GridBagConstraints.REMAINDER; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(minLogLevelCB,c);
		maxLogLevelCB= setupTypeCB(maxLogLevelCB);
		maxLogLevelCB.setSelectedIndex(LogTypeHelper.ENTRYTYPE_EMERGENCY);
		c.gridx=1; c.gridy=7; c.gridwidth=GridBagConstraints.REMAINDER; c.insets = new Insets(5,5,10,5);
		optionsPnl.add(maxLogLevelCB,c);
		
		// Add the OK, CANCEL buttons
		JPanel btnPnl = new JPanel();
		btnPnl.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
		BoxLayout boxLayout = new BoxLayout(btnPnl,BoxLayout.LINE_AXIS);
		btnPnl.setLayout(boxLayout);
		btnPnl.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10));
		submitBtn = new JButton("Submit");
		submitBtn.addActionListener(this);
		submitBtn.setEnabled(archive.getDBStatus()==ArchiveConnectionManager.DATABASE_OK);
		doneBtn = new JButton("Cancel");
		doneBtn.addActionListener(this);
		btnPnl.add(submitBtn,BorderLayout.WEST);
		btnPnl.add(Box.createRigidArea(new Dimension(10, 0)));
		btnPnl.add(doneBtn,BorderLayout.EAST);
		// Add the subpanels
		mainPnl.add(optionsPnl,BorderLayout.CENTER);
		mainPnl.add(btnPnl,BorderLayout.SOUTH);
	}
	
	private JComboBox setupTypeCB(JComboBox box) {
		JComboBox logLevelCB = new JComboBox(LogTypeHelper.getAllTypesDescriptions());
        
        // Build the renderer for the combo boxex
        LogTypeRenderer rendererCB = new LogTypeRenderer();
        
        logLevelCB.setEditable(false);
        logLevelCB.setMaximumRowCount(LogTypeHelper.getNumberOfTypes());
        logLevelCB.setRenderer(rendererCB);
		return logLevelCB;
	}
	
	/**
	 * Submit a query to the archive and insert the logs in the main window
	 *
	 */
	private void submitQuery() {
		if (!checkFields()) {
			JOptionPane.showMessageDialog(this,"Error getting values from the form","Input error!",JOptionPane.ERROR_MESSAGE);
			submitBtn.setEnabled(true);
			return;
		}
		System.out.println("Submitting a query");
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
		
		short minType = (short)minLogLevelCB.getSelectedIndex();
		short maxType = (short)maxLogLevelCB.getSelectedIndex();
		
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
		try {
			logs = archive.getLogs(from.toString()+".000",to.toString()+".000",minType,maxType,routine,source,process,maxRows);
		} catch (Throwable t) {
			System.out.println("Error executing the query: "+t.getMessage());
			t.printStackTrace(System.err);
			JOptionPane.showMessageDialog(this,"Error executing the query:\n"+t.getMessage(),"Database error!",JOptionPane.ERROR_MESSAGE);
		}
		if (logs!=null) {
			System.out.println("Num. of logs read from DB: "+logs.size());
			LogsPublisher flusher = new LogsPublisher(logs,parser,submitBtn);
			flusher.start();
			logs=null;
		}
	}
	
	/**
	 * Check the fields in the GUI before executing a query.
	 * It makes only some checks...
	 * 
	 * @return true if the vaules in the fields are ok
	 */
	private boolean checkFields() {
		boolean ret = 
			Pattern.matches("[0-9]+",fromYY.getText()) &&
			Pattern.matches("[0-9]+",fromMM.getText()) &&
			Pattern.matches("[0-9]+",fromMM.getText()) &&
			Pattern.matches("[0-9]+",fromHr.getText()) &&
			Pattern.matches("[0-9]+",fromMin.getText()) &&
			Pattern.matches("[0-9]+",fromSec.getText()) &&
			Pattern.matches("[0-9]+",toYY.getText()) &&
			Pattern.matches("[0-9]+",toMM.getText()) &&
			Pattern.matches("[0-9]+",toDD.getText()) &&
			Pattern.matches("[0-9]+",toHr.getText()) &&
			Pattern.matches("[0-9]+",toMin.getText()) &&
			Pattern.matches("[0-9]+",toSec.getText()) &&
			Pattern.matches("[0-9]+",rowLimit.getText());
			
		int fromY, fromD,fromM, from_h,from_m,from_s;
		int toY, toM, toD, to_h, to_m, to_s;
		fromY = Integer.parseInt(fromYY.getText());
		fromM= Integer.parseInt(fromMM.getText());
		fromD=Integer.parseInt(fromDD.getText());
		from_h=Integer.parseInt(fromHr.getText());
		from_m=Integer.parseInt(fromMin.getText());
		from_s=Integer.parseInt(fromSec.getText());
		toY=Integer.parseInt(toYY.getText());
		toM=Integer.parseInt(toMM.getText());
		toD=Integer.parseInt(toDD.getText());
		to_h=Integer.parseInt(toHr.getText());
		to_m=Integer.parseInt(toMin.getText());
		to_s=Integer.parseInt(toSec.getText());
		
		ret = ret && fromY>=2000 && fromY<2100;
		ret = ret && toY>=2000 && toY<2100;
		ret = ret && fromM>=1 && fromM<=12;
		ret = ret && toM>=1 && toM<=12;
		ret = ret && fromD>=1 && fromD<=31;
		ret = ret && toD>=1 && toD<=31;
		
		ret = ret && from_h>=0 && from_h<24;
		ret = ret && to_h>=0 && to_h<24;
		ret = ret && from_m>=0 && from_m<60;
		ret = ret && to_m>=0 && to_m<60;
		ret = ret && from_s>=0 && from_s<60;
		ret = ret && to_s>=0 && to_s<60;
		
			
        return ret;
	}
	
	
	
}
