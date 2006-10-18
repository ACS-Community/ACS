
package alma.acs.logging.archive;

import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JTextField;

import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.settings.LogTypeRenderer;

/**
 * A class to setup a query to submit to the DB
 * 
 * @author acaproni
 *
 */
public class QueryDlg extends JDialog implements ActionListener {
	
	private JButton submitBtn;
	private JButton doneBtn;
	
	// The archive
	private ArchiveConnectionManager archive;
	
	// The time limit for the query
	private JTextField from,to;
	
	// The min and max log type
	private JComboBox minLogLevelCB, maxLogLevelCB;
	
	// The process name for the query
	private JTextField procName;
	
	// The max number of log to get from the DB
	private JTextField rowLimit;

	/**
	 * Empty constructor
	 */
	public QueryDlg(ArchiveConnectionManager archiveConn) {
		super();
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
			System.out.println("Submitting a query");
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
		JRootPane mainPnl = this.getRootPane();
		mainPnl.setLayout(new BorderLayout());
		
		// The panel with the option of the query
		JPanel optionsPnl = new JPanel();
		GridBagLayout prefsLayout = new GridBagLayout();
		GridBagConstraints c = new GridBagConstraints();
		optionsPnl.setLayout(prefsLayout);
		// Add all the labels
		JLabel fromLbl = new JLabel("From:");
		c.gridx=0; c.gridy=0; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(fromLbl,c);
		JLabel toLbl = new JLabel("To");
		c.gridx=0; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(toLbl,c);
		JLabel minLogType = new JLabel("From type");
		c.gridx=0; c.gridy=2; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(minLogType,c);
		JLabel maxLogType = new JLabel("To type");
		c.gridx=0; c.gridy=3; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(maxLogType,c);
		JLabel procNameLbl = new JLabel("Process name");
		c.gridx=0; c.gridy=4; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(procNameLbl,c);
		JLabel maxLogs = new JLabel("Max num of logs to load");
		c.gridx=0; c.gridy=5; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(maxLogs,c);
		// Add the input widgets
		from = new JTextField(20);
		c.gridx=1; c.gridy=0; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(from,c);
		to = new JTextField(20);
		c.gridx=1; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(to,c);
		// Build the renderer for the combo boxex
        LogTypeRenderer rendererCB = new LogTypeRenderer();
		minLogLevelCB = new JComboBox(LogTypeHelper.getAllTypesDescriptions());
		minLogLevelCB.setSelectedIndex(LogTypeHelper.ENTRYTYPE_INFO);
		minLogLevelCB.setRenderer(rendererCB);
		minLogLevelCB.setEditable(false);
		minLogLevelCB.setMaximumRowCount(LogTypeHelper.getNumberOfTypes());
		c.gridx=1; c.gridy=2; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(minLogLevelCB,c);
		maxLogLevelCB= new JComboBox(LogTypeHelper.getAllTypesDescriptions());
		maxLogLevelCB.setSelectedIndex(LogTypeHelper.ENTRYTYPE_EMERGENCY);
		maxLogLevelCB.setEditable(false);
		maxLogLevelCB.setRenderer(rendererCB);
		maxLogLevelCB.setMaximumRowCount(LogTypeHelper.getNumberOfTypes());
		c.gridx=1; c.gridy=3; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(maxLogLevelCB,c);
		procName = new JTextField(20);
		c.gridx=1; c.gridy=4; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(procName,c);
		rowLimit = new JTextField(20);
		c.gridx=1; c.gridy=5; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPnl.add(rowLimit,c);
		
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
}
