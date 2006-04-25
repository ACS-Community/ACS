/*
 * Created on Jul 6, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.cosylab.logging.search;

import javax.swing.JDialog;

import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JTextField;
import java.awt.BorderLayout;
import javax.swing.JButton;
import javax.swing.JRadioButton;
import javax.swing.JCheckBox;
import java.awt.GridLayout;
import javax.swing.JOptionPane;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.engine.log.ILogEntry;

/**
 * @author acaproni
 *
 * The dialog to look for strings in the logs
 * The dialog delegates the search to a search engine object
 */
public class SearchDialog extends JDialog {

	private javax.swing.JPanel jContentPane = null;
	private JPanel upperPanel = null;
	private JLabel findLbl = null;
	private JTextField findTF = null;
	private JPanel buttonPanel = null;
	private JButton findBtn = null;
	private JButton advancedBtn = null;
	private JButton doneBtn = null;
	private JPanel centerPanel = null;
	private JPanel optionPanel = null;
	private JPanel directionPanel = null;
	private JRadioButton forwardRB = null;
	private JRadioButton backwardRB = null;
	private JPanel searchTypePanel = null;
	private JCheckBox regExpCB = null;
	private JCheckBox caseSensitiveCB = null;
	private JPanel advancedPanel = null;
	private JPanel advBtnPanel = null;
    
	private JPanel findBtnPanel = null;
	private JPanel doneBtnPanel = null;
    
    // The logging client (main window)
    private LoggingClient loggingClient;
    
    private JCheckBox coulmnToSearchIn[]= new JCheckBox[ILogEntry.NUMBER_OF_FIELDS];
	private JCheckBox wholeWordCB = null;
    
    private SearchEngine searchEngine;
    
	/**
	 * This is the default constructor
	 */
	public SearchDialog(LoggingClient mainWin) {
		super();
        this.loggingClient=mainWin;
		initialize();
        pack();
        // Build the search engine
        searchEngine = new SearchEngine(mainWin.getScrollPaneTable());
	}
	
	/**
	 * Override the Component setVisible method.
	 * This is needed because the dialog is built only once
	 * but the field to show in the advanced panel can be changed
	 * so we need to refresh them before displaying the panel.
	 * 
	 * @see Component.setVisible(boolean)
	 */
	public void setVisible(boolean b) {
		initializeAdvancedPanel();
		super.setVisible(b);
	}
	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		this.setDefaultCloseOperation(javax.swing.WindowConstants.HIDE_ON_CLOSE);
		this.setBounds(20, 35, 300, 200);
		this.setTitle("Find...");
		this.setContentPane(getJContentPane());
        initializeAdvancedPanel();
	}
    
    /**
     * Initialize the advanced panel adding a check box for
     * each column of the main window
     * If a column is not visible in the main window then the checkbox
     * is disabled and unchecked
     */
    private void initializeAdvancedPanel() {
    	if (!(advancedPanel.getLayout() instanceof GridLayout)) {
    		advancedPanel.setLayout(new GridLayout(ILogEntry.NUMBER_OF_FIELDS,1,5,3));
    	}
        boolean visibeColsInMainWindow[]=loggingClient.getScrollPaneTable().getVisibleColumns(true);
        for (int t=0; t<ILogEntry.NUMBER_OF_FIELDS; t++) {
        	if (coulmnToSearchIn[t]==null) {
        		coulmnToSearchIn[t]=new JCheckBox(ILogEntry.fieldNames[t]);
        	}
            advancedPanel.add(coulmnToSearchIn[t]);
            coulmnToSearchIn[t].setVisible(true);
            coulmnToSearchIn[t].setEnabled(visibeColsInMainWindow[t]);
            coulmnToSearchIn[t].setSelected(visibeColsInMainWindow[t]);
            if (visibeColsInMainWindow[t]) {
                coulmnToSearchIn[t].setToolTipText("Search in column "+ILogEntry.fieldNames[t]);
            } else {
                coulmnToSearchIn[t].setToolTipText("This column is not visible in the main window");
            }
        }
    }
	/**
	 * This method initializes jContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private javax.swing.JPanel getJContentPane() {
		if(jContentPane == null) {
			BorderLayout borderLayout4 = new BorderLayout();
			jContentPane = new javax.swing.JPanel();
			jContentPane.setLayout(borderLayout4);
			borderLayout4.setVgap(5);
			borderLayout4.setHgap(5);
			jContentPane.add(getUpperPanel(), java.awt.BorderLayout.NORTH);
			jContentPane.add(getButtonPanel(), java.awt.BorderLayout.SOUTH);
			jContentPane.add(getCenterPanel(), java.awt.BorderLayout.CENTER);
		}
		return jContentPane;
	}
	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */    
	private JPanel getUpperPanel() {
		if (upperPanel == null) {
			findLbl = new JLabel();
			BorderLayout borderLayout1 = new BorderLayout();
			upperPanel = new JPanel();
            upperPanel.setLayout(borderLayout1);
			findLbl.setText("Find:");
			borderLayout1.setHgap(5);
			borderLayout1.setVgap(5);
			upperPanel.add(findLbl, java.awt.BorderLayout.WEST);
			upperPanel.add(getFindTF(), java.awt.BorderLayout.CENTER);
		}
		return upperPanel;
	}
	/**
	 * This method initializes jTextField	
	 * 	
	 * @return javax.swing.JTextField	
	 */    
	private JTextField getFindTF() {
		if (findTF == null) {
			findTF = new JTextField();
			findTF.setToolTipText("Insert here the string to find");
		}
		return findTF;
	}
	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */    
	private JPanel getButtonPanel() {
		if (buttonPanel == null) {
			buttonPanel = new JPanel();
			buttonPanel.setLayout(new BorderLayout());
			buttonPanel.add(getAdvBtnPanel(), java.awt.BorderLayout.CENTER);
			buttonPanel.add(getFindBtnPanel(), java.awt.BorderLayout.WEST);
			buttonPanel.add(getDoneBtnPanel(), java.awt.BorderLayout.EAST);
		}
		return buttonPanel;
	}
	/**
	 * This method initializes jButton	
	 * 	
	 * @return javax.swing.JButton	
	 */    
	private JButton getFindBtn() {
		if (findBtn == null) {
			findBtn = new JButton();
			findBtn.setText("Find");
			findBtn.setToolTipText("Find the string");
			findBtn.addActionListener(new java.awt.event.ActionListener() { 
				public void actionPerformed(java.awt.event.ActionEvent e) {    
					search();
				}
			});
		}
		return findBtn;
	}
	/**
	 * This method initializes jButton	
	 * 	
	 * @return javax.swing.JButton	
	 */    
	private JButton getAdvancedBtn() {
		if (advancedBtn == null) {
			advancedBtn = new JButton();
			advancedBtn.setText("Advanced >>");
			advancedBtn.setToolTipText("Switch to advanced/standard mode");
			advancedBtn.addActionListener(new java.awt.event.ActionListener() { 
				public void actionPerformed(java.awt.event.ActionEvent e) {    
					if (advancedBtn.getText().compareTo("Advanced >>")==0) {
					    advancedBtn.setText("Standard <<");
                        advancedPanel.setVisible(true);
                        pack();
                    } else {
                        advancedBtn.setText("Advanced >>");
                        advancedPanel.setVisible(false);
                        pack();
                    }
				}
			});
		}
		return advancedBtn;
	}
	/**
	 * This method initializes jButton	
	 * 	
	 * @return javax.swing.JButton	
	 */    
	private JButton getDoneBtn() {
		if (doneBtn == null) {
			doneBtn = new JButton();
			doneBtn.setText("Done");
			doneBtn.setToolTipText("Done");
			doneBtn.addActionListener(new java.awt.event.ActionListener() { 
				public void actionPerformed(java.awt.event.ActionEvent e) {    
					setVisible(false);
				}
			});
		}
		return doneBtn;
	}
	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */    
	private JPanel getCenterPanel() {
		if (centerPanel == null) {
			centerPanel = new JPanel();
			centerPanel.setLayout(new BorderLayout());
			centerPanel.add(getOptionPanel(), java.awt.BorderLayout.NORTH);
		}
		return centerPanel;
	}
	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */    
	private JPanel getOptionPanel() {
		if (optionPanel == null) {
			optionPanel = new JPanel();
			optionPanel.setLayout(new BorderLayout());
			optionPanel.add(getDirectionPanel(), java.awt.BorderLayout.WEST);
			optionPanel.add(getSearchTypePanel(), java.awt.BorderLayout.EAST);
			optionPanel.add(getAdvancedPanel(), java.awt.BorderLayout.SOUTH);
		}
		return optionPanel;
	}
	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */    
	private JPanel getDirectionPanel() {
		if (directionPanel == null) {
			GridLayout gridLayout2 = new GridLayout();
			directionPanel = new JPanel();
			directionPanel.setLayout(gridLayout2);
			directionPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Direction", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, null, null));
			directionPanel.setComponentOrientation(java.awt.ComponentOrientation.LEFT_TO_RIGHT);
			gridLayout2.setRows(3);
			gridLayout2.setColumns(1);
			directionPanel.add(getForwardRB(), null);
			directionPanel.add(getBackwardRB(), null);
		}
		return directionPanel;
	}
	/**
	 * This method initializes jRadioButton	
	 * 	
	 * @return javax.swing.JRadioButton	
	 */    
	private JRadioButton getForwardRB() {
		if (forwardRB == null) {
			forwardRB = new JRadioButton();
			forwardRB.setText("Forward");
			forwardRB.setSelected(true);
			forwardRB.setToolTipText("Search forward");
			forwardRB.addActionListener(new java.awt.event.ActionListener() { 
				public void actionPerformed(java.awt.event.ActionEvent e) {    
					backwardRB.setSelected(false);
				}
			});
		}
		return forwardRB;
	}
	/**
	 * This method initializes jRadioButton	
	 * 	
	 * @return javax.swing.JRadioButton	
	 */    
	private JRadioButton getBackwardRB() {
		if (backwardRB == null) {
			backwardRB = new JRadioButton();
			backwardRB.setText("Backward");
			backwardRB.setToolTipText("Search backward");
			backwardRB.addActionListener(new java.awt.event.ActionListener() { 
				public void actionPerformed(java.awt.event.ActionEvent e) {    
					forwardRB.setSelected(false);
				}
			});
		}
		return backwardRB;
	}
	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */    
	private JPanel getSearchTypePanel() {
		if (searchTypePanel == null) {
			GridLayout gridLayout3 = new GridLayout();
			searchTypePanel = new JPanel();
			searchTypePanel.setLayout(gridLayout3);
			searchTypePanel.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Options", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, null, null));
			gridLayout3.setRows(3);
			gridLayout3.setColumns(1);
			searchTypePanel.add(getRegExpCB(), null);
			searchTypePanel.add(getCaseSensitiveCB(), null);
			searchTypePanel.add(getWholeWordCB(), null);
		}
		return searchTypePanel;
	}
	/**
	 * This method initializes jCheckBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */    
	private JCheckBox getRegExpCB() {
		if (regExpCB == null) {
			regExpCB = new JCheckBox();
			regExpCB.setText("Regular expression");
			regExpCB.setToolTipText("Search a regular expression");
			regExpCB.addItemListener(new java.awt.event.ItemListener() { 
				public void itemStateChanged(java.awt.event.ItemEvent e) {    
                    wholeWordCB.setEnabled(!regExpCB.isSelected());
				}
			});
		}
		return regExpCB;
	}
	/**
	 * This method initializes jCheckBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */    
	private JCheckBox getCaseSensitiveCB() {
		if (caseSensitiveCB == null) {
			caseSensitiveCB = new JCheckBox();
			caseSensitiveCB.setText("Case sensitive");
			caseSensitiveCB.setToolTipText("Case sensitive");
		}
		return caseSensitiveCB;
	}
	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */    
	private JPanel getAdvancedPanel() {
		if (advancedPanel == null) {
			advancedPanel = new JPanel();
			advancedPanel.setVisible(false);
			advancedPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Search in these columns:", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, null, null));
		}
		return advancedPanel;
	}
	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */    
	private JPanel getAdvBtnPanel() {
		if (advBtnPanel == null) {
			advBtnPanel = new JPanel();
			advBtnPanel.add(getAdvancedBtn(), null);
		}
		return advBtnPanel;
	}
	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */    
	private JPanel getFindBtnPanel() {
		if (findBtnPanel == null) {
			findBtnPanel = new JPanel();
			findBtnPanel.add(getFindBtn(), null);
		}
		return findBtnPanel;
	}
	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */    
	private JPanel getDoneBtnPanel() {
		if (doneBtnPanel == null) {
			doneBtnPanel = new JPanel();
			doneBtnPanel.add(getDoneBtn(), null);
		}
		return doneBtnPanel;
	}
	/**
	 * This method initializes jCheckBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */    
	private JCheckBox getWholeWordCB() {
		if (wholeWordCB == null) {
			wholeWordCB = new JCheckBox();
			wholeWordCB.setText("Whole Word");
			wholeWordCB.setToolTipText("Whole Word");
		}
		return wholeWordCB;
	}
    
    /**
     * Search the string/regular expriossion delegating the SearchEngine
     *
     */
    public void search() {
        // Check if there is something to look for before starting 
        // the engine
        if (loggingClient.getScrollPaneTable().getRowCount()<=0 || findTF.getText().length()<=0) {
            JOptionPane.showMessageDialog(
                    this,
                    "Invalid search parameters or no logs in the main window",
                    "Search error",
                    JOptionPane.ERROR_MESSAGE);
            loggingClient.enableSearchNext(false);
            return;
        }
        
        // Enable the search next menu item of the main window
        loggingClient.enableSearchNext(true);
        
        // Build the vector of the column where to look into
        boolean[] cols = new boolean[coulmnToSearchIn.length];
        for (int t=0; t<coulmnToSearchIn.length; t++) {
            cols[t]=coulmnToSearchIn[t].isSelected();
        }
        int row=-1;
        if (!regExpCB.isSelected()) {
            // Standard string search
            row = searchEngine.find(
                    findTF.getText(),
                    caseSensitiveCB.isSelected(),
                    wholeWordCB.isSelected(),
                    forwardRB.isSelected(),
                    cols);
        } else {
            // Regular expression search
            int flags = (caseSensitiveCB.isSelected())?0:Pattern.CASE_INSENSITIVE;
            try {
                Pattern regexp = Pattern.compile(findTF.getText(),flags);
                row = searchEngine.find(regexp,forwardRB.isSelected(),cols);
            } catch (PatternSyntaxException pse) {
                System.err.println("Exception compiling the regular expression "+pse.getMessage());
                JOptionPane.showMessageDialog(
                        this,
                        findTF.getText()+" is not a valid regular expression",
                        "Error compiling the regular expression",
                        JOptionPane.ERROR_MESSAGE);
                loggingClient.enableSearchNext(false);
                return;
            }
        }
        if (row>-1) {
            loggingClient.getScrollPaneTable().changeSelection(row,1,false,false);
            loggingClient.getScrollPaneTable().showColumn(row);
        } else {
        	String msg = "<html>No log matching \"<I>"+findTF.getText()+"</I>\" found<BR>Search from ";
        	if (forwardRB.isSelected()) {
        		msg += "beginning";
        	} else {
        		msg += "end";
        	}
        	msg+="?";
            // Now log found: shows a message
            int ret = JOptionPane.showConfirmDialog(
                this,
                msg,
                "No log found",
                JOptionPane.YES_NO_CANCEL_OPTION);
            switch (ret) {
	            case JOptionPane.YES_OPTION: {
	            	if (forwardRB.isSelected()) {
	            		row = 0;
	            	} else {
	            		row = loggingClient.getScrollPaneTable().getRowCount()-1;
	            	}
	            	loggingClient.getScrollPaneTable().changeSelection(row,1,false,false);
	            	search();
	            	return;
	            }
	            case JOptionPane.NO_OPTION: {
	            	return;
	            }
	            case JOptionPane.CANCEL_OPTION: {
	            	setVisible(false);
	            	return;
	            }
            	default: {
            		return;
            	}
            }
        }
    }
}
