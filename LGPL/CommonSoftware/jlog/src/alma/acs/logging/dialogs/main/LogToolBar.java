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
package alma.acs.logging.dialogs.main;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToolBar;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.settings.LogTypeRenderer;

/**
 * The toolbar of the main window
 * 
 * @author acaproni
 *
 */
public class LogToolBar extends JToolBar {
	
    //	The ComboBox in the toolbar and its default value (i.e. the log level
    // at startup
    private JComboBox logLevelCB;
    public final int DEFAULT_LOGLEVEL = LogTypeHelper.ENTRYTYPE_INFO;
    
    // The ComboBox with the discard level in the toolbar
    // (the logs with a level lower then what is shown in this ComboBox
    // are discarded when read from the NC)
    private JComboBox discardLevelCB;
    public final int DEFAULT_DISCARDLEVEL = LogTypeHelper.ENTRYTYPE_DEBUG;
    
//  The button to enable/disable (play/pause) the scroll lock
    private JButton pauseBtn;
    private boolean pauseBtnPaused; // The status of the button
    private ImageIcon pauseIcon;
    private ImageIcon playIcon;  
    public static final String pauseStr = "<HTML><FONT size=-2>Pause</FONT>";
    public static final String playStr =  "<HTML><FONT size=-2>Play</FONT>";
    
    // The button to delete the logs
    private JButton clearLogsBtn;
    
    // The search button in the toolbar
    private JButton searchBtn;
    
	/**
	 * Empty constructor
	 *
	 */
	public LogToolBar() {
		super();
		setName("LogToolBar");
		setupToolBar();
	}
	
	

	/** 
     * Builds the toolbar
     */
    private void setupToolBar() {
        setFloatable(false);
        
        // The panel for the toolbar
        JPanel toolBarPanel = new JPanel();
        toolBarPanel.setLayout(new BorderLayout());
        
        // userPanel conatins the Panel with buttons, combo boxes and so on
        JPanel userPanel = new JPanel();
        FlowLayout tbFlowL = new FlowLayout(FlowLayout.LEFT);
        tbFlowL.setHgap(10);
        userPanel.setLayout(tbFlowL);
        
        // Add the label for the log level
        FlowLayout lyLevel = new FlowLayout();
        lyLevel.setHgap(2);
        JPanel tbLevelPanel = new JPanel(lyLevel);
        JLabel logLevelLbl = new JLabel("<HTML><FONT size=-2>Log level: </FONT></HTML>");
        tbLevelPanel.add(logLevelLbl);
        tbLevelPanel.add(getLogLevelCB());
        
        JLabel discardLevelLbl = new JLabel("<HTML><FONT size=-2>Discard level: </FONT></HTML>");
        tbLevelPanel.add(discardLevelLbl);
        tbLevelPanel.add(getDiscardLevelCB());
        tbLevelPanel.add(getPauseBtn());
        tbLevelPanel.add(getSearchBtn());
        tbLevelPanel.add(getClearLogsBtn());
        userPanel.add(tbLevelPanel);
        
        toolBarPanel.add(userPanel,BorderLayout.WEST);
        
        // Rationalize the sizes ...
        Dimension d = discardLevelCB.getPreferredSize();
        d.height=searchBtn.getPreferredSize().height;
        discardLevelCB.setPreferredSize(d);
        d= logLevelCB.getPreferredSize();
        d.height=searchBtn.getPreferredSize().height;
        logLevelCB.setPreferredSize(d);
        
        // Add the toolbar
        add(toolBarPanel);
    }
    
    /**
     * Set the event handler for the widgets in the toolbar
     * 
     * @param listener The action listener
     */
    public void setEventHandler(ActionListener listener) {
    	clearLogsBtn.addActionListener(listener);
    	searchBtn.addActionListener(listener);
    	pauseBtn.addActionListener(listener);
    	logLevelCB.addActionListener(listener);
    }
    
    /**
     * 
     * @return The discrd level CB
     */
    public JComboBox getDiscardLevelCB() {
    	if (discardLevelCB==null) {
    		// Add the ComboBox for the log level
            LogTypeRenderer discardRendererCB = new LogTypeRenderer();
            String[] discardLevelStr = new String[LogTypeHelper.getAllTypesDescriptions().length+1];
            discardLevelStr[0] = "None";
            for (int t=0; t<LogTypeHelper.getAllTypesDescriptions().length; t++) {
            	discardLevelStr[t+1]=LogTypeHelper.getAllTypesDescriptions()[t];
            }
    		discardLevelCB = new JComboBox(discardLevelStr);
    		discardLevelCB.setMaximumRowCount(discardLevelStr.length);
    		discardLevelCB.setSelectedIndex(DEFAULT_DISCARDLEVEL+1);
    		discardLevelCB.setEditable(false);
    		discardLevelCB.setRenderer(discardRendererCB);
    	}
    	return discardLevelCB;
    }
    
    /**
     * 
     * @return The log level CB
     */
    public JComboBox getLogLevelCB() {
    	if (logLevelCB==null) {
	    	// Add the ComboBox for the log level
	        logLevelCB = new JComboBox(LogTypeHelper.getAllTypesDescriptions());
	        
	        // Build the renderer for the combo boxex
	        LogTypeRenderer rendererCB = new LogTypeRenderer();
	        
	        logLevelCB.setSelectedIndex(DEFAULT_LOGLEVEL);
	        logLevelCB.setEditable(false);
	        logLevelCB.setMaximumRowCount(LogTypeHelper.getNumberOfTypes());
	        logLevelCB.setRenderer(rendererCB);
    	}
    	return logLevelCB;
    }
    
    /**
     * 
     * @return The search button
     */
    public JButton getSearchBtn() {
    	if (searchBtn==null) {
    		//  Add the  search button
    		ImageIcon searchIcon=new ImageIcon(LogTypeHelper.class.getResource("/search.png"));
    		searchBtn = new JButton("<HTML><FONT size=-2>Search...</FONT>",searchIcon);
    	}
    	return searchBtn;
    }
    
    /**
     * 
     * @return The button to clear the logs
     */
    public JButton getClearLogsBtn() {
    	if (clearLogsBtn==null) {
	    	// Add the button to delete logs
	        ImageIcon iconClear =new ImageIcon(LogTypeHelper.class.getResource("/delete.png"));
	        clearLogsBtn = new JButton("<HTML><FONT size=-2>Clear logs</FONT>",iconClear);
    	}
    	return clearLogsBtn;
    }
    
    /**
     * 
     * @return The pause button
     */
    public JButton getPauseBtn() {
    	if (pauseBtn==null) {
    		// Add the pause button
	        pauseIcon=new ImageIcon(LogTypeHelper.class.getResource("/pause.png"));
	        playIcon=new ImageIcon(LogTypeHelper.class.getResource("/play.png"));
	        pauseBtn = new JButton(pauseStr,pauseIcon);
	        pauseBtnPaused=false;
    	}
    	return pauseBtn;
    }
    
    /**
	 * The pause has been pressed
	 * Change the test and icon in the button.
	 * 
	 * @return true if the button is in pause
	 */
	public boolean clickPauseBtn() {
		pauseBtnPaused=!pauseBtnPaused;
    	if (pauseBtnPaused) {
    		getPauseBtn().setIcon(playIcon);
    		getPauseBtn().setText(playStr);
    	} else {
    		getPauseBtn().setIcon(pauseIcon);
    		getPauseBtn().setText(pauseStr);
    	}
    	return pauseBtnPaused;
	}
	
	/**
	 * Set the play/pause button in pause mode
	 * 
	 * @return The status of the button previous of the command 
	 *         (true means pasused)
	 */
	public boolean pause() {
		boolean ret=pauseBtnPaused;
		pauseBtnPaused=true;
		getPauseBtn().setIcon(playIcon);
		getPauseBtn().setText(playStr);
		return ret;
	}
	
	/**
	 * Unpause the play/pause button
	 * 
	 * @return The status of the button previous of the command 
	 *         (true means pasused)
	 */
	public boolean unpause() {
		boolean ret=pauseBtnPaused;
		pauseBtnPaused=false;
		getPauseBtn().setIcon(pauseIcon);
		getPauseBtn().setText(pauseStr);
		return ret;
	}
	
	/**
	 * Enable/Disbale all the control in tha GUI than can cause
	 * the invalidation of the logs
	 * 
	 * @param enabled If true the controls are enabled
	 */
	public void setEnabledGUIControls(boolean enabled) {
		getLogLevelCB().setEnabled(enabled);
		getSearchBtn().setEnabled(enabled);
		pauseBtn.setEnabled(enabled);
		clearLogsBtn.setEnabled(enabled);
	}
	
	/**
	 * 
	 * @return true if the play/pause button is pressed
	 */
	public boolean isPaused() {
		return pauseBtnPaused;
	}
	
}
