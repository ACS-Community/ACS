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

import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.settings.LogTypeRenderer;

/**
 * The toolbar of the main window
 * 
 * @author acaproni
 *
 */
public class LogToolBar extends JToolBar {
	
    /**	
     * The ComboBox in the toolbar and its default value (i.e. the log level
     * at startup
     */
    private JComboBox logLevelCB;
    
    
    /** 
     * The ComboBox with the discard level in the toolbar
     * (the logs with a level lower then what is shown in this ComboBox
     * are discarded when read from the NC)
     */
    private JComboBox discardLevelCB;
    
    
    //  The button to enable/disable (play/pause) the scroll lock
    private JButton pauseBtn;
    private boolean pauseBtnPaused; // The status of the button
    private ImageIcon pauseIcon;
    private ImageIcon playIcon;  
    public static final String pauseStr = "<HTML><FONT size=-2>Pause</FONT>";
    public static final String playStr =  "<HTML><FONT size=-2>Play</FONT>";
    
    /** 
     * The button to delete the logs
     */
    private JButton clearLogsBtn;
    
    /**
     * The button to manage filters
     */
    private JButton filtersBtn;
    
    /**
     * The button for zooming
     */
    private JButton zoomBtn;
    
    /**
     * The initial discard level
     */
    private LogTypeHelper initialDiscardLevel;
    
    /**
     * The initial log level
     */
    private LogTypeHelper initialLogLevel;
    
	/**
	 * Constructor
	 *
	 * @param initialLogLvl The initial log level to set in the CB
	 * @param intialDiscardLvl The initial discard level to set in the COB 
	 */
	public LogToolBar(LogTypeHelper initialLogLvl, LogTypeHelper intialDiscardLvl) {
		super();
		if (initialLogLvl==null) {
			throw new IllegalArgumentException("The initial log level can't be null");
		}
		setName("LogToolBar");
		initialLogLevel=initialLogLvl;
		initialDiscardLevel=intialDiscardLvl;
		setupToolBar();
	}

	/** 
     * Builds the toolbar
     */
    private void setupToolBar() {
        setFloatable(true);
        
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
        tbLevelPanel.add(getClearLogsBtn());
        tbLevelPanel.add(getFiltersBtn());
        tbLevelPanel.add(getZoomBtn());
        userPanel.add(tbLevelPanel);
        
        toolBarPanel.add(userPanel,BorderLayout.WEST);
        
        // Rationalize the sizes ...
        Dimension d = discardLevelCB.getPreferredSize();
        d.height=pauseBtn.getPreferredSize().height;
        discardLevelCB.setPreferredSize(d);
        d= logLevelCB.getPreferredSize();
        d.height=pauseBtn.getPreferredSize().height;
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
    	logLevelCB.addActionListener(listener);
    	discardLevelCB.addActionListener(listener);
    	pauseBtn.addActionListener(listener);
    	filtersBtn.addActionListener(listener);
    	zoomBtn.addActionListener(listener);
    }
    
    /**
     * 
     * @return The discard level CB
     */
    public JComboBox getDiscardLevelCB() {
    	if (discardLevelCB==null) {
    		// Add the ComboBox for the log level
            LogTypeRenderer discardRendererCB = new LogTypeRenderer();
            String[] discardLevelStr = new String[LogTypeHelper.values().length+1];
            discardLevelStr[0] = "None";
            int t=0;
            for (LogTypeHelper logType: LogTypeHelper.values()) {
            	discardLevelStr[++t]=logType.logEntryType;
            }
    		discardLevelCB = new JComboBox(discardLevelStr);
    		discardLevelCB.setMaximumRowCount(discardLevelStr.length);
    		if (initialDiscardLevel==null) {
    			discardLevelCB.setSelectedIndex(0);
    		} else {
    			discardLevelCB.setSelectedIndex(initialDiscardLevel.ordinal()+1);
    		}
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
    		LogTypeHelper[] types = LogTypeHelper.values();
    		int t=0;
//    		for (LogTypeHelper logType: LogTypeHelper.values()) {
//    			Descriptions[t++]=logType.logEntryType;
//            }
	        logLevelCB = new JComboBox(types);
	        
	        // Build the renderer for the combo boxes
	        LogTypeRenderer rendererCB = new LogTypeRenderer();
	        
	        logLevelCB.setSelectedItem(initialLogLevel);
	        logLevelCB.setEditable(false);
	        logLevelCB.setMaximumRowCount(LogTypeHelper.values().length);
	        logLevelCB.setRenderer(rendererCB);
    	}
    	return logLevelCB;
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
     * 
     * @return The filter button
     */
    public JButton getFiltersBtn() {
    	if (filtersBtn==null) {
    		ImageIcon filterIcon = new ImageIcon(LogTypeHelper.class.getResource("/filters.png"));
    		filtersBtn=new JButton("<HTML><FONT size=-2>Filters</HTML>",filterIcon);
    	}
    	return filtersBtn;
    }
    
    /**
     * 
     * @return The zoom button
     */
    public JButton getZoomBtn() {
    	if (zoomBtn==null) {
    		ImageIcon zoomIcon = new ImageIcon(LogTypeHelper.class.getResource("/zoom.png"));
    		zoomBtn=new JButton("<HTML><FONT size=-2>Drill down</HTML>",zoomIcon);
    	}
    	return zoomBtn;
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
	 * Enable/Disable all the control in the GUI than can cause
	 * the invalidation of the logs
	 * 
	 * @param enabled If true the controls are enabled
	 */
	@Override
	public void setEnabled(boolean enabled) {
		getLogLevelCB().setEnabled(enabled);
		pauseBtn.setEnabled(enabled);
		clearLogsBtn.setEnabled(enabled);
		filtersBtn.setEnabled(enabled);
		getDiscardLevelCB().setEnabled(enabled);
		super.setEnabled(enabled);
	}
	
	/**
	 * 
	 * @return true if the play/pause button is pressed
	 */
	public boolean isPaused() {
		return pauseBtnPaused;
	}
	
	/**
	 * Set the state of the zoom button
	 * 
	 * @param zoomable If <code>true</code> the zoom button is enabled.
	 */
	public void setZoomable(boolean zoomable) {
		zoomBtn.setEnabled(zoomable);
	}
	
}
