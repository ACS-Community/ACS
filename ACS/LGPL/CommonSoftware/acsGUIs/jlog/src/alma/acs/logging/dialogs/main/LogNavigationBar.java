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

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JToolBar;

import alma.acs.logging.table.LogEntryTable;

import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * The toolbar with the controls to navigate through the logs in the table.
 *  
 * @author acaproni
 *
 */
public class LogNavigationBar extends JToolBar implements ActionListener {
	
	/**
	 * The button to jump at the beginning of the document
	 */
	private JButton beginBtn;
	
	/**
	 * The button to jump at the beginning of the document
	 */
	private JButton endBtn;
	
	/**
	 * The button to jump to the previous row
	 */
	private JButton prevBtn;
	
	/**
	 * The button to jump to the next row
	 */
	private JButton nextBtn;
	
	/**
	 * The button to jump to the selected row
	 */
	private JButton selectedBtn;
	
	/**
	 * The search button in the toolbar
	 */ 
    private JButton searchBtn;
    
    /**
     * The table of logs
     */
    private LogEntryTable table;
	
	/**
	 * Constructor
	 */
	public LogNavigationBar(LogEntryTable table) {
		super();
		if (table==null) {
			throw new IllegalArgumentException("The table can't be null");
		}
		initialize();
		this.table=table;
	}
	
	/**
	 * Initialize the GUI
	 */
	private void initialize() {
		setFloatable(true);
		
		// The panel for the toolbar
        JPanel toolBarPanel = new JPanel();
        toolBarPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        
        beginBtn= new JButton(new ImageIcon(this.getClass().getResource("/begin.png")));
        beginBtn.setToolTipText("To beginning");
		toolBarPanel.add(beginBtn);
		
		prevBtn= new JButton(new ImageIcon(this.getClass().getResource("/prev.png")));
		prevBtn.setToolTipText("To prev");
		toolBarPanel.add(prevBtn);
		
		nextBtn= new JButton(new ImageIcon(this.getClass().getResource("/next.png")));
		nextBtn.setToolTipText("To next");
		toolBarPanel.add(nextBtn);
		
		endBtn= new JButton(new ImageIcon(this.getClass().getResource("/end.png")));
		endBtn.setToolTipText("To end");
		toolBarPanel.add(endBtn);
		
		toolBarPanel.add(new JToolBar.Separator());
		
		selectedBtn= new JButton(new ImageIcon(this.getClass().getResource("/selected.png")));
		selectedBtn.setToolTipText("To selected");
		toolBarPanel.add(selectedBtn);
		
		ImageIcon searchIcon=new ImageIcon(LogTypeHelper.class.getResource("/search.png"));
   		searchBtn = new JButton("<HTML><FONT size=-2>Search...</FONT>",searchIcon);
   		searchBtn.setToolTipText("Search logs");
   		toolBarPanel.add(searchBtn);
   		
		add(toolBarPanel);
		// Set the event handler for the widgets in the toolbar
		beginBtn.addActionListener(this);
		prevBtn.addActionListener(this);
		nextBtn.addActionListener(this);
		endBtn.addActionListener(this);
		selectedBtn.addActionListener(this);
	}
	
	/**
     * Set the event handler for the search button
     * 
     * @param listener The action listener
     */
    public void setEventHandler(ActionListener listener) {
    	searchBtn.addActionListener(listener);
    }
   
   /**
	 * Enable/Disable all the control in the GUI than can cause
	 * the invalidation of the logs
	 * 
	 * @param enabled If true the controls are enabled
	 */
    @Override
	public void setEnabled(boolean enabled) {
		beginBtn.setEnabled(enabled);
		prevBtn.setEnabled(enabled);
		nextBtn.setEnabled(enabled);
		endBtn.setEnabled(enabled);
		selectedBtn.setEnabled(enabled);
		searchBtn.setEnabled(enabled);
		super.setEnabled(enabled);
	}
   
   /**
    * 
    * @return The search button
    */
   public JButton getSearchBtn() {
   	return searchBtn;
   }

	/**
	 * @return the beginBtn
	 */
	public JButton getBeginBtn() {
		return beginBtn;
	}

	/**
	 * @return the endBtn
	 */
	public JButton getEndBtn() {
		return endBtn;
	}

	/**
	 * @return the prevBtn
	 */
	public JButton getPrevBtn() {
		return prevBtn;
	}

	/**
	 * @return the nextBtn
	 */
	public JButton getNextBtn() {
		return nextBtn;
	}

	/**
	 * @return the selectedBtn
	 */
	public JButton getSelectedBtn() {
		return selectedBtn;
}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==prevBtn) {
			table.scrollToPrevSelectedRow();
		} else if (e.getSource()==nextBtn) {
			table.scrollToNextSelectedRow();
		} else if (e.getSource()==beginBtn) {
			table.scrollToFirstRow();
		} else if (e.getSource()==endBtn) {
			table.scrollToLastRow();
		} else if (e.getSource()==selectedBtn) {
			table.scrollToSelectedRow();
		}
	}
	
}
