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
package alma.acs.logging.table;

import java.awt.Component;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.swing.DefaultListSelectionModel;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.Field;
import com.cosylab.logging.settings.UserInfoDlg;

/**
 * The JPopupMenu displayed when the user presses the right mouse button
 * over a row of the table
 * 
 * @author acaproni
 *
 */
public class TablePopupMenu extends JPopupMenu implements ActionListener {
	
	/**
	 * The class to set the clipboard content 
	 *  
	 * @author acaproni
	 *
	 */
	public final class TextTransfer implements ClipboardOwner {
		/**
	   * Empty implementation of the ClipboardOwner interface.
	   */
	   public void lostOwnership( Clipboard aClipboard, Transferable aContents) {
	     //do nothing
	   }
	   
	   /**
	    * Place a String on the clipboard, and make this class the
	    * owner of the Clipboard's contents.
	    */
	    public void setClipboardContents( String str){
	      StringSelection stringSelection = new StringSelection(str);
	      Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
	      try {
	      	clipboard.setContents( stringSelection, stringSelection );
	      } catch (IllegalStateException e) {
	      	// This exception may be returned in some cases
	      	// It is a temporary situation: we do nothing here
	      	// and the user will retry again or most likely 
	      	// submit an SPR ;-)
	      	e.printStackTrace();
	      }
	      /* Check if the clipboards contains the right string
	      String readStr=null;
	      try {
	      	readStr = (String)(clipboard.getContents(this).getTransferData(DataFlavor.stringFlavor));
	      } catch (Exception e) {
	      	e.printStackTrace();
	      }
	      System.out.println("Write ["+str+"]");
	      System.out.println("Write ["+readStr+"]");
	      System.out.println("Comparison="+str.compareTo(readStr));*/
	    }
	    
	    /**
	     * Get the String residing on the clipboard.
	     *
	     * @return any text found on the Clipboard; if none found, return an
	     * empty String.
	     */
	     private String getClipboardContents() {
	       String result = "";
	       Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
	       //odd: the Object param of getContents is not currently used
	       Transferable contents = clipboard.getContents(null);
	       boolean hasTransferableText = (contents != null) &&
		   		contents.isDataFlavorSupported(DataFlavor.stringFlavor);
	       if (hasTransferableText) {
	         try {
	           result = (String)contents.getTransferData(DataFlavor.stringFlavor);
	         }
	         catch (UnsupportedFlavorException ex){
	           //highly unlikely since we are using a standard DataFlavor
	           System.out.println(ex);
	         }
	         catch (IOException ex) {
	           System.out.println(ex);
	         }
	       }
	       return result;
	     }
	}
	
	/**
	 * The menu item to show the error stack.
	 * <P>
	 * It is enabled only if the stack ID is not null
	 */
	private JMenuItem showErrorStack = new JMenuItem("Show error stack");
	
	/** 
	 * The menu item to copy the text to the clipboard
	 */
	private JMenuItem copyClipboard = new JMenuItem("to clipboard");
	
	/** 
	 * The menu item to allow the user to add his information
	 */
	private JMenuItem addUserInfo = new JMenuItem("add Info");
	
	/** 
	 * The icon of the menu item to save selected logs
	 */
	private ImageIcon saveIcon =new ImageIcon(LogTypeHelper.class.getResource("/disk.png"));
	
	/**
	 * The menu item to save the selected logs
	 */
	private JMenuItem saveSelected = new JMenuItem("Save selected logs...",saveIcon);
	
	/** 
	 * The icon of the menu item to save selected logs
	 */
	private ImageIcon zoomIcon =new ImageIcon(LogTypeHelper.class.getResource("/zoom.png"));
	
	/**
	 * The menu item to save the selected logs
	 */
	private JMenuItem zoomOverSelected = new JMenuItem("Drill down",zoomIcon);
	
	private LogTableRowSorter sorter;
	
	/** 
	 * The text to copy
	 */
	private String textToCopy;
	
	/** 
	 * The row and column under the mouse pointer
	 */
	private int row;
	
	/**
	 * This property is used to select the logs for the error browser.
	 * Its value is initialized in the constructor by reading the <code>STACKID</code> of the selected log.
	 */
	private String stackId;
	
	/**
	 * The <code>LoggingClient</code>
	 */
	private final LoggingClient loggingClient;
	
	/**
	 * The table
	 */
	private final LogEntryTable table;
	
	/**
	 * The model
	 */
	private final LogTableDataModel model;
	
	/**
	 * The selection model
	 */
	private final DefaultListSelectionModel selectionModel;
	
	/**
	 * The helper for the clipboard
	 */
	private final TextTransfer textTransfer=new TextTransfer();
	
	/**
	 * Constructor
	 * 
	 * @param row The row below the mouse pointer
	 * @param col The col below he pointer
	 * @param text The text below the pointer
	 * @param logCli The <code>LoggingClient</code>
	 */
	public TablePopupMenu(LoggingClient logCli,LogEntryTable table)
	{
		this.loggingClient=logCli;
		this.table=table;
		this.model=table.getLCModel();
		this.selectionModel=(DefaultListSelectionModel)table.getSelectionModel();
		this.sorter=(LogTableRowSorter)table.getRowSorter();
		
		// Add the Label
		setLabel("Paste");
		
		// Add the menu items
		add(zoomOverSelected);
		zoomOverSelected.addActionListener(this);
		addSeparator();
		add(showErrorStack);
		addSeparator();
		add(saveSelected);
		addSeparator();
		add(copyClipboard);
		add(addUserInfo);
		
		// Add the listeners
		showErrorStack.addActionListener(this);
		copyClipboard.addActionListener(this);
		addUserInfo.addActionListener(this);
		saveSelected.addActionListener(this);
	}
	
	/**
	 * Handle the events from the menu
	 * 
	 * @param e The event
	 */
	public void actionPerformed(ActionEvent e) {
		if (textToCopy==null) {
			return;
		}
		if (e.getSource()==copyClipboard) {
			// Build the text to copy in the clipboard
			if (!selectionModel.isSelectionEmpty()) {
				StringBuffer strBuffer = new StringBuffer();
				for (int i=selectionModel.getMinSelectionIndex(); 
					i<=selectionModel.getMaxSelectionIndex(); i++) {
					if (!selectionModel.isSelectedIndex(i)) {
						continue;
					} else {
						ILogEntry log = model.getVisibleLogEntry(table.convertRowIndexToModel(i));
						strBuffer.append(log.toXMLString());
						strBuffer.append("\n");
					}
				}
				// Copy the text to the clipboard
				textTransfer.setClipboardContents(strBuffer.toString());
			} 
		} else if (e.getSource()==addUserInfo) {
			System.out.println("AAAAAAAAAAAAAA");
			// Show the dialog
			UserInfoDlg dlg = new UserInfoDlg();
			if (dlg.okPressed()) {
				String name =  dlg.getInfoName();
				String value = dlg.getInfo();
				// Replace some chars potentially dangerous for xml
				value = value.replaceAll("&","&amp;");
				value = value.replaceAll("'","&apos;");
				value = value.replaceAll("`","&apos;");
				value = value.replaceAll("\"","&quot;");
				value = value.replaceAll("<","&lt;");
				value = value.replaceAll(">","&gt;");
				name = name.replaceAll("&","&amp;");
				name = name.replaceAll("'","&apos;");
				name = name.replaceAll("`","&apos;");
				name = name.replaceAll("\"","&quot;");
				name = name.replaceAll("<","&lt;");
				name = name.replaceAll(">","&gt;");
				
				ILogEntry logEntry = model.getVisibleLogEntry(table.convertRowIndexToModel(row));
				logEntry.addData(name,value);
				model.replaceLog(table.convertRowIndexToModel(row),logEntry);
				
				LogTableDataModel tableModel = (LogTableDataModel) table.getModel();
				tableModel.fireTableRowsUpdated(table.convertRowIndexToModel(row),table.convertRowIndexToModel(row));
				table.setRowSelectionInterval(row,row);
				
				loggingClient.setLogDetailContent(logEntry);
			}
		} else if (e.getSource()==saveSelected) {
			saveSelectedLogs();
		} else if (e.getSource()==showErrorStack) {
			if (stackId!=null && !stackId.isEmpty()) {
				loggingClient.addErrorTab(stackId);
			}
		} else if (e.getSource()==zoomOverSelected) {
			
			Thread t = new Thread(new Runnable() {
        		public void run() {
        			table.zoom();
        		}
        	});
        	t.setDaemon(true);
        	t.setName("TablePopupMenu.actionPerformed.Zoom");
        	t.start();
		} else {
			System.err.println("Unhandled event "+e);
		}
	}
	
	/**
	 * Save the selected logs into a file
	 *
	 */
	private void saveSelectedLogs() {
		// Build the text to save in the file
		StringBuilder strBuffer = new StringBuilder();
		if (!selectionModel.isSelectionEmpty()) {
			for (int i=selectionModel.getMinSelectionIndex(); 
				i<=selectionModel.getMaxSelectionIndex(); i++) {
				if (!selectionModel.isSelectedIndex(i)) {
					continue;
				} else {
					ILogEntry log = model.getVisibleLogEntry(sorter.convertRowIndexToModel(i));
					strBuffer.append(log.toXMLString());
					strBuffer.append("\n");
				}
			}
			if (strBuffer.length()==0) {
				// Nothing to save
				return;
			}
				
		}
		FileFilter fileFileter = new FileNameExtensionFilter("XML file","xml");
		JFileChooser fc = new JFileChooser();
		fc.addChoosableFileFilter(fileFileter);
		if (fc.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();
			// If not present, add the xml extension
			if (!file.getAbsolutePath().toLowerCase().endsWith(".xml")) {
				file = new File(file.getAbsolutePath()+".xml");
			}
			FileOutputStream outFStream=null;
			try {
				outFStream = new FileOutputStream(file);
			} catch (FileNotFoundException fnfe) {
				JOptionPane.showMessageDialog(null,"Error creating "+file.getAbsolutePath()+":\n"+fnfe.getMessage(),"Error saving logs",JOptionPane.ERROR_MESSAGE);
				return;
			}
			try {
				outFStream.write(strBuffer.toString().getBytes());
				outFStream.flush();
				outFStream.close();
			} catch (IOException ioe) {
				JOptionPane.showMessageDialog(null,"Error saving "+file.getAbsolutePath()+":\n"+ioe.getMessage(),"Error saving logs",JOptionPane.ERROR_MESSAGE);
			}
		}
	}

	/**
	 * Show the popup menu
	 * 
	 * @param invoker the component in whose space the popup menu is to appear
	 * @param x the x coordinate in invoker's coordinate space at which the popup menu is to be displayed
	 * @param y the y coordinate in invoker's coordinate space at which the popup menu is to be displayed
	 * @param row The row below the pointer
	 * @param col The col below the pointer
	 * @param txt The text below the pointer
	 */
	public void show(Component invoker, int x, int y, int row, String txt) {
		this.row=row;
		this.textToCopy=new String(txt);
		
		// Hide the unneeded buttons
		boolean singleLineSelected =
			selectionModel.getMinSelectionIndex() == selectionModel.getMaxSelectionIndex();
		
		// The error stack menu item is enabled only if the stack ID is not empty and not null
		
		if (singleLineSelected) {
			ILogEntry selectedLog = model.getVisibleLogEntry(table.convertRowIndexToModel(row));
			stackId = (String)selectedLog.getField(Field.STACKID);
			showErrorStack.setEnabled(stackId!=null && !stackId.isEmpty());
		} else {
			stackId=null;
			showErrorStack.setEnabled(false);
		}
		
		addUserInfo.setEnabled(singleLineSelected);
		copyClipboard.setEnabled(true);
		saveSelected.setEnabled(true);
		
		// Disable the menu items if the test is null or empty
		if (textToCopy==null || textToCopy.length()==0) {
			copyClipboard.setEnabled(false);
			return;
		}
		// Check if the system clipboard is available
		// and eventually disable the menu item
		SecurityManager sm = System.getSecurityManager();
	    if (sm != null) {
	    	try {
	    		sm.checkSystemClipboardAccess();
	    	} catch (Exception e) {
	    		copyClipboard.setEnabled(false);
	    	}
	    }
		
		// Disable the zoom if only one line is selected
		zoomOverSelected.setEnabled(!singleLineSelected && loggingClient.getZoomManager().isAvailable());
		
		show(invoker, x, y);
	}
	
	
}
