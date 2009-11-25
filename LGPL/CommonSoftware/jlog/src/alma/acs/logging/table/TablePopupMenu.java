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
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;

import alma.acs.logging.tools.CSVConverter;
import alma.acs.logging.tools.LogConverter;
import alma.acs.logging.tools.TextConverter;
import alma.acs.logging.tools.TwikiTableConverter;
import alma.acs.logging.tools.XMLConverter;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.LogField;
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
	 * The file chooser to see the icon of the given type
	 */
	public class CustomFileChooser extends JFileChooser {
		
		/**
		 * The icon to show for the file type
		 */
		private final ImageIcon icon;
		
		/**
		 * The filter for the type of the file
		 */
		private final FileFilter filter;
		
		/**
		 * Constructor
		 * 
		 * @param icon
		 */
		public CustomFileChooser(ImageIcon icon, FileFilter filter) {
			if (icon==null || filter==null) {
				throw new IllegalArgumentException("Invalid null param");
			}
			this.icon=icon;
			this.filter=filter;
		}
		/* (non-Javadoc)
		 * @see javax.swing.JFileChooser#getIcon(java.io.File)
		 */
		@Override
		public Icon getIcon(File f) {
			if (f.isDirectory()) {
				return super.getIcon(f);
			} else if (filter!=null && filter.accept(f)) {
				return icon;
			} else {
				return super.getIcon(f);
			}
		}
		
	}
	
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
	private final JMenuItem showErrorStack = new JMenuItem("Show error stack");
	
	/** 
	 * The menu item to allow the user to add his information
	 */
	private final JMenuItem addUserInfo = new JMenuItem("add Info");
	
	/** 
	 * The icon of the menu item to save selected logs
	 */
	private final ImageIcon saveIcon =new ImageIcon(LogTypeHelper.class.getResource("/disk.png"));
	
	/**
	 * The sub menu item to save the selected logs
	 */
	private final JMenu saveSelected = new JMenu("Save selected logs...");
	
	/** 
	 * The icon of the menu item to save selected logs as twiki table
	 */
	private final ImageIcon saveXmlIcon =new ImageIcon(LogTypeHelper.class.getResource("/xml-file.png"));
	
	/**
	 * The menu item to save selected logs as XML
	 */
	private final JMenuItem saveSelectedAsXML=new JMenuItem("Save as XML",saveXmlIcon);
	
	/** 
	 * The icon of the menu item to save selected logs as twiki table
	 */
	private final ImageIcon saveTextIcon =new ImageIcon(LogTypeHelper.class.getResource("/text-files.gif"));
	
	/**
	 * The menu item to save selected logs as plain ASCII
	 */
	private final JMenuItem saveSelectedAsText=new JMenuItem("Save as text",saveTextIcon);
	
	/** 
	 * The icon of the menu item to save selected logs as twiki table
	 */
	private final ImageIcon saveTwikiTableIcon =new ImageIcon(LogTypeHelper.class.getResource("/T-twiki.gif"));
	
	/**
	 * The menu item to save selected logs as plain ASCII (twiki table)
	 */
	private final JMenuItem saveSelectedAsTwiki=new JMenuItem("Save as twiki table",saveTwikiTableIcon);
	
	/** 
	 * The icon of the menu item to save selected logs as twiki table
	 */
	private final ImageIcon saveCsvIcon =new ImageIcon(LogTypeHelper.class.getResource("/csv-files.png"));
	
	/**
	 * The menu item to save selected logs as plain ASCII (CSV)
	 */
	private final JMenuItem saveSelectedAsCSV=new JMenuItem("Save as CSV",saveCsvIcon);
	
	/** 
	 * The icon of the menu item to save selected logs
	 */
	private final ImageIcon zoomIcon =new ImageIcon(LogTypeHelper.class.getResource("/zoom.png"));
	
	/**
	 * The menu item to save the selected logs
	 */
	private final JMenuItem zoomOverSelected = new JMenuItem("Drill down",zoomIcon);
	
	private LogTableRowSorter sorter;
	
	/** 
	 * The icon of the clipboard menu item
	 */
	private final ImageIcon clipboardIcon =new ImageIcon(LogTypeHelper.class.getResource("/clipboard.png"));
	
	/** 
	 * The menu to copy the text to the clipboard
	 */
	private final JMenu copyClipboard = new JMenu("to clipboard");
	
	/**
	 * Copy the clipboard as text
	 */
	private JMenuItem copyClipTxt= new JMenuItem("Copy as text",saveTextIcon);
	
	/**
	 * Copy the clipboard as XML
	 */
	private JMenuItem copyClipXml= new JMenuItem("Copy as XML",saveXmlIcon);
	
	/**
	 * Copy the clipboard as twiki table
	 */
	private JMenuItem copyClipTwikiTable= new JMenuItem("Copy as TwikiTable",saveTwikiTableIcon);
	
	/**
	 * Copy the clipboard as CSV
	 */
	private JMenuItem copyClipCSV= new JMenuItem("Copy as CSV",saveCsvIcon);
	
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
		
		saveSelected.setIcon(saveIcon);
		saveSelected.add(saveSelectedAsXML);
		saveSelected.add(saveSelectedAsText);
		saveSelected.add(saveSelectedAsTwiki);
		saveSelected.add(saveSelectedAsCSV);
		
		add(saveSelected);
		addSeparator();
		copyClipboard.setIcon(clipboardIcon);
		copyClipboard.add(copyClipXml);
		copyClipboard.add(copyClipTxt);
		copyClipboard.add(copyClipTwikiTable);
		copyClipboard.add(copyClipCSV);
		
		add(copyClipboard);
		add(addUserInfo);
		
		// Add the listeners
		showErrorStack.addActionListener(this);
		copyClipXml.addActionListener(this);
		copyClipTxt.addActionListener(this);
		copyClipTwikiTable.addActionListener(this);
		copyClipCSV.addActionListener(this);
		addUserInfo.addActionListener(this);
		saveSelectedAsCSV.addActionListener(this);
		saveSelectedAsText.addActionListener(this);
		saveSelectedAsTwiki.addActionListener(this);
		saveSelectedAsXML.addActionListener(this);
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
		if (e.getSource()==copyClipXml) {
			copyToClipboard(new XMLConverter());
		} else if (e.getSource()==copyClipTxt) {
			copyToClipboard(new TextConverter(null));
		} else if (e.getSource()==copyClipTwikiTable) {
			copyToClipboard(new TwikiTableConverter(null));
		} else if (e.getSource()==copyClipCSV) {
			copyToClipboard(new CSVConverter());
		} else if (e.getSource()==addUserInfo) {
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
		} else if (e.getSource()==saveSelectedAsCSV) {			
			saveSelectedLogs(new CSVConverter(),saveCsvIcon,"txt");
		} else if (e.getSource()==saveSelectedAsText) {
			saveSelectedLogs(new TextConverter(null),saveTextIcon,"txt");
		} else if (e.getSource()==saveSelectedAsTwiki) {
			saveSelectedLogs(new TwikiTableConverter(null),saveTwikiTableIcon,"txt");
		} else if (e.getSource()==saveSelectedAsXML) {
			saveSelectedLogs(new XMLConverter(),saveXmlIcon,"xml");
		}else if (e.getSource()==showErrorStack) {
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
	 * Save the selected logs into a file in the passed format.
	 * 
	 * @param converter The converter to desired format
	 * @param fileIcon The icon of the file type
	 */
	private void saveSelectedLogs(LogConverter converter, ImageIcon fileIcon, String extension) {
		if (converter==null) {
			throw new IllegalArgumentException("The converter can't be null");
		}
		if (extension==null || extension.isEmpty()) {
			throw new IllegalArgumentException("Invalid file extension");
		}
		converter.setCols(buildFields());
		// Build the text to save in the file
		StringBuilder strBuffer = new StringBuilder();
		if (!selectionModel.isSelectionEmpty()) {
			for (int i=selectionModel.getMinSelectionIndex(); 
				i<=selectionModel.getMaxSelectionIndex(); i++) {
				if (!selectionModel.isSelectedIndex(i)) {
					continue;
				} else {
					ILogEntry log = model.getVisibleLogEntry(sorter.convertRowIndexToModel(i));
					strBuffer.append(converter.convert(log));
				}
			}
			if (strBuffer.length()==0) {
				// Nothing to save
				return;
			}
				
		}
		FileFilter fileFilter = new FileNameExtensionFilter("File "+extension,extension);
		CustomFileChooser fc = new CustomFileChooser(fileIcon,fileFilter);
		fc.addChoosableFileFilter(fileFilter);
		if (fc.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();
			// If not present, add the xml extension
			if (!file.getAbsolutePath().toLowerCase().endsWith("."+extension)) {
				file = new File(file.getAbsolutePath()+"."+extension);
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
			stackId = (String)selectedLog.getField(LogField.STACKID);
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
	
	/**
	 * Build the string of log fields to write in the output
	 * 
	 * @return the string of log fields to write in the output
	 */
	private String buildFields() {
		int colCount=table.getColumnCount();
		String ret="";
		
		for (int t=0; t<colCount; t++) {
			int modelCount = table.convertColumnIndexToModel(t);
			if (modelCount==0) {
				continue;
			}
			ret+=LogField.values()[modelCount-1].id;
		}
		return ret;
	}
	
	private void copyToClipboard(LogConverter converter) {
		if (converter==null) {
			throw new IllegalArgumentException("Invalid null converter");
		}
		converter.setCols(buildFields());
		// Build the text to copy in the clipboard
		if (!selectionModel.isSelectionEmpty()) {
			StringBuffer strBuffer = new StringBuffer();
			for (int i=selectionModel.getMinSelectionIndex(); 
				i<=selectionModel.getMaxSelectionIndex(); i++) {
				if (!selectionModel.isSelectedIndex(i)) {
					continue;
				} else {
					ILogEntry log = model.getVisibleLogEntry(table.convertRowIndexToModel(i));
					strBuffer.append(converter.convert(log));
				}
			}
			// Copy the text to the clipboard
			textTransfer.setClipboardContents(strBuffer.toString());
		}
	}
}
