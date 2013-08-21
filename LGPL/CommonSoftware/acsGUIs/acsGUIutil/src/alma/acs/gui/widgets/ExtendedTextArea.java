/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
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
package alma.acs.gui.widgets;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.io.FileWriter;

import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTextArea;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.text.Element;

import alma.acs.gui.util.threadsupport.EDTExecutor;

/**
* <CODE>ExtendedTextArea</CODE> is a {@link JTextArea} written to replace
*  <CODE>com.cosylab.gui.components.r2.SmartTextArea</CODE>.
*  It is a light component with a minimum set of functionalities compared to that of 
*  the abeans <CODE>SmartTextArea</CODE> because most of such functionalities have 
*  never been used in ALMA.
* <P>
* This widget allows to easily add messages controlling the number of messages
* displayed to avoid out of memory while running for long time.
* It allows to save the content of the widget in a file by means of a popup menu.   
* <P>
* The widget displays at most {@link #maxNumOfMessages} (default is {@link #defaultNumOfMessages})
* at a given time: when a new message arrives, the oldest one is removed if it is the case.
* 
* @author acaproni
* @since ACS 12.1
*/
public class ExtendedTextArea extends JTextArea implements MouseListener {
	
	/**
	 * The popup menu shown when the user presses the right mouse button
	 * over the <CODE>ExtendedTextArea</CODE> component.
	 *
	 * @author acaproni
	 * @since ACS 12.1
	 */
	class PopupMenu extends JPopupMenu {

		/**
		 * The menu item to save the content of the widget
		 */
		public final JMenuItem saveMI;
		
		/**
		 * Constructor.
		 */
		public PopupMenu() {
			saveMI=new JMenuItem("Save");
			add(saveMI);
			saveMI.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					final JFileChooser chooser = new JFileChooser();
				    FileNameExtensionFilter filter = new FileNameExtensionFilter("Text files", "txt");
				    chooser.setFileFilter(filter);
				    int returnVal = chooser.showSaveDialog(ExtendedTextArea.this);
				    if(returnVal == JFileChooser.APPROVE_OPTION) {
				    	Thread saveThread = new Thread(new Runnable() {
							@Override
							public void run() {
								ExtendedTextArea.this.saveContent(chooser.getSelectedFile(),ExtendedTextArea.this.getText());
							}
						},"ExtendedTextArea.saveThread");
				    	saveThread.setDaemon(true);
				    	saveThread.start();
				    }
				}
			});
		}
	}
	
	/**
	 * The default of the max number of messages displayed by the widget.
	 */
	public static final int defaultNumOfMessages=500;
	
	/**
	 * The max number of messages displayed by the widget. 
	 */
	private int maxNumOfMessages=ExtendedTextArea.defaultNumOfMessages;
	
	/**
	 * The popup menu
	 * @see PopupMenu
	 */
	private PopupMenu menu;
	
	/**
	 * Constructor
	 * 
	 * @param maxNumOfMessages The max number of messages displayed by the widget
	 */
	public ExtendedTextArea(int maxNumOfMessages) {
		this.maxNumOfMessages=maxNumOfMessages;
		EDTExecutor.instance().execute(new Runnable() {
			@Override
			public void run() {
				initGUI();
			}
		});
	}
	
	/**
	 * Constructor with the default max number of messages ({@link #defaultNumOfMessages}
	 */
	public ExtendedTextArea() {
		this(defaultNumOfMessages);
	}
	
	/**
	 * Initialize the widget
	 */
	private void initGUI() {
		setEditable(false);
		addMouseListener(this);
		
		menu = new PopupMenu();
	}

	/**
	 * Append the passed message to the status area.
	 */
	public void append(final String msg) {
		if (msg==null || msg.isEmpty()) {
			// Nothing to do
			return;
		}
		EDTExecutor.instance().execute(new Runnable() {
			@Override
			public void run() {
				String txtToAppend=(msg.endsWith("\n"))?msg.substring(0,msg.length()-1):msg;
				if (getDocument().getDefaultRootElement().getElementCount()>0) {
					txtToAppend="\n"+txtToAppend;
				}
				ExtendedTextArea.super.append(txtToAppend);
				while (getDocument().getDefaultRootElement().getElementCount()>maxNumOfMessages) {
					Element root = getDocument().getDefaultRootElement();
					Element first = root.getElement(0);
					try {
						getDocument().remove(first.getStartOffset(), first.getEndOffset());
					} catch (Throwable t) {
						t.printStackTrace();
					}
				}
			}
		});
	}
	
	/**
	 * @see MouseListener
	 */
	@Override
	public void mouseClicked(MouseEvent e) {}

	/**
	 * @see MouseListener
	 */
	@Override
	public void mousePressed(MouseEvent e) {
		if (e.isPopupTrigger()){
			menu.show(e.getComponent(), e.getX(), e.getY());
		}
	}

	/**
	 * @see MouseListener
	 */
	@Override
	public void mouseReleased(MouseEvent e) {
		if (e.isPopupTrigger()){
			menu.show(e.getComponent(), e.getX(), e.getY());
		}
	}

	/**
	 * @see MouseListener
	 */
	@Override
	public void mouseEntered(MouseEvent e) {}

	/**
	 * @see MouseListener
	 */
	@Override
	public void mouseExited(MouseEvent e) {}
	
	/**
	 * Save the content of the text area in a file with the passed name.
	 * <P>
	 * This method must not run into the EDT.
	 * 
	 * @param outFile The file to save the content of the text area into
	 * @param content The content of the text area
	 */
	private void saveContent(File outFile, String content) {
		if (outFile==null) {
			throw new IllegalArgumentException("The file can't be null!");
		}
		if (content==null) {
			throw new IllegalArgumentException("The string to be saved can't be null!");
		}
		if (content.isEmpty()) {
			return;
		}
		FileWriter writer=null;
		String errorMsg=null; // Set in case of error
		try {
			writer = new FileWriter(outFile);
			writer.write(content);
		} catch (Throwable t) {
			errorMsg="Error writing into "+outFile.getPath()+": "+t.getMessage();
			System.err.println(errorMsg);
			t.printStackTrace(System.err);
		} finally {
			if (writer!=null) {
				try {
					writer.close();
				} catch (Throwable t) {
					String msg="Error closing "+outFile.getPath()+": "+t.getMessage();
					errorMsg=(errorMsg==null)?msg:"\n"+msg;
					System.err.println(msg);
					t.printStackTrace(System.err);
				}
			}
		}
		// Report the error, if any
		if (errorMsg!=null) {
			JOptionPane.showMessageDialog(this, errorMsg, "Error saving data", JOptionPane.ERROR_MESSAGE);
		}
		
	}
}
