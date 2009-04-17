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
package com.cosylab.logging;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

/**
 * The widget reporting messages.
 * <P>
 * This widget is invisible and appears only when a message must be notified
 * to the user. 
 * When the user presses the Ok button, the panel hides itself.
 * The method <code>removeError()</code> allows to hide the panel programmatically. 
 * <P>
 * The look of the panel changes if a detailed description of the error is present.
 * If it is the case, the panel shows a title label on top.
 * If instead the detailed description is not defined, then the label and the detailed panel are hidden.
 * This allows to use the component as "light" or "heavy" by setting a detailed description.
 * Which of the two modality is in use is returned by the <code>boolean</code> of the two <code>showMessage(...)</code> methods.
 *  
 * @author acaproni
 *
 */
public class MessageWidget extends JPanel {
	
	/**
	 * The type of the message shown by the glass pane
	 * 
	 * @author acaproni
	 *
	 */
	public enum MessageType {
		
		Error("dialog-error.png"),
		Warning("dialog-warning.png"),
		Info("dialog-information.png");
		
		/**
		 * The icon shown for each type of dialog
		 * 
		 */
		public final ImageIcon icon;
		
		/**
		 * Constructor
		 * 
		 * @param iconPath
		 */
		private MessageType(String iconPath) {
			icon=new ImageIcon(MessageWidget.class.getResource("/"+iconPath));
		}
	};
	
	/**
	 * The listener to be notified when the user acknowledge
	 * the message by pressing the button
	 * 
	 * @author acaproni
	 *
	 */
	public interface MessageWidgetListener {
		public void errorAcknowledged();
	}
	
	/**
	 * The text describing the problem
	 */
	private final JLabel shortDescriptionLbl=new JLabel();
	
	/**
	 * The text to give a detailed description of the problem.
	 */
	private final JTextArea detailedDescritpionTA= new JTextArea();
	
	/**
	 * The panel with the detailed description of the problem.
	 * <P>
	 * This component appears only if it is not empty.
	 */
	private JScrollPane detailedDescPnl;
	
	/**
	 * The icon
	 */
	private final JLabel iconLbl = new JLabel();
	
	/** 
	 * The title of the error.
	 * <P>
	 * The title is displayed on top but only if the detailed description is present.
	 */
	private final JLabel titleLbl = new JLabel();
	
	/**
	 * The button to acknowledge the message
	 */
	private final JButton ackBtn = new JButton("Ok");
	
	/**
	 * The listeners to be notified when the user acknowledges the message
	 */
	private Collection<MessageWidgetListener> ackListeners=null;
	
	/**
	 * Constructor
	 * 
	 * @param client The {@link LoggingClient} that owns this glass pane
	 */
	public MessageWidget() {
		initialize();
		setVisible(false);
	}
	
	/**
	 * Init te GUI.
	 */
	private void initialize() {
		BorderLayout layout = new BorderLayout();
		layout.setHgap(10);
		layout.setVgap(10);
		setLayout(layout);
		
		
		// The upper panel contains the icon, the message and the ack button
		JPanel upperPnl=new JPanel(new BorderLayout());
		upperPnl.add(iconLbl,BorderLayout.WEST);
		upperPnl.add(shortDescriptionLbl,BorderLayout.CENTER);
		upperPnl.add(ackBtn,BorderLayout.EAST);
		// Add the title
		JPanel titlePnl = new JPanel(new FlowLayout(FlowLayout.CENTER));
		titlePnl.add(titleLbl);
		upperPnl.add(titlePnl,BorderLayout.NORTH);
		ackBtn.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				removeMessage();
				notifyListeners();
			}
			
		});
		add(upperPnl,BorderLayout.NORTH);
		
		detailedDescPnl = new JScrollPane(
				detailedDescritpionTA,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		detailedDescPnl.setBorder(BorderFactory.createTitledBorder("Further details"));
		detailedDescritpionTA.setEditable(false);
		add(detailedDescPnl,BorderLayout.CENTER);
	}
	
	/**
	 * Show the glass pane and catches all the events until the user acknowledges the message
	 * by pressing the button.
	 * <P>
	 * The message type and the short description are mandatory to present to the user the 
	 * message.
	 * A detailed description can also be given to better explain what's going on. It might be 
	 * a stack trace, for example.
	 * 
	 * @param messageType The type of the message
	 * @param shortDescription A short description of the message
	 * @param description The detailed description; it can be <code>null</code> or empty. 
	 * @return <code>true</code> if the detailed description is not empty  i.e. the description
	 * 				scroll panel is visible 
	 */
	public boolean showMessage(final MessageType messageType, final String shortDescription, final String description) {
		if (messageType==null) {
			throw new IllegalArgumentException("The type can't be null");
		}
		if (shortDescription==null || shortDescription.isEmpty()) {
			throw new IllegalArgumentException("Invalid message descritpion");
		}
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				shortDescriptionLbl.setText(shortDescription);
				iconLbl.setIcon(messageType.icon);
				
				setVisible(true);
				if (description!=null && !description.isEmpty()) {
					detailedDescritpionTA.setText(description);
					detailedDescPnl.setVisible(true);
					titleLbl.setText("<HTML><FONT size=\"+1\">"+messageType+"</FONT></HTML>");
					titleLbl.setVisible(true);
				} else {
					detailedDescritpionTA.setText("");
					detailedDescPnl.setVisible(false);
					titleLbl.setVisible(false);
				}		
			}
		});
		
		return (description!=null && !description.isEmpty());
	}
	
	/**
	 * Show the glass pane and catches all the events until the user acknowledges the message
	 * by pressing the button.
	 * <P>
	 * This method shows an error message, by setting the stack trace of the passed {@link Throwable} 
	 * in the detailed description of the error.
	 * The throwable and the short description are mandatory to present to the user the 
	 * message.
	 * 
	 * @param shortDescription A short description of the message
	 * @param t The not <code>null</code> throwable to be displayed in the detailed text area.
	 * @return <code>true</code> if the detailed description is not empty  i.e. the description
	 * 				scroll panel is visible 
	 */
	public boolean showMessage(String shortDescription, Throwable t) {
		if (t==null) {
			throw new IllegalArgumentException("The throwable can't be null");
		}
		StringWriter writer = new StringWriter();
		PrintWriter printW = new PrintWriter(writer);
		printW.flush();
		t.printStackTrace(printW);
		String desc=writer.getBuffer().toString();
		printW.close();
		return showMessage(MessageType.Error,shortDescription,desc);
	}
	
	/**
	 * Hide the error panel.
	 * <P>
	 * This happens when the user presses the OK button but it might happen
	 * when the abnormal situation has been fixed and therefore jlog has 
	 * no reason to ask for the attention of the user.
	 * One example could be that of an automatic reconnection.
	 */
	public void removeMessage() {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				setVisible(false);
			}
		});
	}
	
	public JButton getAckButton() {
		return ackBtn;
	}
	
	/**
	 * Add a listener to be notified when the user presses the ack button.
	 * 
	 * @param listener The listener to add to the list of listeners; 
	 * 			the listener is not added if it is already in the list
	 */
	public void addAckListener(MessageWidgetListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		if (ackListeners==null) {
			Vector<MessageWidgetListener> v = new Vector<MessageWidgetListener>();
			ackListeners=Collections.synchronizedCollection(v);
		}
		if (!ackListeners.contains(listener)) {
			ackListeners.add(listener);
		}
	}
	
	/**
	 * Remove a listener from the listeners to be notified when the user 
	 * acknowledges the message.
	 * If the listener is not in the list, does nothing.
	 * 
	 * @param listener The listener to remove from the list of listeners;
	 * @return <code>true</code> if an element was removed as a result of this call 
	 */
	public boolean removeAckListener(MessageWidgetListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		if (ackListeners==null) {
			return false;
		}
		return ackListeners.remove(listener);
	}
	
	/**
	 * Notify all the listeners that the user acknowledged the message
	 */
	private void notifyListeners() {
		if (ackListeners==null) {
			return;
		}
		Iterator<MessageWidgetListener> iter = ackListeners.iterator();
		while (iter.hasNext()) {
			iter.next().errorAcknowledged();
		}
	}
}
