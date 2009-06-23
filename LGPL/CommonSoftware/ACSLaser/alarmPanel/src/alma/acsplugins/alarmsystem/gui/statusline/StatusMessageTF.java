/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acsplugins.alarmsystem.gui.statusline;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

/**
 * The label showing a status message.
 * <P>
 * After the timeout elapses without adding any other message, the 
 * label is cleared;
 * 
 * @author acaproni
 *
 */
public class StatusMessageTF extends JTextField implements ActionListener {
	
	/**
	 * The timeout to clear the label
	 */
	private final int TIMEOUT=10000;
	
	/**
	 * The time to clear the label
	 */
	private Timer timer=new Timer(TIMEOUT,this);
	
	/**
	 * The foreground of the statusMessage label
	 */
	private final Color statusMessageFgColor = getForeground();
	
	public StatusMessageTF() {
		super(20);
		setEditable(false);
		timer.setRepeats(false);
		timer.addActionListener(this);
	}
	
	/**
	 * Show a message in the label
	 * 
	 * @param mesg The not <code>null</code> nor empty message to show
	 * @param red <code>true</code> if the string must be shown in red
	 */
	public void showMessage(final String mesg, final boolean red) {
		if (mesg==null || mesg.isEmpty()) {
			throw new IllegalArgumentException("The mmessage can't be null nor empty");
		}
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				if (red) {
					setForeground(Color.red);
				} else {
					setForeground(statusMessageFgColor);
				}
				setText(mesg);
			}
		});
		timer.restart();
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==timer) {
			setText("");
		}
	}
}
