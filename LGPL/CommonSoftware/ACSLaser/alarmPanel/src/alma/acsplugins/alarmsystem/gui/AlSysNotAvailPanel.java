/**
 * 
 */
package alma.acsplugins.alarmsystem.gui;

import java.awt.BorderLayout;
import java.awt.FlowLayout;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

/**
 * The panel shown when the AS is not available even if the CERN
 * AS is in use.
 * <P>
 * This panel is shown at startup until the client connects to the AS.
 * The purpose is to inform the user that the AS is not available but can signal 
 * an error if the AS did not start.
 * 
 * @author acaproni
 */
public class AlSysNotAvailPanel extends JPanel {

	/**
	 * The label shown by the panel
	 */
	private final JLabel lbl = new JLabel("<HTML>Alarm service <B>NOT</B> connected!</HTML>");
	
	private final JTextArea messages = new JTextArea();
	
	/**
	 * Constructor
	 */
	public AlSysNotAvailPanel() {
		initialize();
	}
	
	/**
	 * Initialize the GUI
	 */
	private void initialize() {
		setLayout(new BorderLayout());
		
		// Add the label
		JPanel labelPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		labelPnl.add(lbl);
		
		add(labelPnl,BorderLayout.NORTH);
		
		// Add the message area
		messages.setEditable(false);
		add(messages,BorderLayout.CENTER);
	}
	
	/**
	 * Add a message in the message area
	 * 
	 * @param msg The message to add in the message area
	 */
	public synchronized void addMessage(final String msg) {
		if (msg==null || msg.isEmpty()) {
			// Do not add emty/null messages
			return;
		}
		
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				String txt = messages.getText();
				if (!txt.endsWith("\n")) {
					txt = txt +"\n"+msg;
				} else {
					txt = txt+msg;
				}
				messages.setText(txt);
			}
		});
	}
}
