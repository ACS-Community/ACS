package alma.acsplugins.alarmsystem.gui;
import java.awt.FlowLayout;

import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * 
 */

/**
 * The panel shown when the ACS alarm system is in use.
 * <P>
 * This panel contains a label to inform the user to open jlog instead
 * 
 * @author acaproni
 */
public class AcsAlSysPanel extends JPanel {
	/**
	 * The label shown by the panel
	 */
	private final JLabel lbl = new JLabel("<HTML><B>ACS alarm system in use!</B><BR>Use <I>jlog</I> to monitor alarms.</HTML>");
	
	/**
	 * Constructor
	 */
	public AcsAlSysPanel() {
		initialize();
	}
	
	/**
	 * Initialize the GUI
	 */
	private void initialize() {
		setLayout(new FlowLayout(FlowLayout.LEFT));
		add(lbl);
	}
}
