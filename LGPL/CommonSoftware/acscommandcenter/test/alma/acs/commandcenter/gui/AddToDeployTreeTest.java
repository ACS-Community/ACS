/*
 * Created on Nov 8, 2004 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.util.List;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;

import junit.extensions.jfcunit.JFCTestCase;
import junit.extensions.jfcunit.JFCTestHelper;
import junit.extensions.jfcunit.TestHelper;
import junit.extensions.jfcunit.eventdata.MouseEventData;
import junit.extensions.jfcunit.eventdata.StringEventData;
import junit.extensions.jfcunit.finder.AbstractButtonFinder;
import junit.extensions.jfcunit.finder.ComponentFinder;
import junit.extensions.jfcunit.finder.DialogFinder;


public class AddToDeployTreeTest extends JFCTestCase {

	// =============================================
	/** used for something else */
	public static void main(String[] args) {
		JPanel panel = new AddToDeployTree(null, null);
		JFrame frame = new JFrame();
		frame.getContentPane().add(panel);
		frame.pack();
		frame.setVisible(true);
	}
	// =============================================
	
	
	private AddToDeployTree panel = null;

	public AddToDeployTreeTest(String name) {
		super(name);
	}

	@Override
	protected void setUp () throws Exception {
		super.setUp();

		// Choose the text Helper
		setHelper(new JFCTestHelper()); // Uses the AWT Event Queue.
		// setHelper( new RobotTestHelper( ) ); // Uses the OS Event Queue.

		panel = new AddToDeployTree(null, null);

		JFrame frame = new JFrame();
		frame.getContentPane().add(panel);
		frame.pack();
		frame.setVisible(true);
	}

	@Override
	protected void tearDown () throws Exception {
		panel = null;
		TestHelper.cleanUp(this);
		super.tearDown();
	}

	public void testPortValidation () throws Exception {

		JDialog dialog;

		AbstractButtonFinder buttonFinder = new AbstractButtonFinder("Add to View");
		buttonFinder.setIgnoreVisibility(true); // { m_ignoreVisiblity || comp.isShowing() }
		
		JButton addButton = (JButton) buttonFinder.find(panel, 0);
		assertNotNull("Could not find the Add button", addButton);

		buttonFinder.setText("Full Refresh");
		JButton refreshButton = (JButton) buttonFinder.find(panel, 0);
		assertNotNull("Could not find the Refresh button", refreshButton);

		ComponentFinder componentFinder = new ComponentFinder(JTextField.class);
		componentFinder.setIgnoreVisibility(true); // { m_ignoreVisiblity || comp.isShowing() }
		
		JTextField hostField = (JTextField) componentFinder.find(panel, 0);
		assertNotNull("Could not find the host field", hostField);
		assertEquals("host field is empty", "", hostField.getText());

		JTextField portField = (JTextField) componentFinder.find(panel, 0);
		assertNotNull("Could not find the port field", portField);
		assertEquals("port field is empty", "", portField.getText());
		
		getHelper().sendString (new StringEventData (this, hostField, "testhost"));
		getHelper().sendString (new StringEventData (this, portField, "2"));
		
		getHelper().enterClickAndLeave(new MouseEventData(this, addButton));
		
		DialogFinder dFinder = new DialogFinder(null);
		dFinder.setWait(2);
		List<Object> showingDialogs = dFinder.findAll();
		assertEquals("Number of dialogs showing is wrong", 1, showingDialogs.size());

		dialog = (JDialog) showingDialogs.get(0);
		assertEquals("Wrong dialog showing up", "Message", dialog.getTitle());

		TestHelper.disposeWindow(dialog, this);
	}

}

//
//
//
//
//
//
//
//
//
//
//
//