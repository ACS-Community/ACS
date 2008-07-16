package alma.acs.logging.errorbrowser;

import java.awt.Dimension;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;

import alma.acs.logging.table.LogEntryTableModelBase;

/**
 * The dialog showing the stack trace of errors.
 * <P>
 * The dialog has one tab for each stack trace whose title is the stack ID.
 * 
 * @author acaproni
 *
 */
public class ErrorBrowserDlg extends JDialog {
	
	/**
	 * The pane showing the error tabs
	 */
	private JTabbedPane tabbedPane = new JTabbedPane(JTabbedPane.TOP);

	/**
	 * Constructor
	 */
	public ErrorBrowserDlg() {
		super((JFrame)null, "Error browser");
		setDefaultCloseOperation(JDialog.HIDE_ON_CLOSE);
		initialize();
	}
	
	/**
	 * Init the GUI.
	 * <P>
	 * The dialog is composed of a set of tabs, one for each stack trace.
	 */
	private void initialize() {
		add(tabbedPane);
		setMinimumSize(new Dimension(400,100));
		setPreferredSize(new Dimension(400,100));
		pack();
		setVisible(true);
	}
	
	/**
	 * Add a new error tab.
	 * <P>
	 * The tab is added by a separate thread.
	 * 
	 * @param sourceModel The model used by the <code>Engine</code> to look for logs
	 *                    belonging to the error trace with the give <code>STACKID</code>
	 * @param stackId The <code>STACKID</code> of logs of the stack trace
	 */
	public void addErrorTab(LogEntryTableModelBase sourceModel, String stackId) {
		if (stackId==null || stackId.isEmpty()) {
			throw new IllegalArgumentException("The stackID can't be null nor empty");
		}
		if (sourceModel==null) {
			throw new IllegalArgumentException("The model can't be null");
		}
		
		// Check if a tab with the given name already exists
		for (int t=0; t<tabbedPane.getTabCount(); t++) {
			if (tabbedPane.getTitleAt(t).equals(stackId)) {
				tabbedPane.removeTabAt(t);
				break;
			}
		}
		// Add the new tab
		ErrorTab newTab=null;
		try {
			newTab = new ErrorTab(sourceModel,stackId);
		} catch (Exception e) {
			e.printStackTrace();
			JOptionPane.showMessageDialog(null, "Error creating the error tab: "+e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		class AddTab implements Runnable {
			String id;
			ErrorTab tab;
			public void run() {
				tabbedPane.add(tab,id);
				setVisible(true);
			}
		}
		AddTab temp = new AddTab();
		temp.id=stackId;
		temp.tab=newTab;
		SwingUtilities.invokeLater(temp);
	}
	
	/**
	 * Close the dialog
	 */
	public void close() {
		for (int t=0; t<tabbedPane.getTabCount(); t++) {
			ErrorTab tab = (ErrorTab)tabbedPane.getComponentAt(t);
			if (tab!=null) {
				tab.close();
			} 
		}
		setVisible(false);
		dispose();
	}
	
}
