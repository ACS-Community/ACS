/*
 * Created on Apr 6, 2004 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.PlainDocument;



public class FeedbackArea extends JPanel {

	protected final String[] kbytes = new String[]{"Max.Size: 32K", "Max.Size: 128K", "Max.Size: 512K", "Max.Size: unlimited"};
	protected final int[] chars = new int[]{32*512, 128*512, 512*512, Integer.MAX_VALUE};

	protected FeedbackTabs surroundingTabbedPane;
	protected String surroundingTabTitle;
	protected JTextArea outputArea;
	protected JComboBox txtSize;
	protected CommandCenterGui master;
	protected boolean scrollLock = false;
	protected int maxLength;

	protected FeedbackArea(CommandCenterGui master, FeedbackTabs tabbedPane, String tabTitle) {

		this.master = master;

		//needed to be able to remove the tab through the button below
		surroundingTabbedPane = tabbedPane;
		surroundingTabTitle = tabTitle;
		setLayout(new BorderLayout());

		JToolBar toolbar = new JToolBar();
		toolbar.setFloatable(false);

		final JToggleButton btnScrollLock = new JToggleButton("Scroll Lock");
		btnScrollLock.setToolTipText("Scroll Lock (forces Unlimited Buffer Size)");
		btnScrollLock.addActionListener(new ActionListener() {

			public void actionPerformed (ActionEvent e) {
				scrollLock = btnScrollLock.isSelected();
				if (scrollLock) {
					txtSize.setSelectedIndex(kbytes.length-1);
				}
			}
		});
		toolbar.add(btnScrollLock);
		toolbar.add(Box.createHorizontalStrut(7));
		
		txtSize = new JComboBox(kbytes);
		txtSize.setToolTipText("Change Buffer Size (caution: takes immediate effect)");
		txtSize.setMaximumSize(txtSize.getPreferredSize());
		txtSize.setEditable(false);
		class SizeListener extends KeyAdapter implements FocusListener, ItemListener {
			void changeSize() {
				int newLength = chars[txtSize.getSelectedIndex()];
				if (newLength != maxLength) {
					maxLength = newLength;
					//System.out.println("new maxLength set: "+newLength);
					FeedbackArea.this.append(""); // trigger evalutation
				}
			}

			// will respond to all selections, e.g. with mouse but also
			// when wandering through the popup-list using the arrow keys. 
			// i personally don't find this ergonomic but others do.  
			public void itemStateChanged (ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED)
					changeSize();
			}

			// expect the user to press "return" to confirm
			// a value chosen with the arrow keys
			@Override
			public void keyPressed (KeyEvent e) {
				/*
				if (e.getKeyCode() == 10)
					changeSize();
				*/
			}

			// expect the user to remove the blue "i'm selected" mark
			// from the newly chosen value by clicking on some other component
			// (and thereby taking the focus away from this combobox)
			public void focusLost (FocusEvent e) {
				/*
				changeSize();
				*/
			}
			public void focusGained (FocusEvent e) {}
			
			
		};
		SizeListener sizeLis = new SizeListener();
		txtSize.addKeyListener(sizeLis);
		txtSize.addFocusListener(sizeLis);
		txtSize.addItemListener(sizeLis);
		toolbar.add(txtSize);
		toolbar.add(Box.createHorizontalStrut(7));
		
		final JButton btnSave = new JButton("Save...");
		btnSave.setToolTipText("Save this Log");
		btnSave.addActionListener(new ActionListener() {

			public void actionPerformed (ActionEvent e) {
				surroundingTabbedPane.saveTab(FeedbackArea.this);
			}
		});
		toolbar.add(btnSave);

		toolbar.add(Box.createHorizontalGlue());

		final JButton btnClear = new JButton("Clear");
		btnClear.setToolTipText("Clear this Log");
		btnClear.addActionListener(new ActionListener() {

			public void actionPerformed (ActionEvent e) {
				clear();
			}
		});
		toolbar.add(btnClear);
		toolbar.add(Box.createHorizontalStrut(7));

		final JButton btnRemove = new JButton("Remove");
		btnRemove.setToolTipText("Remove this Log");
		btnRemove.addActionListener(new ActionListener() {

			public void actionPerformed (ActionEvent evt) {
				surroundingTabbedPane.removeTab(surroundingTabTitle);
			}
		});
		toolbar.add(btnRemove);
		toolbar.add(Box.createHorizontalStrut(7));

		toolbar.addSeparator();
		
		final JButton btnClearAll = new JButton("Clear All");
		btnClearAll.setToolTipText("Clear all Logs");
		btnClearAll.addActionListener(new ActionListener() {

			public void actionPerformed (ActionEvent evt) {
				surroundingTabbedPane.clearAllTabs();
			}
		});
		toolbar.add(btnClearAll);

		
		this.add(toolbar, BorderLayout.NORTH);
		Document doc = new PlainDocument();
		outputArea = new JTextArea(doc, null, 10, 40);
		outputArea.setEditable(false);

		JScrollPane scp = new JScrollPane(outputArea); 
		this.add(scp, BorderLayout.CENTER);
		
		// --- post config
		txtSize.setSelectedIndex(1);
		maxLength = chars[1];
		
		
		outputArea.setName("txt_Output");
		txtSize.setName("txt_BufferSize");
		btnScrollLock.setName("btn_ScrollLock");
		btnSave.setName("btn_Save");
		btnClear.setName("btn_Clear");
		btnRemove.setName("btn_Remove");
		btnClearAll.setName("btn_ClearAll");
	}

	protected void clear() {
		outputArea.setText("");
	}
	
	protected void append (String feedback) {
		Document doc = outputArea.getDocument();
		if (doc == null) {
			return;
		}
		
		try {
			doc.insertString(doc.getLength(), feedback, null);
		} catch (BadLocationException exc) {}

		
		int newCaretPosition = outputArea.getCaretPosition(); 
		
		int newLength = doc.getLength();
		int tooMuch = Math.max(newLength - maxLength, 0);
		if (newLength > maxLength) {
			newCaretPosition -= (newLength - maxLength);
		}

		int caretPosition;
		if (!scrollLock)
			caretPosition = doc.getLength() - tooMuch;
		else {
			if (newCaretPosition >= 0)
				caretPosition = newCaretPosition;
			else
				caretPosition = 0;
		}
		outputArea.setCaretPosition(caretPosition);
		
		if (newLength > maxLength) {
			try {
				doc.remove(0, newLength - maxLength);
			} catch (BadLocationException exc1) {}
		}
		

	}

}
////////////////////////////////////////////////////////
/// ------------------- API ------------------------ ///
////////////////////////////////////////////////////////

////////////////////////////////////////////////////////
/// ----------------- Internal --------------------- ///
////////////////////////////////////////////////////////

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