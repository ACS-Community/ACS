/*
 * Created on Oct 17, 2006 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.Component;
import java.io.PrintWriter;
import java.io.StringWriter;

import javax.swing.Box;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;




public class ErrorBox {

	public static void showErrorDialog (Component parentComponent, String summary, Throwable detail) {
		StringWriter w = new StringWriter(1024);
		detail.printStackTrace(new PrintWriter(w, true));
		showErrorDialog(parentComponent, summary, w.toString());
	}

	public static void showErrorDialog (Component parentComponent, String summary, String detail) {
		JTextArea s = new JTextArea(summary);
		s.setOpaque(false);
		s.setEditable(false);
		s.setLineWrap(true);
		
		JTextArea a = new JTextArea(6, 40);
		Box c = Box.createVerticalBox();
		c.add(s);
		c.add(Box.createVerticalStrut(10));
		c.add(new JScrollPane(a));
		a.setText(detail);
		a.setCaretPosition(0);
		JOptionPane.showMessageDialog(parentComponent, c, summary, JOptionPane.ERROR_MESSAGE);
	}

	public static void showMessageDialog (Component parentComponent, String message, boolean failure) {
		int type = (failure) ? JOptionPane.ERROR_MESSAGE : JOptionPane.INFORMATION_MESSAGE;
		String title = (failure) ? "Error" : "Information";

		JOptionPane.showMessageDialog(parentComponent, message, title, type);
	}
}


