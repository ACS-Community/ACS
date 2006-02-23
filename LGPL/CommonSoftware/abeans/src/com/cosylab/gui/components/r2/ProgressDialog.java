package com.cosylab.gui.components.r2;

import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.Timer;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;


/**
 * Insert the type's description here.
 * Creation date: (4/14/2002 12:55:00)
 * @author: 
 */
public class ProgressDialog extends javax.swing.JDialog {
	private JLabel statusLabel;
	private JProgressBar progressBar;

	private AbstractProgressTask task;

	private Timer timer;

	protected class TimerListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			updateState();
		}
	}

	protected class StateListener implements ProgressListener {
		public void progress(ProgressEvent e) {
			updateState();
		}
		public void taskComplete(ProgressEvent e) {
			updateState();
			setVisible(false);
		}
		public void taskInterruped(ProgressEvent e) {
			setStatus(e.getStatus());
			hide();
		}
		public void taskStarted(ProgressEvent e) {
			show();
		}

	}
/**
 * ProgressDialog constructor comment.
 */
public ProgressDialog(AbstractProgressTask task) {
	super();
	this.task = task;
	initialize();
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 12:56:53)
 * @param x int
 * @param y int
 */
private GridBagConstraints createConstraints(int x, int y) {
	GridBagConstraints gc = new GridBagConstraints();
	gc.gridx = x;
	gc.gridy = y;
	gc.fill = GridBagConstraints.HORIZONTAL;
	gc.weightx = 1.0;
	gc.weighty = 0.0;
	gc.anchor = GridBagConstraints.WEST;
	gc.insets = new java.awt.Insets(4, 4, 4, 4);

	return gc;
		
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 12:58:34)
 */
private void initialize() {
	setSize(350, 100);
	getContentPane().setLayout(new GridBagLayout());
	
	statusLabel = new JLabel();
	getContentPane().add(statusLabel, createConstraints(0, 0));

	progressBar = new JProgressBar();
	getContentPane().add(progressBar, createConstraints(0, 1));
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 13:01:29)
 * @param args java.lang.String[]
 */
public static void main(String[] args) {
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 14:14:38)
 */
public void performTask() {
	
	TimerListener tl = new TimerListener();
	ProgressListener pl = new StateListener();

	timer = new Timer(500, tl);
	task.addProgressListener(pl);

	task.start();
	timer.start();

}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 13:01:10)
 * @param value int
 */
protected void setProgress(int value) {
	progressBar.setValue(value);
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 13:00:29)
 * @param min int
 * @param max int
 */
protected void setProgressBounds(int min, int max) {
	if (min >= max)
		return;
		
	progressBar.setMinimum(min);
	progressBar.setMaximum(max);
	progressBar.setValue(min);
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 13:00:01)
 * @param status java.lang.String
 */
protected void setStatus(String status) {
	statusLabel.setText(status);
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:54:10)
 */
protected void updateState() {
    ProgressDialog owner = ProgressDialog.this;

    int current = task.getCurrent();
    int total = task.getTotal();

    owner.progressBar.setVisible(total > -1);

    owner.setProgressBounds(0, total);
    owner.setProgress(current);

    setStatus(owner.task.getStatus());

}
}
