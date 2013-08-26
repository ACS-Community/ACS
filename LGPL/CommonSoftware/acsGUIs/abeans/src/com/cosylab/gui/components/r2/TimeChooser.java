package com.cosylab.gui.components.r2;

import javax.swing.*;
import java.util.*;
import java.awt.*;
import java.beans.*;

/**
 * Class specialized for entering the time. All fields are validating and allow only
 * entry of valid time. The interface to application is provided through 
 * <code>java.util.Date</code> class. After creation, the component will default to
 * current time. 
 * Creation date: (2/4/02 2:23:26 PM)
 * @author: 
 */
public class TimeChooser extends JPanel {
	protected JIntegerTextField hourChooser;
	protected JIntegerTextField minuteChooser;
	protected JIntegerTextField secondChooser;

	private JLabel separator1;
	private JLabel separator2;

	private JLabel hourLabel;
	private JLabel minuteLabel;
	private JLabel secondsLabel;
	
	private Calendar calendar;


	private class NumberChangeListener implements PropertyChangeListener {
		public void propertyChange(PropertyChangeEvent e) {
			TimeChooser owner = TimeChooser.this;

			if (e.getPropertyName().equals("integerValue")) {
			
				int newValue = ((Integer)e.getNewValue()).intValue();
				if (e.getSource() == owner.hourChooser) {
					owner.setHour(newValue);
				}
				if (e.getSource() == owner.minuteChooser) {
					owner.setMinute(newValue);
				}
				if (e.getSource() == owner.secondChooser) {
					owner.setSeconds(newValue);
				}
			}
		}
	}
/**
 * TimeChooser constructor comment.
 */
public TimeChooser() {
	super(new GridBagLayout());

	calendar = new GregorianCalendar();

	createComponents();

	updateDisplay();
}
/*
 * Creates and layouts the components that represent this panel.
 * Creation date: (2/4/02 2:27:43 PM)
 */
protected void createComponents() {

	NumberChangeListener cl = new NumberChangeListener();
	
	hourLabel = new JLabel("Hours");
	add(hourLabel, createConstraints(0, 0, 0, 4, 0));
	
	hourChooser = new JIntegerTextField();
	hourChooser.addPropertyChangeListener(cl);
	add(hourChooser, createConstraints(0, 1, 1.0, 0, 4));
	hourChooser.setMinimum(0);
	hourChooser.setMaximum(23);

	separator1 = new JLabel(":");
	add(separator1, createConstraints(1, 1, 0, 4, 4));
	
	minuteLabel = new JLabel("Minutes");
	add(minuteLabel, createConstraints(2, 0, 0, 4, 0));
	
	minuteChooser = new JIntegerTextField();
	minuteChooser.addPropertyChangeListener(cl);
	add(minuteChooser, createConstraints(2, 1, 1.0, 0, 4));
	minuteChooser.setMinimum(0);
	minuteChooser.setMaximum(59);

	separator2 = new JLabel(":");
	add(separator2, createConstraints(3, 1, 0, 4, 4));
	
	secondsLabel = new JLabel("Seconds");
	add(secondsLabel, createConstraints(4, 0, 0, 4, 0));

	secondChooser = new JIntegerTextField();
	secondChooser.addPropertyChangeListener(cl);
	add(secondChooser, createConstraints(4, 1, 1.0, 0, 4));
	secondChooser.setMinimum(0);
	secondChooser.setMaximum(59);
	
}	
protected GridBagConstraints createConstraints(int x, int y, double ratio, int top, int bottom) {
	GridBagConstraints constraints = new GridBagConstraints();
	constraints.gridx = x;
	constraints.gridy = y;
	constraints.weightx = ratio;
	constraints.weighty = 0.0;
	constraints.fill = GridBagConstraints.HORIZONTAL;
//	constraints.fill = GridBagConstraints.NONE;
	constraints.insets = new java.awt.Insets(top, 4, bottom, 4);
	constraints.anchor = GridBagConstraints.WEST;
	
	return constraints;
}
/**
 * Returns the currently selected hour. The value returned is between 0 and 23 inclusively.
 * Creation date: (2/4/02 3:14:36 PM)
 * @return int
 */
public int getHour() {
	return calendar.get(Calendar.HOUR_OF_DAY);
}
/**
 * Returns the currently selected minute. The value is in range 0..59 inclusively.
 * Creation date: (2/4/02 3:15:48 PM)
 * @return int
 */
public int getMinute() {
	return calendar.get(Calendar.MINUTE);
}
/**
 * Returns the currently selected seconds. The value is in range 0..59 inclusively.
 * Creation date: (2/4/02 3:16:45 PM)
 * @return int
 */
public int getSeconds() {
	return calendar.get(Calendar.SECOND);
}
/**
 * Returns the currently selected time. Only hour, minute and second portions of the
 * result are defined.
 * Creation date: (2/4/02 3:40:17 PM)
 * @return java.util.Date
 */
public Date getTime() {
	System.out.println("TimeChooser.getTime()" + calendar.getTime().toString());
	return calendar.getTime();
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 2:31:58 PM)
 * @param args java.lang.String[]
 */
public static void main(String[] args) {
	JDialog dialog = new JDialog();
	dialog.setModal(true);
	dialog.setSize(200, 100);
	dialog.getContentPane().setLayout(new GridBagLayout());

	TimeChooser dc = new TimeChooser();

	GridBagConstraints gbc = dc.createConstraints(0, 0, 1.0, 0, 0);
	gbc.fill = GridBagConstraints.BOTH;
	dialog.getContentPane().add(dc, gbc);

	dialog.show();

	System.exit(0);
}
/**
 * Insert the method's description here.
 * Creation date: (2/5/02 5:41:18 PM)
 * @param how boolean
 */
public void setEnabled(boolean how) {
	hourChooser.setEnabled(how);
	minuteChooser.setEnabled(how);
	secondChooser.setEnabled(how);
}
/**
 * Sets the hour. The value will be cropped to the 0..23 range.
 * Creation date: (2/4/02 3:13:00 PM)
 * @param hour int
 */
public void setHour(int hour) {
	if (hour != getHour()) {
		if (hour < 0)
			hour = 0;

		if (hour > 23)
			hour = 23;
			
		calendar.set(Calendar.HOUR_OF_DAY, hour);

		hourChooser.setIntegerValue(hour);
	}
}
/**
 * Sets the minutes of the current time. New value will be cropped to the 0..59 range.
 * Creation date: (2/4/02 3:19:22 PM)
 * @param minute int
 */
public void setMinute(int minute) {
	if (minute != getMinute()) {
		if (minute < 0)
			minute = 0;

		if (minute > 59)
			minute = 59;
		
		calendar.set(Calendar.MINUTE, minute);

		minuteChooser.setIntegerValue(minute);
	}	
}
/**
 * Sets the seconds of the current time. New value will be cropped to the 0..59 range.
 * Creation date: (2/4/02 3:21:45 PM)
 * @param seconds int
 */
public void setSeconds(int seconds) {
	if (seconds != getSeconds()) {
		if (seconds < 0)
			seconds = 0;

		if (seconds > 59)
			seconds = 59;
		
		calendar.set(Calendar.SECOND, seconds);

		secondChooser.setIntegerValue(seconds);
	}	
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:38:43 PM)
 */
public void setTime() {}
/**
 * Sets the time to display. Only the hour, minute and second portion of the date
 * will be used.
 * Creation date: (2/4/02 3:38:43 PM)
 */
public void setTime(Date date) {
	calendar.setTime(date);
	updateDisplay();
}
/**
 * Updates the displayed values after manually setting the time.
 * Creation date: (2/4/02 3:23:33 PM)
 */
protected void updateDisplay() {
	hourChooser.setIntegerValue(getHour());
	minuteChooser.setIntegerValue(getMinute());
	secondChooser.setIntegerValue(getSeconds());
	
}
}
