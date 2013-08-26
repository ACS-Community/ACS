package com.cosylab.gui.components.r2;

import javax.swing.*;
import java.awt.*;
import java.util.*;
/**
 * This is a container for DateChooser and TimeChooser components, combining
 * the functionality of both. The components consists of six user-editable
 * fields for year, month, day, hour, minute and seconds. All fields are
 * validating allowing user to select only valid date and time values.
 * These are the obtained using getDate and setDate methods.
 * Creation date: (2/7/02 12:04:48 PM)
 * @author: 
 */
public class DateTimeChooser extends javax.swing.JPanel {
	private DateChooser dateChooser;
	private TimeChooser timeChooser;

	private Calendar calendar = new GregorianCalendar();

/**
 * Constructs a new DateTimeChooser.
 */
public DateTimeChooser() {
	super(new GridBagLayout());
	createComponents();
}
/**
 * Internal helper routine creates all the components.
 * Creation date: (2/7/02 12:12:14 PM)
 */
protected void createComponents() {
	GridBagConstraints c = new GridBagConstraints();
	c.weightx = 1.0;
	c.weighty = 0.0;
	c.fill = GridBagConstraints.HORIZONTAL;
	c.gridx = 0;
	c.insets = new Insets(4, 4, 4, 4);

	c.gridy = 0;
	
	dateChooser = new DateChooser();
	add(dateChooser, c);

	timeChooser = new TimeChooser();
	c.gridy = 1;
	add(timeChooser, c);
}
/**
 * Returns the date displayed by this component. The result is java.util.Date
 * object. Only the following fields are set: SECOND, MINUTE, HOUR_OF_DAY,
 * DAY_OF_MONTH, MONTH, YEAR. For more information on these fields see the
 * java.util.Calendar.
 * Creation date: (2/7/02 12:19:59 PM)
 * @return java.util.Date
 */
public Date getDate() {
	calendar.set(Calendar.SECOND, timeChooser.getSeconds());
	calendar.set(Calendar.MINUTE, timeChooser.getMinute());
	calendar.set(Calendar.HOUR_OF_DAY, timeChooser.getHour());
	calendar.set(Calendar.YEAR, dateChooser.getYear());
	calendar.set(Calendar.MONTH, dateChooser.getMonth());
	calendar.set(Calendar.DAY_OF_MONTH, dateChooser.getDay());
	return calendar.getTime();
}
	public static void main(String[] args) {
		JDialog d = new JDialog();
		DateTimeChooser dtc = new DateTimeChooser();

		GridBagConstraints c = new GridBagConstraints();

		c.fill = GridBagConstraints.BOTH;
		
		d.getContentPane().setLayout(new GridBagLayout());
		d.getContentPane().add(dtc, c);

		d.setModal(true);
		d.show();
		System.out.println(dtc.getDate().toString());
	}
/**
 * Sets the date to display in this selector. Value is set through the
 * java.util.Date object. Only the following fields are considerer: 
 * SECOND, MINUTE, HOUR_OF_DAY, DAY_OF_MONTH, MONTH, YEAR. For more 
 * information on these fields see the java.util.Calendar class.
 * Creation date: (2/7/02 12:19:27 PM)
 * @param date java.util.Date
 */
public void setDate(Date date) {
	dateChooser.setDate(date);
	timeChooser.setTime(date);	
}
/**
 * Sets the enabled state of this component and the components contained in
 * it.
 * Creation date: (2/7/02 12:14:21 PM)
 * @param how boolean
 */
public void setEnabled(boolean how) {
	super.setEnabled(how);
	dateChooser.setEnabled(how);
	timeChooser.setEnabled(how);
}
}
