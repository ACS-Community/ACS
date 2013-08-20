package com.cosylab.gui.components.r2;

import javax.swing.*;
import java.awt.event.ItemListener;
import java.awt.event.ItemEvent;
import java.awt.*;
import java.util.*;
/**
 * This class provides user-friendly entry of dates. The components consists
 * of three combo boxes containing year, month and day values. All values
 * are validated and always display a valid date. This is then provided by
 * getDate and setDate methods. When setting or getting the date from or to
 * java.util.Date class, only YEAR, MONTH and DAY_OF_MONTH properties are
 * set (@see java.util.Calendar). GregorianCalendar class is used to
 * calculate the dates. Since only the number of years is unlimited, it is
 * set to 20 by default, allowing years 2000 through 2019 inclusively to be
 * chosen. This can, however, be modified.
 * Creation date: (2/4/02 9:53:10 AM)
 * @author: 
 */
public class DateChooser extends javax.swing.JPanel {
	protected  int startingYear = 2000;
	private int numberOfYears = 20;
	
	protected JComboBox daySelector = null;
	protected JComboBox monthSelector = null;
	protected JComboBox yearSelector = null;

	private JLabel dayLabel = null;
	private JLabel monthLabel = null;
	private JLabel yearLabel = null;

	private Calendar calendar = null;

	private class DateChangeListener implements ItemListener {
		public void itemStateChanged(ItemEvent e) {
			if (e.getStateChange() == ItemEvent.SELECTED) {
				DateChooser owner = DateChooser.this;

				if (e.getSource() == owner.daySelector)
					owner.setDay(owner.daySelector.getSelectedIndex()+1);

				if (e.getSource() == owner.monthSelector)
					owner.setMonth(owner.monthSelector.getSelectedIndex());

				if (e.getSource() == owner.yearSelector)
					owner.setYear(owner.yearSelector.getSelectedIndex()+owner.startingYear);
			}
		}
	}
/**
 * Creates a new DateChooser.
 */
public DateChooser() {
	super(new GridBagLayout());

	// must be created first
	calendar = new GregorianCalendar();

	createComponents();

	updateComponents();
	
	DateChangeListener dcl = new DateChangeListener();

	daySelector.addItemListener(dcl);
	monthSelector.addItemListener(dcl);
	yearSelector.addItemListener(dcl);
}
/**
 * Creates and layouts the components that represent this panel.
 * Creation date: (2/4/02 9:56:24 AM)
 */
protected void createComponents() {

	dayLabel = new JLabel("Day");
	add(dayLabel, createConstraints(0, 0, 0.1, 4, 0));
	
	monthLabel = new JLabel("Month");
	add(monthLabel, createConstraints(1, 0, 1.0, 4, 0));

	yearLabel = new JLabel("Year");
	add(yearLabel, createConstraints(2, 0, 0.1, 4, 0));

	daySelector = new JComboBox();
	add(daySelector, createConstraints(0, 1, 0.1, 0, 4));
	
	String[] monthNames = {"January", "February", "March", "April", "May", "June",
						 "July", "August", "September", "October", "November", "December"};
	
	monthSelector = new JComboBox(monthNames);
	add(monthSelector, createConstraints(1, 1, 1.0, 0, 4));

	String[] yearNames = new String[numberOfYears];
	for (int i = 0; i < yearNames.length; i++)
		yearNames[i] = String.valueOf(startingYear+i);
		
	yearSelector = new JComboBox(yearNames);
	add(yearSelector, createConstraints(2, 1, 0.1, 0, 4));

}
/**
 * Internal helper routine returns GridBagConstraints object.<p>
 * Creation date: (2/4/02 9:57:12 AM)
 * @return java.awt.GridBagConstraints
 * @param x int
 * @param y int
 * @param ratio double
 */
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
 * Internal helper routine updates the correct number of days for the
 * selected month.<p>
 * Creation date: (2/4/02 10:37:59 AM)
 */
protected void displayDays() {
	int min = calendar.getActualMinimum(Calendar.DAY_OF_MONTH);
	int max = calendar.getActualMaximum(Calendar.DAY_OF_MONTH);

	int nDays = max - min + 1;

	String[] dayNumbers = new String[nDays];

	for (int i = 0; i<nDays; i++) 
		dayNumbers[i] = String.valueOf(i+min);

	daySelector.setModel(new DefaultComboBoxModel(dayNumbers));
	
}
/**
 * Returns the currently selected date as java.util.Date.
 * Creation date: (2/4/02 12:35:43 PM)
 * @return java.util.Date
 */
public Date getDate() {
	return calendar.getTime();
}
/**
 * Returns number of the selected day in current month, starting with 1.
 * Creation date: (2/4/02 10:58:33 AM)
 * @return int
 */
public int getDay() {
	return calendar.get(Calendar.DAY_OF_MONTH);
}
/**
 * Returns the index of the currently selected month starting with 1.
 * Creation date: (2/4/02 10:56:46 AM)
 * @return int
 */
public int getMonth() {
	return calendar.get(Calendar.MONTH);
}
/**
 * Returns the number of year to display in the year selector.
 * <p>
 * Creation date: (2/10/2002 18:12:00)
 * @return int
 */
public int getNumberOfYears() {
	return numberOfYears;
}
/**
 * Returns the starting year to display in the year selector. No year lower
 * than this can be selected by the user or set using setDate method.
 * <p>
 * Creation date: (2/10/2002 18:12:00)
 * @return int
 */
public int getStartingYear() {
	return startingYear;
}
/**
 * Returns the currently selected year.
 * Creation date: (2/4/02 10:57:25 AM)
 * @return int
 */
public int getYear() {
	return calendar.get(Calendar.YEAR);
}
public static void main(String[] args) {
	JDialog dialog = new JDialog();
	dialog.setModal(true);
	dialog.setSize(200, 100);
	dialog.getContentPane().setLayout(new GridBagLayout());

	DateChooser dc = new DateChooser();

	GridBagConstraints gbc = dc.createConstraints(0, 0, 1.0, 0, 0);
	gbc.fill = GridBagConstraints.BOTH;
	dialog.getContentPane().add(dc, gbc);

	dialog.show();

	System.exit(0);
		
}
/**
 * Sets the date to display.
 * Creation date: (2/4/02 12:36:48 PM)
 * @param date java.util.Date
 */
public void setDate(Date date) {
	calendar.setTime(date);
	updateComponents();
}
/**
 * Sets the day to the specified index. To ensure compatibility with
 * java.util.Calendar class, first day of month is specified as 1.
 * Creation date: (2/4/02 10:51:15 AM)
 * @param index int
 */
public void setDay(int index) {
	if (index != getDay()) {
		int min = calendar.getActualMinimum(Calendar.DAY_OF_MONTH);
		int max = calendar.getActualMaximum(Calendar.DAY_OF_MONTH);

		if (index < min)
			index = min;

		if (index > max)
			index = max;

		calendar.set(Calendar.DAY_OF_MONTH, index);

		daySelector.setSelectedIndex(getDay()-1);
	}	
}
/**
 * Sets the enabled state of this component. If the component is disabled,
 * all the combo boxes are also disabled.
 * Creation date: (2/5/02 5:23:59 PM)
 * @param how boolean
 */
public void setEnabled(boolean how) {
	super.setEnabled(how);
	daySelector.setEnabled(how);
	monthSelector.setEnabled(how);
	yearSelector.setEnabled(how);
}
/**
 * Sets the currently selected month. January has the index of 0.
 * Creation date: (2/4/02 10:36:44 AM)
 */
protected void setMonth(int index) {
	if (index != getMonth()) {
		if (index < 1)
			index = 1;

		if (index > 12)
			index = 12;
		
		calendar.set(Calendar.MONTH, index);

		displayDays();
	}	
}
/**
 * Sets the number of years to display in the year selector. User can only
 * select the years shown in the year selector.
 * <p>
 * Creation date: (2/10/2002 18:19:55)
 * @param newNumberOfYears int
 */
public void setNumberOfYears(int newNumberOfYears) {
	numberOfYears = newNumberOfYears;
}
/**
 * Sets the starting year to be displayed in the year selector. Only the
 * years between startingYear and startingYear+numberOfYears inclusively
 * can be selected by the user or by calling the setDate method.
 * <p>
 * Creation date: (2/10/2002 18:19:55)
 * @param newStartingYear int
 */
public void setStartingYear(int newStartingYear) {
	startingYear = newStartingYear;
}
/**
 * Sets the currently selected year. Years are specified absolutely, although
 * only years between startingYear and numberOfYears will be displayed. If
 * the year is not within this range, the closest value will be set.
 * Creation date: (2/4/02 11:22:21 AM)
 * @param index int
 */
public void setYear(int index) {
	if (index != getYear()) {
		if (index < startingYear)
			index = startingYear;

		if (index > startingYear + numberOfYears)
			index = startingYear + numberOfYears;
		
		calendar.set(Calendar.YEAR, index);	

		displayDays();
	}
}
/**
 * Updates the display after setting the value properties.
 * Creation date: (2/4/02 10:09:05 AM)
 */
private void updateComponents() {
	displayDays();
	
	daySelector.setSelectedIndex(getDay()-1);
	monthSelector.setSelectedIndex(getMonth());
	yearSelector.setSelectedIndex(getYear() - startingYear);
} 
}
