/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

package alma.acs.gui.widgets;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Calendar;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * A widget to select date and time replacing
 *  <CODE>com.cosylab.gui.components.r2.DateTimeChooser</CODE>.
 * <P>
 * The widget allows the user to select the date and time.
 * 
 * @author acaproni
 * @since ACS 12.1
 */
public class DateTimeSelector extends JComponent implements ActionListener {
	
	/**
	 * The combo box to select the year.
	 */
	private final JComboBox yearCB=new JComboBox();
	
	/**
	 * The combo box to select the month.
	 */
	private final JComboBox monthCB=new JComboBox();
	
	/**
	 * The combo box to select the day of the month.
	 */
	private final JComboBox dayCB=new JComboBox();
	
	/**
	 * The combo box to select the hours.
	 */
	private final JComboBox hoursCB=new JComboBox();
	/**
	 * The combo box to select the minutes.
	 */
	private final JComboBox minsCB=new JComboBox();
	
	/**
	 * The combo box to select the seconds.
	 */
	private final JComboBox secsCB=new JComboBox();
	
	/**
	 * A helper object	
	 */
	private final JComboBox[] comboBoxes = {yearCB, monthCB, dayCB, hoursCB, minsCB, secsCB };
	
	/**
	 * Constructor
	 */
	public DateTimeSelector() {
		initGUI();
	}
	
	/**
	 * Init the GUI
	 */
	private void initGUI() {
		// YearCB is initialized from 2000 to the year next to this on
		Calendar cal = Calendar.getInstance();
		int year = cal.get(Calendar.YEAR);
		for (int t=2000; t<=cal.get(Calendar.YEAR)+1; t++) {
			yearCB.addItem(Integer.valueOf(t));
		}
		// Month
		for (int t=1; t<=12; t++) {
			monthCB.addItem(Integer.valueOf(t));
		}
		
		// The day has its dedicated model
		//dayCB.setModel(new DayComboBoxModel());
		for (int t=1; t<=31; t++) {
			dayCB.addItem(Integer.valueOf(t));
		}
		
		// hour
		for (int t=0; t<24; t++) {
			hoursCB.addItem(Integer.valueOf(t));
		}
		
		// Minutes and seconds
		for (int t=0; t<60; t++) {
			secsCB.addItem(Integer.valueOf(t));
			minsCB.addItem(Integer.valueOf(t));
		}
		
		
		// Add the date panel
		JPanel datePnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		datePnl.setBorder(BorderFactory.createTitledBorder("Date"));
		datePnl.add(new JLabel("Day: "));
		datePnl.add(dayCB);
		datePnl.add(new JLabel("Month: "));
		datePnl.add(monthCB);
		datePnl.add(new JLabel("Year: "));
		datePnl.add(yearCB);
		// Add the time panel 
		JPanel timePnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		timePnl.setBorder(BorderFactory.createTitledBorder("Time"));
		timePnl.add(new JLabel("Hour: "));
		timePnl.add(hoursCB);
		timePnl.add(new JLabel("Minute: "));
		timePnl.add(minsCB);
		timePnl.add(new JLabel("Second: "));
		timePnl.add(secsCB);
		// Add the widgets to the component
		setLayout(new BorderLayout());
		add(datePnl,BorderLayout.NORTH);
		add(timePnl,BorderLayout.SOUTH);
		
		for (JComboBox box: comboBoxes) {
			box.setEditable(false);
			// Initial selection that will be updated by the next setDate
			//
			// Do not add the action listener before selecting a item because actionPerformed calls #ratioDayCB
			// that needs to get a selected item.
			box.setSelectedIndex(0);
			box.addActionListener(this);
		}
		
		setDate(cal);
	}
	
	/**
	 * The days displayed by {@link #dayCB} depends on the month
	 */
	private void ratioDayCB() {
		int selectedMonth=(Integer)monthCB.getSelectedItem();
		int selectedYear=(Integer)yearCB.getSelectedItem();
		
		int maxDays;
		switch (selectedMonth) {
		case 2: {
			// Feb
			maxDays=(selectedYear%4==0) ? 29 :28;
			break;
		}
		case 3: // Mar
		case 4: // Apr
		case 6: //Jun
		case 9: // Sep
		case 11: { //Nov 
			maxDays=30;
			break;
		}
		default: {
			maxDays=31;
		}
		}
		
		DefaultComboBoxModel cbModel = (DefaultComboBoxModel)dayCB.getModel();
		for (int d=29; d<=31; d++) {
			if (d<=maxDays) {
				// This day must be present for the selected month
				if (cbModel.getIndexOf(Integer.valueOf(d))==-1) {
					dayCB.addItem(Integer.valueOf(d));
				}
			} else {
				// This day must be removed for the selected month
				if (cbModel.getIndexOf(Integer.valueOf(d))!=-1) {
					dayCB.removeItem(Integer.valueOf(d));
				}
			}
		}
	}
	
	/**
	 *  Set the date of the widgets to that represented by the passed calendar
	 *  
	 * @param cal The date to set in the widgets
	 */
	public void setDate(Calendar cal) {
		if (cal==null) {
			throw new IllegalArgumentException("Can't set a date from a nul calendar obejct");
		}
		int year=cal.get(Calendar.YEAR);
		int month=cal.get(Calendar.MONTH);
		int day=cal.get(Calendar.DAY_OF_MONTH);
		int hour=cal.get(Calendar.HOUR_OF_DAY);
		int min=cal.get(Calendar.MINUTE);
		int sec=cal.get(Calendar.SECOND);
		
		// Select the current date in the widget
		yearCB.setSelectedItem(Integer.valueOf(year));
		monthCB.setSelectedItem(Integer.valueOf(month)+1);
		dayCB.setSelectedItem(Integer.valueOf(day));
		hoursCB.setSelectedItem(Integer.valueOf(hour));
		minsCB.setSelectedItem(Integer.valueOf(min));
		secsCB.setSelectedItem(Integer.valueOf(sec));
		
		ratioDayCB();
	}
	
	/**
	 * 
	 * @return The date selected by the user
	 */
	public Calendar getDate() {
		Calendar ret = Calendar.getInstance();
		int year=(Integer)yearCB.getSelectedItem();
		int month=(Integer)monthCB.getSelectedItem()-1;
		int day=(Integer)dayCB.getSelectedItem();
		int hr=(Integer)hoursCB.getSelectedItem();
		int min=(Integer)minsCB.getSelectedItem();
		int sec=(Integer)secsCB.getSelectedItem();
		ret.set(year, month, day, hr, min, sec);
		return ret;
	}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==monthCB || e.getSource()==yearCB) {
			ratioDayCB();
		} 
	}
}
