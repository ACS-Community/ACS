/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2009
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acsplugins.alarmsystem.gui.detail;

import java.awt.Color;
import java.awt.Component;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.text.SimpleDateFormat;
import java.util.Properties;
import java.util.Set;
import java.util.Vector;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import alma.acs.util.IsoDateFormat;
import alma.acsplugins.alarmsystem.gui.table.AlarmGUIType;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel.AlarmTableColumn;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel.PriorityLabel;

import cern.laser.client.data.Alarm;
import cern.laser.client.data.ResponsiblePerson;
import cern.laser.client.data.Status;
import cern.laser.client.data.Triplet;

/**
 * The table with the details of an alarm 
 * <P>
 * The table has two columns, one with the name of the field and the second with
 * its value.
 * The number of rows can change because the alarm might have user properties. 
 * 
 * @author acaproni
 *
 */
public class AlarmDetailTable extends JTable {
	
	/**
	 * The tiltes of the rows that appear on the left side
	 * of the table.
	 * <i>Note</i>: to change the order of the row, change the declaration
	 * 				order of the items of this enumerated.
	 * 
	 * @author acaproni
	 *
	 */
	public enum RowTitles {
		COMPONENT("Component"), // FM
		TIMESTAMP("Source timestamp "),
		CAUSE("Cause"),
		PRIORITY("Priority"),
		DESCRIPTION("Description"),
		ACTION("Action"),
		CONSEQUENCE("Consequence"),
		STATUS("Status"),
		HOST("Host"),
		URL("Help page:"),
		RESPONSIBLE("Contact"),
		EMAIL("Email"),
		GSM("GSM"),
		CODE("Code"),
		FAMILY("Family"),
		TRIPLET("Triplet"),
		ID("ID");		
		
		/**
		 * The tile of the row
		 */
		private final String title;
		
		/**
		 * The title of the row in HTML, bold.
		 */
		private final String htmlTitle;
		
		/**
		 * Constructor
		 * 
		 * @param The title of the row
		 */
		private RowTitles(String title) {
			this.title=title;
			this.htmlTitle="<HTML><B>"+title+"</B></HTML>";
		}
	}
	
	/**
	 * The model for this table: it display the details of one alarm.
	 * <P>
	 * When the table does not display an alarm, it is blank
	 * 
	 * @author acaproni
	 */
	public class AlarmDetailTableModel extends AbstractTableModel {
		
		/** 
		 * The date format
		 */
		private SimpleDateFormat dateFormat = new IsoDateFormat();
		
		/**
		 * The string shown when there is no alarm
		 */
		private final String blankStr="";

		@Override
		public int getColumnCount() {
			return 2;
		}

		@Override
		public int getRowCount() {
			int ret= RowTitles.values().length;
			if (alarm!=null) {
				Properties props=alarm.getStatus().getUserProperties();
				if (props!=null) {
					ret+=props.size();
				}
			}
			return ret;
		}

		@Override
		public Object getValueAt(int rowIndex, int columnIndex) {
			// Titles
			if (columnIndex==0) {
				if (rowIndex<RowTitles.values().length) {
					return RowTitles.values()[rowIndex].htmlTitle;
				} else {
					// Titles of properties
					return propertyNames.get(rowIndex-RowTitles.values().length);
				}
			}
			if (alarm==null) {
				return blankStr;
			}
			if (rowIndex>=RowTitles.values().length) {
				String key = propertyNames.get(rowIndex-RowTitles.values().length);
				return alarm.getStatus().getUserProperties().getProperty(key);
			}
			switch (RowTitles.values()[rowIndex]) {
			case COMPONENT: return alarm.getTriplet().getFaultMember();
			case CAUSE: return alarm.getCause();
			case DESCRIPTION: return alarm.getProblemDescription();
			case ACTION: return alarm.getAction();
			case CONSEQUENCE: return alarm.getConsequence();
			case EMAIL: return alarm.getPiquetEmail();
			case GSM: return alarm.getPiquetGSM();
			case HOST: {
				Status status= alarm.getStatus();
				if (status!=null) {
					return status.getSourceHostname();
				} else {
					return blankStr;
				}
			}
			case ID: return alarm.getAlarmId();
			case RESPONSIBLE: {
				ResponsiblePerson responsible= alarm.getResponsiblePerson();
				if (responsible!=null) {
					return responsible.getFirstName()+ " "+responsible.getFamilyName();
				} else {
					return blankStr;
				}
			}
			case STATUS: {
				Status st = alarm.getStatus();
				if (st!=null) {
					if (st.isActive()) {
						return "Active";
					} else {
						return "Cleared";
					}
				} else {
					return blankStr;
				}
			}
			case TRIPLET: {
				Triplet triplet = alarm.getTriplet();
				if (triplet!=null) {
					StringBuilder str = new StringBuilder("<");
					str.append(triplet.getFaultFamily());
					str.append(", ");
					str.append(triplet.getFaultMember());
					str.append(", ");
					str.append(triplet.getFaultCode());
					str.append(">");
					return str.toString();
				} else {
					return "Cleared";
				}
			}
			case URL: return alarm.getHelpURL();
			case PRIORITY: {
				int priority=alarm.getPriority().intValue();
				return AlarmTableModel.PriorityLabel.fromPriorityNumber(priority);
			}
			case TIMESTAMP: {
				return dateFormat.format(alarm.getStatus().getSourceTimestamp());
			}
			case FAMILY: return alarm.getTriplet().getFaultFamily();
			case CODE: return alarm.getTriplet().getFaultCode();
			}
			return blankStr;
		}		
	}
	
	/**
	 * The model of the table.
	 */
	private final AlarmDetailTableModel model  = new AlarmDetailTableModel();
	
	/**
	 * The alarm to which this table shows the details.
	 * <P>
	 * If <code>null</code> tha table is cleared.
	 */
	private Alarm alarm=null;
	
	/**
	 * The name of the user properties are stored in a Vector that 
	 * is easier to manipulate from the point of view of the table model.
	 * <P>
	 * It is <code>null</code> if the alarm contains no properties i.e.
	 * it is <code>null</code> ir its size is greater then 0.
	 */
	private Vector<String> propertyNames=null;
	
	/**
	 * The renderer for the table value
	 */
	private final JLabel renderer = new JLabel();
	
	/**
	 * Constructor
	 */
	public AlarmDetailTable() {
		super();
		setModel(model);
		
		initialize();
	}
	
	/**
	 * Init the GUI
	 */
	private void initialize() {
		setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
		setTitleColumnSize(null);
		
		// Remove the table header
		TableColumn col0=getColumnModel().getColumn(0);
		col0.setHeaderValue("Field");
		TableColumn col1=getColumnModel().getColumn(1);
		col1.setHeaderValue("Value");
	}
	
	/**
	 * Calculate the width of the first column to be at least wide 
	 * enough to contain the titles in {@link RowTitles}.
	 * <P>
	 * The width of the column is the greatest between the width needed
	 * to show the title or the passed width
	 * 
	 * @param sz A vector of string (can be <code>null</code>)
	 * @return The width of the first column
	 */
	private int setTitleColumnSize(Vector<String> strings) {
		BufferedImage bImg = new BufferedImage(100,100,BufferedImage.TYPE_INT_RGB);
		Graphics2D g2D= bImg.createGraphics();
		FontMetrics fm=g2D.getFontMetrics();
		int sz=0;
		for (RowTitles row: RowTitles.values()) {
			if (sz<fm.stringWidth(row.title)) {
				sz=fm.stringWidth(row.title);
			}
		}
		if (strings!=null) {
			for (String str: strings) {
				if (sz<fm.stringWidth(str)) {
					sz=fm.stringWidth(str);
				}	
			}
		}
		sz+=20;
		TableColumn col=getColumnModel().getColumn(0);
		col.setPreferredWidth(sz);
		col.setMinWidth(sz);
		col.setMaxWidth(sz);
		col.setWidth(sz);
		col.setResizable(false);
		col=getColumnModel().getColumn(1);
		col.setResizable(true);
		return sz;
	}
	
	@Override
	public String getColumnName(int column) {
		if (column==0) {
			return "Field";
		} else {
			return "Value";
		}
	}

	/**
	 * Set the content of the view with the details of the
	 * passed alarm.
	 * 
	 * @param alarm The alarm to which display the details;
	 * 				if <code>null</code> the table will be cleared
	 */
	public void showAlarmDetails(Alarm alarm) {
		this.alarm=alarm;
		Status st=alarm.getStatus();
		if (st==null) {
			propertyNames=null;
		} else {
			Properties props = st.getUserProperties();
			if (props==null || props.isEmpty()) {
				propertyNames=null;
			} else {
				propertyNames=new Vector<String>();
				Set<String> keys=props.stringPropertyNames();
				for (String key: keys) {
					propertyNames.add(key);
				}
			}
		}
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				setTitleColumnSize(propertyNames);
			}
		});
		model.fireTableDataChanged();
	}
}
