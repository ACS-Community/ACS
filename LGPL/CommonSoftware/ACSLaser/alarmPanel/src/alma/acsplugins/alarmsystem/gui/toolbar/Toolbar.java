/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
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
package alma.acsplugins.alarmsystem.gui.toolbar;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;
import javax.swing.SwingConstants;
import javax.swing.border.EtchedBorder;

import alma.acsplugins.alarmsystem.gui.AlarmTableModel;
import alma.acsplugins.alarmsystem.gui.CellColor;

/**
 * The toolbar for the alarm panel
 * 
 * @author acaproni
 *
 */
public class Toolbar extends JPanel implements ActionListener {
	
	/** 
	 * The rendered for the auto acknowledge combo box
	 * This renderer shows each level with it color as defined in CellColor
	 * 
	 * @author acaproni
	 *
	 */
	public class ComboBoxRenderer implements ListCellRenderer {
		
		// The label shown by the combo box (i.e. the text of the 
		// selected item)
		private JLabel selectedLabel=new JLabel();

		/**
		 * Constructor
		 */
		public ComboBoxRenderer() {
			selectedLabel.setOpaque(false);
			Dimension d = new Dimension(ComboBoxValues.getWidth(),ComboBoxValues.getHeight());
			selectedLabel.setMinimumSize(d);
		}

		/**
		 * @see ListCellRenderer
		 */
		public Component getListCellRendererComponent(JList list, Object value,
				int index, boolean isSelected, boolean cellHasFocus) {
			if (index==-1) {
				System.out.println("Returning -1 "+(ComboBoxValues)value+" selected: "+isSelected);
				ComboBoxValues val = (ComboBoxValues)value;
				selectedLabel.setText(val.title);
				selectedLabel.setBackground(val.normalRenderer.getBackground());
				selectedLabel.setForeground(val.normalRenderer.getForeground());

				return selectedLabel;
			}
			if (isSelected) {
				return ((ComboBoxValues)value).selectedRenderer;
			} else {
				return ((ComboBoxValues)value).normalRenderer;
			}
		}
		
	}
	
	/**
	 * The values  shown in the ComboBox.
	 * It contains the labels to use as renderer for each cell.
	 * One label is for the normal situation and the second one
	 * is used when the cell has focus (inverted colors)
	 * 
	 * @author acaproni
	 *
	 */
	public enum ComboBoxValues {
		NONE("None",CellColor.INACTIVE),
		PRIORITY3("Priority 3",CellColor.PRI_3),
		PRIORITY2("Priority 2",CellColor.PRI_2),
		PRIORITY1("Priority 1",CellColor.PRI_1);
		
		public final String title;
		public final JLabel normalRenderer;
		public final JLabel selectedRenderer;
		
		// Width and height of the label
		// They are calculated from the dimension of the strings
		// to show
		private static int height=0;
		private static int width=0;
		
		/**
		 * Constructor
		 * 
		 * @param title
		 * @param color
		 */
		private ComboBoxValues(String tit, CellColor color) {
			title=tit;
			normalRenderer = new JLabel(tit);
			normalRenderer.setBackground(color.backg);
			normalRenderer.setForeground(color.foreg);
			normalRenderer.setHorizontalAlignment(SwingConstants.CENTER);
			normalRenderer.setVerticalAlignment(SwingConstants.CENTER);
			normalRenderer.setOpaque(true);
			Font fnt = normalRenderer.getFont();
			Font newFont = fnt.deriveFont(fnt.getSize()*80/100);
			normalRenderer.setFont(newFont);
			selectedRenderer = new JLabel(tit);
			selectedRenderer.setBackground(color.foreg);
			selectedRenderer.setForeground(color.backg);
			selectedRenderer.setHorizontalAlignment(SwingConstants.CENTER);
			selectedRenderer.setVerticalAlignment(SwingConstants.CENTER);
			selectedRenderer.setFont(newFont);
			selectedRenderer.setOpaque(true);
			selectedRenderer.setBorder(BorderFactory.createLineBorder(color.backg));
		}
		
		/**
		 * Init the sizes of all the labels
		 */
		public static void initSizes() {
			for (ComboBoxValues val: ComboBoxValues.values()) {
				Font f = val.normalRenderer.getFont();
				FontMetrics fm = val.normalRenderer.getFontMetrics(f);
				int h = fm.getHeight()+5;
				int w = fm.charsWidth(val.title.toCharArray(), 0, val.title.length())+10;
				setHeight(h);
				setWidth(w);
			}
			
			Dimension d = new Dimension(ComboBoxValues.getWidth(),ComboBoxValues.getHeight());
			for (ComboBoxValues val: ComboBoxValues.values()) {
				val.normalRenderer.setPreferredSize(d);
				val.selectedRenderer.setPreferredSize(d);
				val.normalRenderer.setMinimumSize(d);
				val.selectedRenderer.setMinimumSize(d);
			}
			
			System.out.println(d);
		}
		
		/**
		 * Return the height of each label
		 * 
		 * @return The height of each lable
		 */
		public static int getHeight() {
			return height;
		}

		/**
		 * Return the width of each label
		 * 
		 * @return The width of each label
		 */
		public static int getWidth() {
			return width;
		}

		/**
		 * Set the height (static fields can't be
		 * called directly by the constructor)
		 * 
		 * @param height The new height
		 */
		private static void setHeight(int height) {
			if (height>ComboBoxValues.height) {
				ComboBoxValues.height = height;
			}
		}

		/**
		 * Set the width (static fields can't be
		 * called directly by the constructor)
		 * 
		 * @param width The new width
		 */
		private static void setWidth(int width) {
			if (width>ComboBoxValues.width) {
				ComboBoxValues.width = width;
			}
		}
	}
	
	// The combo box for autoacknowledgement of alarms
	private JComboBox autoAckLevelCB=new JComboBox(ComboBoxValues.values());

	// The label box for autoacknowledgement of alarms
	private JLabel autoAckLbl = new JLabel("Auto ack: ");
	
	// The table model
	private AlarmTableModel model;
	
	/**
	 * Constructor
	 */
	public Toolbar(AlarmTableModel model) {
		super();
		if (model==null) {
			throw new IllegalArgumentException("the model can't be null");
		}
		this.model=model;
		initialize();
	}
	
	/**
	 * Initialize the toolbar
	 */
	private void initialize() {
		FlowLayout layout = (FlowLayout)getLayout();
		layout.setAlignment(FlowLayout.LEFT);
		setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		
		// Add the label and the combobox for auto ack
		Font fnt = autoAckLbl.getFont();
		Font newFont = fnt.deriveFont(fnt.getSize()*80/100);
		autoAckLbl.setFont(newFont);
		add(autoAckLbl);
		autoAckLevelCB.setFont(newFont);
		autoAckLevelCB.setEditable(false);
		autoAckLevelCB.setOpaque(true);
		// Set the colors of the renderers
		ComboBoxValues.initSizes();
		autoAckLevelCB.setRenderer(new ComboBoxRenderer());
		autoAckLevelCB.setSelectedIndex(ComboBoxValues.NONE.ordinal());
		autoAckLevelCB.setMaximumRowCount(ComboBoxValues.values().length);
		autoAckLevelCB.setEditable(false);
		autoAckLevelCB.addActionListener(this);
		Dimension d = new Dimension(ComboBoxValues.getWidth(),ComboBoxValues.getHeight());
		autoAckLevelCB.setMinimumSize(d);
		add(autoAckLevelCB);
	}
	
	/**
	 * @see ActionListener
	 */
	public void actionPerformed(ActionEvent e) {
		System.out.println("Selected item: "+autoAckLevelCB.getSelectedItem());
		model.setAutoAckLevel((ComboBoxValues)autoAckLevelCB.getSelectedItem());
	}
}
