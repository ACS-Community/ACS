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
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.ListCellRenderer;
import javax.swing.SwingConstants;
import javax.swing.border.EtchedBorder;

import alma.acsplugins.alarmsystem.gui.AlarmPanel;
import alma.acsplugins.alarmsystem.gui.table.AlarmGUIType;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel;

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
		NONE("None",AlarmGUIType.INACTIVE),
		PRIORITY3("Priority 3",AlarmGUIType.PRIORITY_3),
		PRIORITY2("Priority 2",AlarmGUIType.PRIORITY_2),
		PRIORITY1("Priority 1",AlarmGUIType.PRIORITY_1);
		
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
		private ComboBoxValues(String tit, AlarmGUIType color) {
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
	
	/**
	 * The panel showing the toolbar
	 */
	private final AlarmPanel alarmPanel;
	
	/**
	 * The combo box for auto-acknowledgment of alarms
	 */
	private JComboBox autoAckLevelCB=new JComboBox(ComboBoxValues.values());
	
	/**
	 * The icon shown in the button when reduction is active
	 */
	private ImageIcon activeReductionIcon;
	
	/**
	 * The icon shown in the button when reduction is inactive
	 */
	private ImageIcon inactiveReductionIcon;
	
	/**
	 * The check box to activate/deactivate the reduction of alarms
	 */
	private JToggleButton reductionRulesBtn;
	
	/**
	 * The icon shown by <code>pauseBtn</code> when the application is paused
	 */
	private ImageIcon pausedIcon = new ImageIcon(Toolbar.class.getResource(AlarmGUIType.iconFolder+"play.png"));
	
	/**
	 * The icon shown by <code>pauseBtn</code> when the application is not paused
	 */
	private ImageIcon notPausedIcon = new ImageIcon(Toolbar.class.getResource(AlarmGUIType.iconFolder+"pause.png"));
	
	/**
	 * The button to pause/unpause the application
	 */
	private JButton pauseBtn = new JButton("Pause",notPausedIcon);

	/**
	 * The label box for auto-acknowledgement of alarms
	 */
	private JLabel autoAckLbl = new JLabel("Auto ack: ");
	
	/**
	 * 
	 */
	private AlarmTableModel model;
	
	/**
	 * Constructor
	 * 
	 * @param model The table model
	 * @param reduce <code>true</code> if the reduction rules are applied at startup
	 * @param panel The panel showing the toolbar
	 */
	public Toolbar(AlarmTableModel model, boolean reduce, AlarmPanel panel) {
		super();
		if (model==null) {
			throw new IllegalArgumentException("The model can't be null");
		}
		if (panel==null) {
			throw new IllegalArgumentException("The panel can't be null");
		}
		this.model=model;
		this.alarmPanel=panel;
		initialize(reduce);
	}
	
	/**
	 * Initialize the toolbar
	 * 
	 * @param <code>true</code> if the reduction rules are applied at startup
	 */
	private void initialize(boolean reduce) {
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
		
		activeReductionIcon=new ImageIcon(this.getClass().getResource("/alma/acsplugins/alarmsystem/gui/resources/arrow_in.png"));
		inactiveReductionIcon=new ImageIcon(this.getClass().getResource("/alma/acsplugins/alarmsystem/gui/resources/arrow_out.png"));
		reductionRulesBtn= new JToggleButton("Reduce alarms",activeReductionIcon, reduce);
		reductionRulesBtn.setFont(newFont);
		add(reductionRulesBtn);
		reductionRulesBtn.addActionListener(this);
		
		add(pauseBtn);
		pauseBtn.addActionListener(this);
	}
	
	/**
	 * @see ActionListener
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==autoAckLevelCB) {
			System.out.println("Selected item: "+autoAckLevelCB.getSelectedItem());
			model.setAutoAckLevel((ComboBoxValues)autoAckLevelCB.getSelectedItem());
		} else if (e.getSource()==reductionRulesBtn) {
			model.applyReductions(reductionRulesBtn.isSelected());
			if (reductionRulesBtn.isSelected()) {
				reductionRulesBtn.setIcon(activeReductionIcon);
			} else {
				reductionRulesBtn.setIcon(inactiveReductionIcon);
			}
		} else if (e.getSource()==pauseBtn) {
			try {
				if (pauseBtn.getIcon()==notPausedIcon) {
						alarmPanel.pause();
				} else {
					alarmPanel.resume();
				}
			} catch (Throwable t) {
				t.printStackTrace(System.err);
				JOptionPane.showMessageDialog(this, t.getMessage(), "Error pausing/unpausing", JOptionPane.ERROR_MESSAGE);
			}
		} else {
			System.err.println("Invalid source of event: "+e.getSource());
		}
	}
	
	/**
	 * Update the state of the pause button depending on the state 
	 * paused/unpaused of the application
	 * <P>
	 * This method is not called directly by <code>actionPerformed</code> when the button 
	 * is pressed.
	 * It is executed when the application is started/paused.
	 *  
	 * @param paused <code>true</code> if the application is paused 
	 */
	public void updatePauseBtn(boolean paused) {
		if (paused) {
			pauseBtn.setIcon(pausedIcon);
			pauseBtn.setText("Play");
		} else {
			pauseBtn.setIcon(notPausedIcon);
			pauseBtn.setText("Pause");
		}
	}
}
