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
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.ListCellRenderer;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.EtchedBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import alma.acsplugins.alarmsystem.gui.CernSysPanel;
import alma.acsplugins.alarmsystem.gui.sound.AlarmSound;
import alma.acsplugins.alarmsystem.gui.table.AlarmGUIType;
import alma.acsplugins.alarmsystem.gui.table.AlarmTable;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel;

/**
 * The toolbar for the alarm panel
 * 
 * @author acaproni
 *
 */
public class Toolbar extends JPanel implements ActionListener, DocumentListener {
	
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
	private final CernSysPanel alarmPanel;
	
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
	 * The icon for the filter button
	 */
	private ImageIcon filterIcon = new ImageIcon(Toolbar.class.getResource(AlarmGUIType.iconFolder+"filters.png"));
	
	/**
	 * The button to select the alarms matching the content of the <code>searchTF</code> text field
	 */
	private JToggleButton showBtn = new JToggleButton("Show",filterIcon, false);
	
	/**
	 * The button to hide the alarms matching the content of the <code>searchTF</code> text field
	 */
	private JToggleButton hideBtn = new JToggleButton("Hide",filterIcon, false);
	
	/**
	 * The button to pause/unpause the application
	 */
	private JButton pauseBtn = new JButton("Pause",notPausedIcon);

	/**
	 * The label box for auto-acknowledgement of alarms
	 */
	private JLabel autoAckLbl = new JLabel("Auto ack: ");
	
	/**
	 * The text field to write the text to search for in the table
	 */
	private JTextField searchTF = new JTextField(16);
	
	/**
	 * The button to search for the next item
	 */
	private JButton nextSearchBtn = new JButton(new ImageIcon(Toolbar.class.getResource(AlarmGUIType.iconFolder+"resultset_next.png")));
	
	/**
	 * The button to search for the next item
	 */
	private JButton prevSearchBtn = new JButton(new ImageIcon(Toolbar.class.getResource(AlarmGUIType.iconFolder+"resultset_previous.png")));
	
	/**
	 * The table of alarms
	 */
	private final AlarmTable table;
	
	/**
	 * The table model
	 */
	private final AlarmTableModel model;
	
	private final SoundWidget soundComponent;
	
	/**
	 * Constructor
	 * 
	 * @param table The table of alarms
	 * @param model The table model
	 * @param alarmSound The object playing audibles
	 * @param reduce <code>true</code> if the reduction rules are applied at startup
	 * @param panel The panel showing the toolbar
	 */
	public Toolbar(AlarmTable table, AlarmTableModel model, AlarmSound alarmSound, boolean reduce, CernSysPanel panel) {
		super();
		if (table==null) {
			throw new IllegalArgumentException("The table can't be null");
		}
		if (model==null) {
			throw new IllegalArgumentException("The model can't be null");
		}
		if (panel==null) {
			throw new IllegalArgumentException("The panel can't be null");
		}
		soundComponent=new SoundWidget(alarmSound);
		this.table=table;
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
		setLayout(new BoxLayout(this,BoxLayout.LINE_AXIS));
		setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		
		// Add the button to inhibit sounds
		add(soundComponent);
		// Add the label and the combobox for auto ack
		Font fnt = autoAckLbl.getFont();
		Font newFont = fnt.deriveFont(fnt.getSize()*80/100);
		autoAckLbl.setFont(newFont);
		add(Box.createHorizontalStrut(2));
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
		add(Box.createHorizontalStrut(5));
		add(autoAckLevelCB);
		
		activeReductionIcon=new ImageIcon(this.getClass().getResource("/alma/acsplugins/alarmsystem/gui/resources/arrow_in.png"));
		inactiveReductionIcon=new ImageIcon(this.getClass().getResource("/alma/acsplugins/alarmsystem/gui/resources/arrow_out.png"));
		reductionRulesBtn= new JToggleButton("Reduce",activeReductionIcon, reduce);
		reductionRulesBtn.setFont(newFont);
		add(Box.createHorizontalStrut(5));
		add(reductionRulesBtn);
		reductionRulesBtn.addActionListener(this);
		
		add(Box.createHorizontalStrut(5));
		add(pauseBtn);
		pauseBtn.addActionListener(this);
		pauseBtn.setFont(newFont);
		
		add(Box.createHorizontalStrut(5));
		add(new JSeparator(JSeparator.VERTICAL));
		add(Box.createHorizontalStrut(5));
		JLabel searchLbl=new JLabel("Search");
		add(searchLbl);
		searchLbl.setFont(newFont);
		add(Box.createHorizontalStrut(5));
		add(searchTF);
		searchTF.setEditable(true);
		searchTF.getDocument().addDocumentListener(this);
		searchTF.setToolTipText("Search");
		add(Box.createHorizontalStrut(3));
		add(prevSearchBtn);
		prevSearchBtn.setToolTipText("Search prev");
		prevSearchBtn.addActionListener(this);
		add(Box.createHorizontalStrut(3));
		add(nextSearchBtn);
		nextSearchBtn.setToolTipText("Search next");
		nextSearchBtn.addActionListener(this);
		add(Box.createHorizontalStrut(3));
		add(showBtn);
		showBtn.setFont(newFont);
		showBtn.setToolTipText("Filter in");
		showBtn.addActionListener(this);
		
		add(Box.createHorizontalStrut(3));
		add(hideBtn);
		hideBtn.setFont(newFont);
		hideBtn.setToolTipText("Filter out");
		hideBtn.addActionListener(this);
		add(Box.createHorizontalStrut(2));
		
		ratioSearchBtns();
	}
	
	/**
	 * @see ActionListener
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==autoAckLevelCB) {
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
		} else if (e.getSource()==prevSearchBtn) {
			table.search(searchTF.getText(), false);
		} else if (e.getSource()==nextSearchBtn) {
			table.search(searchTF.getText(), true);
		} else if (e.getSource()==showBtn) {
			searchTF.setEnabled(!showBtn.isSelected());
			hideBtn.setEnabled(!showBtn.isSelected());
			if (showBtn.isSelected()) {
				table.filter(searchTF.getText(),false);
			} else {
				table.filter(null,false);
			}
		} else if (e.getSource()==hideBtn) {
			searchTF.setEnabled(!hideBtn.isSelected());
			showBtn.setEnabled(!hideBtn.isSelected());
			if (hideBtn.isSelected()) {
				table.filter(searchTF.getText(),true);
			} else {
				table.filter(null,false);
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
	public void updatePauseBtn(final boolean paused) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				if (paused) {
					pauseBtn.setIcon(pausedIcon);
					pauseBtn.setText("Play");
				} else {
					pauseBtn.setIcon(notPausedIcon);
					pauseBtn.setText("Pause");
				}
			}
		});
	}

	/**
	 * The document listener for the text in the search TF
	 * 
	 * @see DocumentListener
	 */
	@Override
	public void changedUpdate(DocumentEvent e) {
		ratioSearchBtns();
	}

	/**
	 * The document listener for the text in the search TF
	 * 
	 * @see DocumentListener
	 */
	@Override
	public void insertUpdate(DocumentEvent e) {
		ratioSearchBtns();
	}

	/**
	 * The document listener for the text in the search TF
	 * 
	 * @see DocumentListener
	 */
	@Override
	public void removeUpdate(DocumentEvent e) {
		ratioSearchBtns();
	}
	
	/**
	 * Enable/disable the buttons for searching depending
	 * on the content of the text field.
	 * <P>
	 * This is the logic:
	 * <UL>
	 * 	<LI>if the TF is empty the buttons are all disabled
	 *  <LI>if the TF contains test, the buttons are all enabled
	 *  <LI>if the user filters then the TF is disabled
	 *  	(this is done while catching the <code>filterBtn</code> event)
	 *</UL>
	 */
	private void ratioSearchBtns() {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				String text = searchTF.getText();
				prevSearchBtn.setEnabled(text!=null && !text.isEmpty());
				nextSearchBtn.setEnabled(text!=null && !text.isEmpty());
				showBtn.setEnabled(text!=null && !text.isEmpty());
				hideBtn.setEnabled(text!=null && !text.isEmpty());
			}
		});
	}
}
