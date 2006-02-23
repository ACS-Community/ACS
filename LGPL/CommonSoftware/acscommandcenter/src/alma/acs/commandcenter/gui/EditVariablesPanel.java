/*
 * Created on Nov 18, 2005 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.border.BevelBorder;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;

import alma.acs.commandcenter.app.CommandCenterLogic;
import alma.acs.commandcenter.util.MapTableModel;
import alma.acs.commandcenter.util.PreparedString;



/**
 * Allows to edit the variables.
 */
class EditVariablesPanel extends JPanel {


	// ==================================================
	// Not-so-generic Part: puts gui and logic together
	
	protected Table t1;
	protected Table t2;
	
	protected CommandCenterGui controller;
	
	public EditVariablesPanel(CommandCenterGui controller) {
		super(new BorderLayout());

		t1 = new Table();
		t2 = new Table();

		JPanel p1 = new JPanel();
		p1.setLayout(new BoxLayout(p1, BoxLayout.Y_AXIS));
		JScrollPane s1 = new JScrollPane(t1); 
		JScrollPane s2 = new JScrollPane(t2);
		Border indent = new EmptyBorder(5,5,5,5);
		s1.setBorder(new CompoundBorder(indent, s1.getBorder()));
		s2.setBorder(new CompoundBorder(indent, s2.getBorder()));
		p1.add(Box.createVerticalStrut(10));
		p1.add(new JLabel("Presumptively used in current project"));
		p1.add(Box.createVerticalStrut(5));
		p1.add(s1);
		p1.add(new JLabel("Previously used / Predefined"));
		p1.add(Box.createVerticalStrut(5));
		p1.add(s2);
		p1.add(Box.createVerticalStrut(10));
		this.add(p1, BorderLayout.CENTER);
		
		Dimension size = new Dimension(450, 120);
		t1.setPreferredScrollableViewportSize(size);
		t2.setPreferredScrollableViewportSize(size);
	}


	protected Map writeTo;
	protected Map inProject;
	protected Map inSession;

	public void preShow(Map writeTo, Map inProject, Map inSession) {
		this.writeTo = writeTo;
		this.inProject = inProject;
		this.inSession = inSession;
		
		t1.readModel(inProject);
		t2.readModel(inSession);
	}
	
	public void afterOk() {
		
		t1.writeModel(writeTo);
		t2.writeModel(writeTo);
	}

	

	// ==================================================
	// Generic Table Part (might factor out to a generic class)

	protected class Table extends JTable {
		
		protected Table() {
			super(new MapTableModel("Name", "Value"));
			setShowVerticalLines(false);
			
		}
		
		protected MapTableModel model() {
			return (MapTableModel)getModel();
		}

		public void readModel (Map m) {
			model().setData(m);
		}
	
		public void writeModel(Map m) {
			MapTableModel tableM = model();
			int nRows = tableM.getRowCount();
			for (int row=0; row < nRows ; row++) {
				Object key = tableM.getValueAt(row, 0);
				Object value = tableM.getValueAt(row, 1);
				if (m instanceof Hashtable && value == null) {
					// skip
				} else {
					m.put(key, value);
				}
			}
		}

	}
	
}


