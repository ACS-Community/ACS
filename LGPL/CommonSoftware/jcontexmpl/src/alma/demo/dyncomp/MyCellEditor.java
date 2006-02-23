package alma.demo.dyncomp;

import javax.swing.table.TableCellEditor;
import javax.swing.AbstractCellEditor;
import javax.swing.JButton;
import javax.swing.JTable;
import java.awt.Component;
import javax.swing.event.CellEditorListener;
import java.util.EventObject;

import alma.demo.dyncomp.JDynAct;

/** MyCellEditor is the cell editor that shows the button in the table
* 
* @author Alessandro Caproni, 2003, Nov 7
*/
class MyCellEditor extends AbstractCellEditor implements TableCellEditor {

	JDynAct m_dynActDlg;
	JButton releaseBtn;

	public MyCellEditor(JDynAct dynActDlg) {
		releaseBtn = new JButton(" ");
		m_dynActDlg=dynActDlg;
	}

	/** Return the component to show when the user edit the second column of the table
	*
	* @param table The table that has to be edit
	* @param value The actual value of the cell (not used)
	* @param isSelected
	* @param row The row of the cell
	* @param column The column of the cell
	*
	* @return The button to release the component
	*/
	public Component getTableCellEditorComponent(
			JTable table, 
			Object value, 
			boolean isSelected, 
			int row, 
			int column) {
		if (table.getValueAt(row,column)!=null) {
			releaseBtn.setText("Release "+(String)table.getValueAt(row,0));
			releaseBtn.addActionListener(m_dynActDlg);
			releaseBtn.setVisible(true);
			return releaseBtn;
		} else return null;
	}

	/** Stops the editing so that the button is deleted
	*/
	public void stopEditing() {
		releaseBtn.setVisible(false);
		fireEditingStopped();
	}

	/** Return the value of the just edit cell 
	*
	* @return Always null (we use the cell to show a button not to store a value)
	*/
	public Object getCellEditorValue() { return null; }
}


