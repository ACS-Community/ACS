/** 
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]inf.utfsm.cl)
 * 
 * @since   1.0
 */


package cl.utfsm.acs.ebe.util;

import java.lang.reflect.Array;
import java.util.TreeMap;

import javax.swing.table.AbstractTableModel;

import cl.utfsm.acs.ebe.Error;
import cl.utfsm.acs.ebe.ErrorBrowserEditor;
import cl.utfsm.acs.ebe.Member;
import cl.utfsm.acs.types.SimpleObject;

/**
 * 
 * @author Jorge Avarias (javarias[at]inf.utfsm.cl)
 * 
 * Implements the table model used in the GUI of
 * Error Browser and Editor. This model is used to
 * show the members list with their attributes
 *
 */
public class MembersTableModel extends AbstractTableModel {
	private Object[][]tableObjects;
	private TreeMap<String,Member> members=null;
	private ErrorBrowserEditor editor;
	                 
	private static String[] columnNames = {
			"Name",
			"Type",
			"Description"
	};
	
	public MembersTableModel(){
	}
	
	public MembersTableModel(ErrorBrowserEditor editor) {
		this.editor=editor;
		members=editor.getSelectedError().getMembers();
		tableObjects = new Object[members.size()][3];
		int i = 0;
		for(Member mem: members.values()){
			tableObjects[i][0]=mem.getAttributes().get("name").toString();
			tableObjects[i][1]=mem.getAttributes().get("type").toString();
			tableObjects[i][2]=mem.getAttributes().get("description").toString();
			i++;
		}
	}

	public int getRowCount() {
		if (tableObjects!=null)
			return Array.getLength(tableObjects);
		return 0;
	}

	public int getColumnCount() {
		return 3;
	}

	public Object getValueAt(int rowIndex, int columnIndex) {
		return tableObjects[rowIndex][columnIndex];
	}
	
	public boolean isCellEditable(int row, int col){
		return true;
	}
	
	public String getColumnName(int c){
		return columnNames[c];
	}
	
	public void setValueAt(Object value, int row, int col){
		TreeMap <String,SimpleObject>attrs = members.get(tableObjects[row][0].toString()).getAttributes();
		String oldValue = tableObjects[row][col].toString();
		tableObjects[row][col]=value;	
		if(col==0){
			Error error = editor.getSelectedError();
			Member mem = error.getMembers().get(oldValue);
			error.getMembers().remove(oldValue);
			//Member mem = new Member();
			mem.setValue(value.toString());
			mem.setAttributeValue("name",value.toString());
			error.getMembers().put(mem.getValue(),mem);
			editor.refreshMembersTable();
			return;
		}
		
		if(col==1){
			SimpleObject o = attrs.get("type");
			o.setValue(value.toString());
			attrs.put("type",o);
		}
		if(col==2){
			SimpleObject o = attrs.get("description");
			o.setValue(value.toString());
			attrs.put("description",o);
		}
	}
}

