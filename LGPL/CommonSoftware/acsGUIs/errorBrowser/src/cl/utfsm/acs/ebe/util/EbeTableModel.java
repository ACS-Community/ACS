
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

import cl.utfsm.acs.ebe.EbeDocument;
import cl.utfsm.acs.ebe.ErrorBrowserEditor;
import cl.utfsm.acs.types.ComplexObject;
import cl.utfsm.acs.types.SimpleObject;

/**
 * 
 * @author javarias[at]inf.utfsm.cl
 * Implements the table model used in the GUI of
 * Error Browser and Editor. This model is used to
 * show the data of Documents and Error/Completion
 * attributes table.
 *
 */
public class EbeTableModel extends AbstractTableModel {
	private Object[][]tableObjects;
	private TreeMap<String,SimpleObject> attrs=null;
	private ComplexObject complex;
	private ErrorBrowserEditor editor;
	                 
	private static String[] columnNames = {
			"Attribute Name",
			"Attribute Value"
	};
	
	public EbeTableModel(){
	}
	
	public EbeTableModel(ComplexObject complex, ErrorBrowserEditor editor) {
		this.complex=complex;
		this.editor=editor;
		attrs=complex.getAttributes();
		tableObjects = new Object[attrs.size()][2];
		int i = 0;
		for(String key: attrs.keySet()){
			tableObjects[i][0]=key;
			tableObjects[i][1]=attrs.get(key).getValue();
			i++;
		}
	}

	public int getRowCount() {
		if (tableObjects!=null)
			return Array.getLength(tableObjects);
		return 0;
	}

	public int getColumnCount() {
		return 2;
	}

	public Object getValueAt(int rowIndex, int columnIndex) {
		return tableObjects[rowIndex][columnIndex];
	}
	
	public boolean isCellEditable(int row, int col){
		if(col<1)
			return false;
		return true;
	}
	
	public String getColumnName(int c){
		return columnNames[c];
	}
	
	public void setValueAt(Object value, int row, int col){
		String key = tableObjects[row][0].toString();
		tableObjects[row][col]=value;	
		if(!(complex instanceof EbeDocument) && (key.compareTo("name")==0)){
			EbeDocument doc = editor.getSelectedDoc();
			doc.getNodes().remove(complex.getValue());
			complex.setValue(value.toString());
			doc.getNodes().put(complex.getValue(),complex);
			editor.refreshDocsTree();
			editor.publicRefreshNodeTree();
		}
		else if((complex instanceof EbeDocument) && (key.compareTo("name")==0)){
			TreeMap<String,EbeDocument> manager = editor.getManager().getDocuments();
			manager.remove(complex.getValue());
			complex.setValue(value.toString());
			manager.put(complex.getValue(),(EbeDocument)complex);
		}
		SimpleObject o = attrs.get(key);
		o.setValue(value.toString());
		attrs.put(key,o);
	}
}
