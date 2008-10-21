/*
 * Created on Nov 21, 2005 by mschilli
 */
package alma.acs.commandcenter.util;

import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import javax.swing.table.DefaultTableModel;



public class MapTableModel extends DefaultTableModel {
	
	public MapTableModel(String keyColumnName, String valueColumnName) {
		super();
		this.init(keyColumnName, valueColumnName);
	}
	
	@SuppressWarnings("unchecked")
	protected void init(String keyColumnName, String valueColumnName) {
		super.columnIdentifiers = new Vector(2);
		super.columnIdentifiers.add(keyColumnName);
		super.columnIdentifiers.add(valueColumnName);
	}

	@SuppressWarnings("unchecked")
	public void setData (Map m) {
		Vector<Vector> data = new Vector<Vector>(m.size());
		Iterator it = m.entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry entry = (Map.Entry)it.next();
			Vector row = new Vector(2);
			row.add(entry.getKey());
			row.add(entry.getValue());
			data.add(row);
		}
		
		super.setDataVector(data, super.columnIdentifiers);
	}

	@Override
	public boolean isCellEditable (int row, int column) {
		return (column == 1);
	}
	
	
	
}

