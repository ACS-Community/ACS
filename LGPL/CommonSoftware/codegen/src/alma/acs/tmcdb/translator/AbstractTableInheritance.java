/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.tmcdb.translator;

import java.util.Collection;
import java.util.List;
import java.util.Map;

public abstract class AbstractTableInheritance {

	public enum CascadeType {
		NONE,
		AGGREGATION,
		AGGREGATION_INVERSE,
		COMPOSITION,
		COMPOSITION_INVERSE
	};

	protected Map<String, String> map;
	protected Map<String, String> keymap;
	protected Map<String, List<String>> keyColumnsMap;
	protected Map<String, List<String>> keyPiecesMap;
	protected Map<String, CascadeType> cascadingTypes;
	protected Map<String, List<String>> xmlClobTableColumns;
	protected Map<String, String> sequences;
	protected Map<String, String> duplicatedForeignKeys;
	protected Map<String, Map<String, String>> checkConstraints;

	/**
	 * Returns the Java table name for the given sql-short table name
	 * @param table The child table name, in the sql-short form
	 * @return The parent full original name, null if is has no parents.
	 */
	public String getSuperTable(String table) { return map.get(table); }

	/**
	 * Returns the name for the key defined in the parent table for a given table.
	 * @param table The name of the table
	 * @return The name (in lowercase) of the key column of the supertable, otherwise null
	 */
	public String getKeynameLowercase(String table) { return keymap.get(table); }

	/**
	 * Checks whether the indicated column of the given table is part of the pieces
	 * that are supposed to generate the primary key. In the grammar, these are written
	 * after the GENERATED FROM statement of the key declaration, if any
	 *
	 * @param table The table
	 * @param column The columns
	 * @return Whether the given column participates in the creation of the PK of the mentioned table
	 */
	public boolean isKeyPiece(String table, String column) {

		if( keyPiecesMap.containsKey( table.toLowerCase()) &&
		    keyPiecesMap.get(table.toLowerCase()).contains(column.toLowerCase()) )
			return true;
		return false;
	}

	/**
	 * Returns a Map containing all the columns that are part of the PK/FK combination
	 * of a child table. Since they are defined in the parent class, the child class
	 * should not redefine them.
	 * @param table The name of the table
	 * @return A {@link java.util.List} with the column names (lowercase) of the PK/FK
	 */
	public List<String> getPkFkCombinationColumns(String table) { return keyColumnsMap.get(table); }

	/**
	 * Checks if a table is parent of another one or not
	 * @param table The name of the table, lowercased
	 * @return If the table is super class of another one or not
	 */
	public boolean isSuperClass(String table) {
		table = table.toLowerCase();
		Collection<String> superClasses = map.values();
		for (String superClass : superClasses) {
			if( superClass.toLowerCase().equals(table) )
				return true;
		}
		return false;
	}

	/**
	 * Checks which is the cascading options for the given foreign key
	 * @param name The foreign key name
	 * @return The cascading type for the foreign key
	 */
	public CascadeType getCascadeTypeForForeigKey(String name) { return cascadingTypes.get(name.toLowerCase()); }

	/**
	 * Checks if a table contains at least one column of type XMLCLOB
	 * @param tableName Name of the table
	 * @return Whether the table defines a column of type XMLCLOB in the model
	 */
	public boolean hasXmlClobType(String tableName) { return xmlClobTableColumns.containsKey(tableName.toLowerCase()); }

	/**
	 * Checks if a column of a given table is of type XMLCLOB
	 * @param tableName Name of the table
	 * @param columnName Name of the column
	 * @return Whether the column of the given table is of type XMLCLOB or not
	 */
	public boolean isXmlClobType(String tableName, String columnName) {

		List<String> cols = xmlClobTableColumns.get(tableName.toLowerCase());
		if( cols != null ) {
			return cols.contains(columnName.toLowerCase());
		}
		return false;
	}

	/**
	 * Returns the name of the Oracle sequence for the given table
	 * @param tableName The name of the table
	 * @return The name of the sequence, <code>null</code> if table doesn't have a generated ID
	 */
	public String getSequenceForTable(String tableName) { return sequences.get(tableName.toLowerCase()); }


	/**
	 * Check if the given column in the given table should generate an inverse collection
	 * in the referenced table class or not. This is necessary for the case that we have
	 * A extends B, and C referencing A and B. This would lead to the generation of Set&lt;C&gt;
	 * in both A and B, which translates into an error when hibernate is starting
	 * @param tableName The table
	 * @param columnName The column
	 * @return Whether the generator should generate the inverse collection for the table
	 */
	public boolean generateInverseCollection(String tableName, String columnName ) {
		if( duplicatedForeignKeys.containsKey(tableName.toLowerCase()) )
			if( duplicatedForeignKeys.containsValue(columnName.toLowerCase()))
				return false;
		return true;
	}

	/**
	 * Returns all the columns, and their associated enum classes, that exist
	 * in a given table
	 *
	 * @param tableName The table name
	 * @return A map with columns/enum classes.
	 */
	public String getEnumTypeForColumn(String tableName, String columnName) {
		if( checkConstraints.get(tableName) != null )
			if( checkConstraints.get(tableName).get(columnName) != null )
				return checkConstraints.get(tableName).get(columnName);
		return null;
	}

	/**
	 * Returns all the columns, and their associated enum classes, that exist
	 * in a given table
	 *
	 * @param tableName The table name
	 * @return A map with columns/enum classes.
	 */
	public Map<String, String> getEnumTypesForTable(String tableName) {
		return checkConstraints.get(tableName);
	}
}