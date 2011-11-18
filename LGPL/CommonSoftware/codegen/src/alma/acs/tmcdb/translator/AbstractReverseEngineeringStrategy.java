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

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hibernate.cfg.reveng.AssociationInfo;
import org.hibernate.cfg.reveng.DefaulAssociationInfo;
import org.hibernate.cfg.reveng.DelegatingReverseEngineeringStrategy;
import org.hibernate.cfg.reveng.ReverseEngineeringStrategy;
import org.hibernate.cfg.reveng.TableIdentifier;
import org.hibernate.mapping.Column;
import org.hibernate.mapping.ForeignKey;
import org.hibernate.mapping.MetaAttribute;
import org.hibernate.tool.hbm2x.MetaAttributeConstants;

import alma.acs.tmcdb.translator.AbstractTableInheritance.CascadeType;

public abstract class AbstractReverseEngineeringStrategy extends DelegatingReverseEngineeringStrategy {

    private static final String ORACLE_SEQUENCE = "oracle-sequence";
	private static final String IS_XML_CLOB_TYPE = "isXmlClobType";
	private static final String HAS_XML_CLOB_TYPE = "hasXmlClobType";
	private static final String HAS_ENUM_TYPES = "has-enum-types";
	private static final String ENUM_TYPES = "enum-types";
	private static final String IS_SUPER_CLASS = "isSuperClass";
	private static final String IS_ENUM_TYPE = "is-enum-type";
	private static final String ENUM_TYPE_NAME = "enum-type-name";


	protected AbstractColumn2Attribute []columnTranslators;
    protected AbstractTable2Class []tableTranslators;
    protected AbstractTableInheritance []inheritanceTranslators;

	/**
	 * Default constructor, that shouldn't be ever used
	 */
	public AbstractReverseEngineeringStrategy() { super(null); };

	public AbstractReverseEngineeringStrategy(ReverseEngineeringStrategy delegate) { super(delegate); };

	@Override
	public String tableToClassName(TableIdentifier table) {
		String className = null;

		for(int i=0; i!=tableTranslators.length; i++) {
			AbstractTable2Class trans = tableTranslators[i];
			className = trans.getMap().get(table.getName().toLowerCase());
			if( className != null )
					break;
		}

		return className;
	}

	@Override
	public String columnToPropertyName(TableIdentifier table, String column) {
		String propertyName = null;

		for (int i = 0; i != columnTranslators.length; i++) {
			AbstractColumn2Attribute trans = columnTranslators[i];
			Map<String, String> tableMap = trans.getMap().get(
					table.getName().toLowerCase());
			if (tableMap != null) {
				propertyName = tableMap.get(column.toLowerCase());
				if (propertyName != null)
					break;
			}
		}

		return propertyName;
	}

	// Used to provide only Object types (e.g., Integer instead of int)
	@Override
	public String columnToHibernateTypeName(TableIdentifier table,
			String columnName, int sqlType, int length, int precision,
			int scale, boolean nullable, boolean generatedIdentifier) {

		String tableName = table.getName().toLowerCase();
		for (int i = 0; i < inheritanceTranslators.length; i++) {
			if( inheritanceTranslators[i].getEnumTypeForColumn(tableName, columnName.toLowerCase()) != null ) {
				return inheritanceTranslators[i].getEnumTypeForColumn(tableName, columnName.toLowerCase());
			}
		}
		return super.columnToHibernateTypeName(table, columnName, sqlType, length,
				precision, scale, true, generatedIdentifier);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Map tableToMetaAttributes(TableIdentifier tableIdentifier) {
		Map map = super.tableToMetaAttributes(tableIdentifier);

		if( map == null )
			map = new HashMap<String, MetaAttribute>();

		// Check if table contains a XMLCLOB column
		for (int i = 0; i < inheritanceTranslators.length; i++) {
			String tableName = tableIdentifier.getName();
			if( inheritanceTranslators[i].hasXmlClobType(tableName) ) {
				MetaAttribute mattr = new MetaAttribute(HAS_XML_CLOB_TYPE);
				mattr.addValue("true");
				map.put(HAS_XML_CLOB_TYPE,mattr);
				break;
			}
		}

		// Check if table has a generated ID, necessary to generate the GenericGenerator custom annotation
		for (int i = 0; i < inheritanceTranslators.length; i++) {
			String tableName = tableIdentifier.getName();
			String sequence = inheritanceTranslators[i].getSequenceForTable(tableName);
			if( sequence != null ) {
				MetaAttribute mattr = new MetaAttribute(ORACLE_SEQUENCE);
				mattr.addValue(sequence);
				map.put(ORACLE_SEQUENCE,mattr);
				break;
			}
		}

		// Check all CHECK constraints for this table
		for (int i = 0; i < inheritanceTranslators.length; i++) {
			String tableName = tableIdentifier.getName().toLowerCase();
			if( inheritanceTranslators[i].getEnumTypesForTable(tableName) != null ) {

				Map<String, String> typesForTable = inheritanceTranslators[i].getEnumTypesForTable(tableName);
				if( typesForTable == null )
					continue;

				MetaAttribute mattr2 = new MetaAttribute(HAS_ENUM_TYPES);
				mattr2.addValue("true");
				map.put(HAS_ENUM_TYPES, mattr2);

				mattr2 = new MetaAttribute(ENUM_TYPES);
				Iterator<Map.Entry<String, String>> it = typesForTable.entrySet().iterator();
				StringBuilder sb = new StringBuilder();
				while(it.hasNext()) {					
					Map.Entry<String, String> entry = it.next();
					sb.append(entry.getKey());
					sb.append("|");
					sb.append(entry.getValue());
					if(it.hasNext())
						sb.append(",");
				}
				mattr2.addValue(sb.toString());
				map.put(ENUM_TYPES, mattr2);
			}
		}

		// Check if table is superclass or child class
		for (int i = 0; i < inheritanceTranslators.length; i++) {
			String tableName = tableIdentifier.getName().toLowerCase();
			String superClass = inheritanceTranslators[i].getSuperTable(tableName);
			if( superClass != null ) {
				MetaAttribute mattr = new MetaAttribute(MetaAttributeConstants.EXTENDS);
				mattr.addValue(superClass);
				map.put("extends",mattr);
				return map;
			}
			else {
				MetaAttribute mattr = new MetaAttribute(MetaAttributeConstants.EXTENDS);
				mattr.addValue("alma.acs.tmcdb.translator.TmcdbObject");
				map.put("extends",mattr);
				for(int j=0; j!= inheritanceTranslators.length; j++) {
					if( inheritanceTranslators[j].isSuperClass(tableName) ) {
						mattr = new MetaAttribute(IS_SUPER_CLASS);
						mattr.addValue("true");
						map.put(IS_SUPER_CLASS,mattr);
						return map;
					}
				}
			}

		}

		return map;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Map columnToMetaAttributes(TableIdentifier identifier, String column) {

		Map map = super.columnToMetaAttributes(identifier, column);
		if( map == null )
			map = new HashMap<String, MetaAttribute>();

		String tableName = identifier.getName().toLowerCase();

		// Don't generate getter/setters for inheritance-related fields
		MetaAttribute mattr = new MetaAttribute(MetaAttributeConstants.GEN_PROPERTY);
		for (int i = 0; i < inheritanceTranslators.length; i++) {
			List<String> columns = inheritanceTranslators[i].getPkFkCombinationColumns(tableName);
			if( columns != null && columns.contains(column.toLowerCase()) ) {
				mattr.addValue("false");
				map.put("gen-property", mattr);
				return map;
			}
		}

		// Make everything protected
		mattr.addValue("protected");
		map.put("scope-field",mattr);

		// Check for special XMLCLOB types, they get a specific annotation generated
		for (int i = 0; i < inheritanceTranslators.length; i++) {
			if( inheritanceTranslators[i].hasXmlClobType(tableName) ) {
				if( inheritanceTranslators[i].isXmlClobType(tableName, column) ) {
					MetaAttribute mattr2 = new MetaAttribute(IS_XML_CLOB_TYPE);
					mattr2.addValue("true");
					map.put(IS_XML_CLOB_TYPE, mattr2);
					break;
				}
			}
		}

		for (int i = 0; i < inheritanceTranslators.length; i++) {
			if( inheritanceTranslators[i].isKeyPiece(tableName, column) ) {
				MetaAttribute mattr2 = new MetaAttribute("use-in-equals");
				mattr2.addValue("true");
				map.put("use-in-equals", mattr2);
				break;
			}
		}

		return map;
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean excludeForeignKeyAsCollection(String keyname,
			TableIdentifier fromTable, List fromColumns,
			TableIdentifier referencedTable, List referencedColumns) {

		if( excludeForeignKey(keyname, fromTable, fromColumns, referencedTable, referencedColumns, true) )
			return true;
		return super.excludeForeignKeyAsCollection(keyname, fromTable, fromColumns,
				referencedTable, referencedColumns);
	}

	@SuppressWarnings("unchecked")
	private boolean excludeForeignKey(String keyname,
			TableIdentifier fromTable, List fromColumns,
			TableIdentifier referencedTable, List referencedColumns,
			boolean checkDuplicatedFK) {

		for (int i = 0; i < inheritanceTranslators.length; i++) {
			String tName = fromTable.getName().toLowerCase();
			String superTable = inheritanceTranslators[i].getSuperTable(tName);
			String keyName = inheritanceTranslators[i].getKeynameLowercase(tName);
			if( superTable != null &&
					superTable.toLowerCase().equals(referencedTable.getName().toLowerCase()) &&
					keyname.toLowerCase().equals(keyName) ) {
				return true;
			}
			if( checkDuplicatedFK && !inheritanceTranslators[i].generateInverseCollection(tName, ((Column)fromColumns.get(0)).getName() ) )
				return true;
		}
		return false;
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean excludeForeignKeyAsManytoOne(String keyname,
			TableIdentifier fromTable, List fromColumns,
			TableIdentifier referencedTable, List referencedColumns) {
		return excludeForeignKey(keyname, fromTable, fromColumns, referencedTable, referencedColumns, false);
	}

	@Override
	public AssociationInfo foreignKeyToAssociationInfo(
			ForeignKey foreignKey) {
		DefaulAssociationInfo info = new DefaulAssociationInfo();

		String name = foreignKey.getName();
		for (int i = 0; i < inheritanceTranslators.length; i++) {
			CascadeType cType = inheritanceTranslators[i].getCascadeTypeForForeigKey(name);
			if( cType != null ) {
				if( cType == CascadeType.NONE) {
					info.setCascade("none"); // if not set, it produces an EJB3 "all" cascading
					break;
				}
				else if( cType == CascadeType.AGGREGATION ) {
					info.setCascade("save-update, persist, lock");
					break;
				}
				else if( cType == CascadeType.COMPOSITION ) {
					info.setCascade("all-delete-orphan");
					break;
				}
				else {
					info.setCascade("none"); // fallback
					break;
				}
			}
		}

		return info;
	}

	@Override
	public AssociationInfo foreignKeyToInverseAssociationInfo(
			ForeignKey foreignKey) {
		DefaulAssociationInfo info = new DefaulAssociationInfo();

		String name = foreignKey.getName();
		for (int i = 0; i < inheritanceTranslators.length; i++) {
			CascadeType cType = inheritanceTranslators[i].getCascadeTypeForForeigKey(name);
			if( cType != null ) {
				if( cType == CascadeType.AGGREGATION_INVERSE ) {
					info.setCascade("save-update, persist, lock");
					break;
				}
				else if( cType == CascadeType.COMPOSITION_INVERSE ) {
					info.setCascade("all-delete-orphan");
					break;
				}
				else {
					info.setCascade("none"); // fallback
					break;
				}
			}
		}

		return info;
	}


//	@Override
//	public boolean excludeColumn(TableIdentifier identifier, String columnName) {
//
//		String tableName = identifier.getName().toLowerCase();
//		for (int i = 0; i < inheritanceTranslators.length; i++) {
//			List<String> columns = inheritanceTranslators[i].getPkFkCombinationColumns(tableName);
//			if( columns != null && columns.contains(columnName.toLowerCase()) ) {
//				System.out.println("KeyColumns!!!!!!! " + tableName + ":" + columnName);
//				return true;
//			}
//		}
//		return super.excludeColumn(identifier, columnName);
//	}

}
