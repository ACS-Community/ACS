/*
 * ALMA - Atacama Large Millimeter Array
 * (c) European Southern Observatory, 2002
 * (c) Associated Universities Inc., 2002
 * Copyright by ESO (in the framework of the ALMA collaboration),
 * Copyright by AUI (in the framework of the ALMA collaboration),
 * All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY, without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307  USA
 *
 * "@(#) $Id: StringEnumUserType.java,v 1.1 2011/05/13 17:40:37 rtobar Exp $"
 */
package alma.hibernate.util;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Properties;

import org.hibernate.Hibernate;
import org.hibernate.HibernateException;
import org.hibernate.usertype.EnhancedUserType;
import org.hibernate.usertype.ParameterizedType;
import org.hibernate.util.ReflectHelper;

/**
 * Custom mapping type for string-backed enumerations.
 *
 * Taken from "Java Persistence with Hibernate", Christian Bauer and
 * Gavin King, Manning, ISBN 1-932394-88-5.
 *
 * This class was initially used by the handwritten ICD/SharedCode/Persistence
 * TMCDB domain classes. It has been moved down to ACS to be used also by the
 * generated TMCDB pojos.
 */
@SuppressWarnings("unchecked")
public class StringEnumUserType implements EnhancedUserType, ParameterizedType {

	private Class<Enum> enumClass;

    public void setParameterValues(Properties parameters) {
        String enumClassName =
            parameters.getProperty("enumClassName");
        try {
            enumClass = ReflectHelper.classForName(enumClassName);
            try {
				enumClass.getMethod("valueOfForEnum", String.class);
			} catch (SecurityException e) {
			} catch (NoSuchMethodException e) {
				throw new HibernateException("Class '" + enumClass.getCanonicalName() + "' does not implement the valueOfForEnum(String), cannot proceed");
			}
        } catch (ClassNotFoundException cnfe) {
            throw new HibernateException("Enum class not found", cnfe);
        }
    }

    public Class returnedClass() {
        return enumClass;
    }

    public int[] sqlTypes() {
        return new int[] { Hibernate.STRING.sqlType() };
    }

    public boolean isMutable() {
        return false;
    }

    public Object deepCopy(Object value) throws HibernateException {
        return value;
    }

    public Serializable disassemble(Object value) throws HibernateException {
        return (Serializable) value;
    }

    public Object assemble(Serializable cached, Object owner)
            throws HibernateException {
        return cached;
    }

    public Object replace(Object original, Object target, Object owner)
            throws HibernateException {
        return original;
    }

    public boolean equals(Object x, Object y) throws HibernateException {
        if (x == y)
            return true;
        if (x == null || y == null)
            return false;
        return x.equals(y);
    }

    public int hashCode(Object x) throws HibernateException {
        return x.hashCode();
    }

    public Object fromXMLString(String xmlValue) {
        try {
			return enumClass.getMethod("valueOfForEnum", String.class).invoke(enumClass, xmlValue);
		} catch (Exception e) {
			e.printStackTrace();
			throw new HibernateException("Cannot invoke valueOfForEnum(String) in class '" + enumClass.getCanonicalName() + "'");
		}
    }

    public String objectToSQLString(Object value) {
        return '\'' + value.toString() + '\'';
    }

    public String toXMLString(Object value) {
        return value.toString();
    }

    public Object nullSafeGet(ResultSet rs, String[] names, Object owner)
            throws HibernateException, SQLException {
        String name = rs.getString(names[0]);
        try {
			return rs.wasNull() ? null : 
				enumClass.getMethod("valueOfForEnum", String.class).invoke(enumClass, name);
		} catch (Exception e) {
			throw new HibernateException("Cannot invoke valueOfForEnum(String) in class '" + enumClass.getCanonicalName() + "'");
		}
    }

    public void nullSafeSet(PreparedStatement st, Object value, int index)
            throws HibernateException, SQLException {
        if (value == null) {
            st.setNull(index, Hibernate.STRING.sqlType());
        } else {
            st.setString(index, value.toString()); // Enum.toString() must be overriden by enumClass
        }
    }
}