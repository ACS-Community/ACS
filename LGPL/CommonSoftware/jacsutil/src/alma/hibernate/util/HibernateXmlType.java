/**
 * Copyright European Southern Observatory 2010
 */

package alma.hibernate.util;

import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.hibernate.HibernateException;
import org.hibernate.type.StringClobType;
import org.hibernate.usertype.UserType;


/**
 * A hibernate UserType that supports the Oracle XMLTYPE.
 * 
 * Because we store the XMLTYPE as a String in our domain class it seems
 * adequate to inherit {@link StringClobType}'s implementation of everything except:
 * nullSafeGet() and nullSafeSet()
 * <p>
 * To use this class in your domain class add the following above the class
 * declaration (where the &#064;Entity is): <br>
 * &#064;TypeDef(name = "xmltype", typeClass = HibernateXmlType.class)
 * <p>
 * Then add this to the Java Strings that map Oracle XMLTYPEs in the DB: <br>
 * &#064;Type(type = "xmltype")
 * <p>
 * When we integrated Robert's class into ACS, we modified it to use reflection so that 
 * we get no errors if the Oracle jars are not on the classpath. 
 * Then simply the old CLOB behavior will be used.
 * 
 * @author rkurowsk, Jan 22, 2010
 */
public class HibernateXmlType extends StringClobType implements UserType, Serializable {

	private static final String ORACLE_JDBC = "oracle.jdbc";
	private static final String GET_STRING_VAL = "getStringVal";
	private static final String XML_TYPE = "oracle.xdb.XMLType";
	private static final String ORACLE_RESULT_SET = "oracle.jdbc.OracleResultSet";
	private static final long serialVersionUID = 2838406736360323902L;

	/* (non-Javadoc)
	 * @see org.hibernate.type.StringClobType#nullSafeGet(java.sql.ResultSet, java.lang.String[], java.lang.Object)
	 */
	@Override
	public Object nullSafeGet(ResultSet rs, String[] names, Object owner) throws HibernateException, SQLException {
		
		if ( rs.getClass().getName().startsWith(ORACLE_JDBC) ) {
			// result set comes from oracle thin jdbc driver, and thanks to the hibernate annotations
			// we know that the column type is XMLTYPE, so we can simply cast...
			try {
				Class<?> clazz = Class.forName(XML_TYPE);
				Method m = clazz.getMethod(GET_STRING_VAL);

				Object xmlType = rs.getObject(names[0]);
				return (xmlType != null) ? m.invoke(xmlType) : null;
			} catch (Exception e) {
				throw new SQLException("Failed to convert XMLTYPE String to Document for retrieval", e);
			}

		}else{
			// If not Oracle (i.e. HSQLDB) use the hibernate StringClobType impl of nullSafeGet()
			return super.nullSafeGet(rs, names, owner);
		}
		
	}
	
	/* (non-Javadoc)
	 * @see org.hibernate.type.StringClobType#nullSafeSet(java.sql.PreparedStatement, java.lang.Object, int)
	 */
	@Override
	public void nullSafeSet(PreparedStatement st, Object value, int index) throws HibernateException, SQLException {
		
		if ( st.getClass().getName().startsWith(ORACLE_JDBC)  ) {
	        try {
	        	Object xmlType = null;
	            if (value != null) {
	            	Class<?> clazz = Class.forName(XML_TYPE); 
	            	Constructor<?> con = clazz.getConstructor(Connection.class, String.class);
	            	xmlType = con.newInstance(st.getConnection(), (String)value);
	            	st.setObject(index, xmlType);
	            }else{
	            	/* TODO: 2007 is oracle.jdbc.OracleTypes.OPAQUE, use reflection */
	                st.setNull(
	                        index,
	                        2007,
	                        "SYS.XMLTYPE");
	            }
	        }
	        catch (Exception e) {
	            throw new SQLException("Failed to convert Document to XMLTYPE String for storage", e);
	        }

        }else{
			// If not Oracle (i.e. HSQLDB) use the hibernate StringClobType impl of nullSafeSet()
        	super.nullSafeSet(st, value, index);
        }
	}

}
