package alma.hibernate.util;

import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.SQLException;

/**
 * Based on Sloan Seaman's version (http://community.jboss.org/wiki/MappingOracleXmlTypetoDocument),
 * which in turn is based upon Spring Frameworks C3P0NativeJdbcExtractor by Juergen Hoeller.
 * 
 * @author Juergen Hoeller (Changes by Sloan Seaman, then by Rodrigo Tobar)
 * @since 1.1.5
 * @see com.mchange.v2.c3p0.C3P0ProxyConnection#rawConnectionOperation
 * @see SimpleNativeJdbcExtractor
 */
public class JdbcNativeExtractor {

	public static Connection getRawConnection(Connection con) {
		return con;
	}

	/**
	 * Retrieve the Connection via C3P0's <code>rawConnectionOperation</code> API,
	 * using the <code>getRawConnection</code> as callback to get access to the
	 * raw Connection (which is otherwise not directly supported by C3P0).
	 * @see #getRawConnection
	 */
	public Connection getNativeConnection(Connection con) throws SQLException {

		if ( con.getClass().getName().startsWith("oracle.jdbc") ||
			 con.getClass().getName().startsWith("org.hsqldb") ) {
			return con;
		}
		else if ( con.getClass().getName().startsWith("com.mchange.v2.c3p0") ) {

			Object oCon = con;
			try {
				Class<?> clazz = Class.forName("com.mchange.v2.c3p0.C3P0ProxyConnection");
				Method rawConnMethod    = getClass().getMethod("getRawConnection", Connection.class);
				Method rawConnOperation = clazz.getMethod("rawConnectionOperation", Method.class, Object.class, Object[].class);
				Object c3p0rawConnection = clazz.getField("RAW_CONNECTION").get(oCon);
				return (Connection) rawConnOperation.invoke(oCon, rawConnMethod, null, new Object[] {c3p0rawConnection});
			}
			catch (Exception ex) {
				ex.printStackTrace();
				throw new SQLException("Error in reflection: "+ex.getMessage());
			}
		}
		else {
			Connection conTmp = con.getMetaData().getConnection();
			if ( conTmp.getClass().getName().contains("oracle.jdbc") )
				return conTmp;
		}

		throw new SQLException("Could not find Native Connection of type OracleConnection");
	}

}