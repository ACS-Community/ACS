/* this class is used to move the monitoring sync tool to ACS.
 * Everything is (un)shamefully copied from 
 * ICD/SharedCode/TMCDB/Persistence/src/alma/tmcdb/utils/Hibernateutil.java
 */

package alma.acs.tmcdb.utils;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Properties;

import org.hibernate.SessionFactory;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.cfg.Configuration;

//import alma.archive.database.helpers.wrappers.TmcdbDbConfig;

public class HibernateUtil {
	
    private static Configuration configuration;
    private static SessionFactory sessionFactory;
	
    public static SessionFactory getSessionFactory() {
      // Alternatively, you could look up in JNDI here
        if (sessionFactory == null)
            createTestConfiguration();
        return sessionFactory;
    }
    
    /**
     * Creates a configuration exclusively from hibernate.cfg.xml file.
     * This function is provided for unit tests, where hibernate.cfg.xml
     * is installed in the directory from which tests are run.
     */
    private static void createTestConfiguration() {
      try {
          configuration = new AnnotationConfiguration();
          sessionFactory = configuration.configure().buildSessionFactory();
      } catch (Throwable ex) {
          throw new ExceptionInInitializerError(ex);
      }
    }
    
    /**
     * Creates an Hibernate configuration adding properties to the default
     * configuration. A session factory is created from the combined configuration.
     * 
     * @param properties Extra properties.
     */
    public static void createConfigurationWithProperties(Properties properties) {
        if (sessionFactory == null) {
            try {
                Configuration cnf = new AnnotationConfiguration();
                cnf.configure("tmcdb.hibernate.cfg.xml");
                cnf.addProperties(properties);
                configuration = cnf;
                sessionFactory = configuration.buildSessionFactory();
            } catch (Throwable ex) {
                throw new ExceptionInInitializerError(ex);
            }
        }
    }
    
    /**
     * Creates an Hibernate configuration setting the connection and
     * dialect properties from the TmcdbDbConfig configurator object,
     * which in turns reads the file DbConfig.properties.
     * 
     * SCornejo: Since the MonitoringSyncTool was moved to ACS
     * this class had to be copied and modified accordingly
     * @param conf TMCDB DbConfig Configurator object
     */
//    public static void createConfigurationFromDbConfig(TmcdbDbConfig conf) {
    public static void createConfigurationFromDbConfig(Object conf) {
    	try{
	    	Class<?> TmcdbDbConfigC = Class.forName("alma.archive.database.helpers.wrappers.TmcdbDbConfig");
	    	if (TmcdbDbConfigC.isInstance(conf)){
	    		final Properties props = new Properties();
	    		Method getDialect = null;
	    		Method getDriver = null;
	    		Method getConnectionUrl = null;
	    		Method getUsername = null;
	    		Method getPassword = null ;
	    		try {
	    			getDialect = conf.getClass().getMethod("getDialect");
	    			getDriver = conf.getClass().getMethod("getDriver");
					getConnectionUrl = conf.getClass().getMethod("getConnectionUrl");
					getUsername = conf.getClass().getMethod("getUsername");
					getPassword = conf.getClass().getMethod("getPassword");
	    		}catch (SecurityException ex){
	    			ex.printStackTrace();
	    		}catch (NoSuchMethodException ex){
	    			ex.printStackTrace();
	    		}
	    		try{
	    			props.setProperty("hibernate.dialect",
	    					(String) getDialect.invoke(conf));
	    			props.setProperty("hibernate.connection.driver_class",
	    					(String) getDriver.invoke(conf));
	    			props.setProperty("hibernate.connection.url",
	    					(String) getConnectionUrl.invoke(conf));
	    			props.setProperty("hibernate.connection.username",
	    					(String) getUsername.invoke(conf));
	    			props.setProperty("hibernate.connection.password",
	    					(String) getPassword.invoke(conf));
	    			props.setProperty("hibernate.current_session_context_class",
		                  "thread");
	    			createConfigurationWithProperties(props);
	    		}catch (IllegalArgumentException e) { 
	    			e.printStackTrace();
    			}catch (IllegalAccessException e) {
    				e.printStackTrace();
    			}catch (InvocationTargetException e) { 
    				e.printStackTrace();
    			}
	    		
	//            props.setProperty("hibernate.dialect",
	//                              conf.getDialect());
	//            props.setProperty("hibernate.connection.driver_class",
	//                              conf.getDriver());
	//            props.setProperty("hibernate.connection.url",
	//                              conf.getConnectionUrl());
	//            props.setProperty("hibernate.connection.username",
	//                              conf.getUsername());
	//            props.setProperty("hibernate.connection.password",
	//                              conf.getPassword());
	//    		props.setProperty("hibernate.current_session_context_class",
	//    		                  "thread");
	//            createConfigurationWithProperties(props);
	    	}
    	}catch (ClassNotFoundException ex){
    		ex.printStackTrace();
    	}

		

    	
    	
        
    }
    /**
     * Creates an Hibernate configuration adding properties to the default
     * configuration. A session factory is created from the combined configuration.
     *
     * @param properties Extra properties.
     */
//    public static void createAcsConfigurationFromDbConfig(TmcdbDbConfig conf) {
    public static void createAcsConfigurationFromDbConfig(Object conf) {
        try{
	    	Class<?> TmcdbDbConfigC = Class.forName("alma.archive.database.helpers.wrappers.TmcdbDbConfig");
	    	if (TmcdbDbConfigC.isInstance(conf)){
	    		final Properties props = new Properties();
	    		Method getDialect = null;
	    		Method getDriver = null;
	    		Method getConnectionUrl = null;
	    		Method getUsername = null;
	    		Method getPassword = null ;
	    		try {
	    			getDialect = conf.getClass().getMethod("getDialect");
	    			getDriver = conf.getClass().getMethod("getDriver");
					getConnectionUrl = conf.getClass().getMethod("getConnectionUrl");
					getUsername = conf.getClass().getMethod("getUsername");
					getPassword = conf.getClass().getMethod("getPassword");
	    		}catch (SecurityException ex){
	    			ex.printStackTrace();
	    		}catch (NoSuchMethodException ex){
	    			ex.printStackTrace();
	    		}
	    		try{
	    			props.setProperty("hibernate.dialect",
	    					(String) getDialect.invoke(conf));
	    			props.setProperty("hibernate.connection.driver_class",
	    					(String) getDriver.invoke(conf));
	    			props.setProperty("hibernate.connection.url",
	    					(String) getConnectionUrl.invoke(conf));
	    			props.setProperty("hibernate.connection.username",
	    					(String) getUsername.invoke(conf));
	    			props.setProperty("hibernate.connection.password",
	    					(String) getPassword.invoke(conf));
	    			props.setProperty("hibernate.current_session_context_class",
		                  "thread");
	    			createConfigurationWithProperties(props);
	    		}catch (IllegalArgumentException e) { 
	    			e.printStackTrace();
    			}catch (IllegalAccessException e) {
    				e.printStackTrace();
    			}catch (InvocationTargetException e) { 
    				e.printStackTrace();
    			}
	    	}
        }catch (ClassNotFoundException ex){
    		ex.printStackTrace();
        }
//        props.setProperty("hibernate.dialect",
//                          conf.getDialect());
//        props.setProperty("hibernate.connection.driver_class",
//                          conf.getDriver());
//        props.setProperty("hibernate.connection.url",
//                          conf.getConnectionUrl());
//        props.setProperty("hibernate.connection.username",
//                          conf.getUsername());
//        props.setProperty("hibernate.connection.password",
//                          conf.getPassword());
//		props.setProperty("hibernate.current_session_context_class",
//		                  "thread");
//        createAcsConfigurationWithProperties(props);
    }

    public static void createAcsConfigurationWithProperties(Properties properties) {
        if (sessionFactory == null) {
            try {
                Configuration cnf = new AnnotationConfiguration();
                cnf.configure("cdb_rdb-hibernate.cfg.xml");
                cnf.addProperties(properties);
                configuration = cnf;
                sessionFactory = configuration.buildSessionFactory();
            } catch (Throwable ex) {
                throw new ExceptionInInitializerError(ex);
            }
        }
    } 
}
