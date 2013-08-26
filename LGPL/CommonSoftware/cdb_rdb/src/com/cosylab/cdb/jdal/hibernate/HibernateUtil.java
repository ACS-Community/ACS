/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
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
package com.cosylab.cdb.jdal.hibernate;

import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.hibernate.HibernateException;
import org.hibernate.Interceptor;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.cfg.Configuration;


/**
 * Basic Hibernate helper class, handles SessionFactory, Session and Transaction.
 * <p>
 * Holds Session and Transactions in thread local variables. All
 * exceptions are wrapped in an unchecked HibernateUtilException.
 */
@SuppressWarnings("unchecked")
public class HibernateUtil {

	
	@SuppressWarnings("serial")
	public static class HibernateUtilException extends Exception
	{
		public HibernateUtilException(String message, Throwable cause) {
			super(message, cause);
		}

		public HibernateUtilException(Throwable cause) {
			super(cause);
		}
		
	}
	
	/**
	 * Singleton accessor
	 * @return
	 */
	public static synchronized HibernateUtil getInstance(Logger logger) {
		if (instance == null) {
			instance = new HibernateUtil(logger);
		}
		return instance;
	}

	/**
	 * Nulls the instance field, so that subsequent calls to {@link #getInstance(Logger)} will create a new instance.
	 * This probably only makes sense between junit tests that want to start out fresh.
	 */
	public static void clearInstance() {
		instance = null;
	}
	
	private static HibernateUtil instance;

	private final Logger logger;

    private Configuration configuration;
    private SessionFactory sessionFactory;
    private final ThreadLocal threadSession = new ThreadLocal();
    private final ThreadLocal threadTransaction = new ThreadLocal();
    private final ThreadLocal threadInterceptor = new ThreadLocal();
    
        
    
    private static final String HIBERNATE_FILENAME_KEY = "cdb_rdb.hibernate.cfg.filename";
    public static final String HIBERNATE_FILENAME_DEFAULT = "cdb_rdb-hibernate.cfg.xml";
    
    private final String getConfigurationFileName() 
    {
    	String ret = System.getProperty(HIBERNATE_FILENAME_KEY, HIBERNATE_FILENAME_DEFAULT);
    	logger.fine("Will try to load hibernate config file '" + ret + "' from the classpath.");
    	return ret;
    }


	private HibernateUtil(Logger logger) {
		this.logger = logger;
	    // Create the initial SessionFactory from the default configuration files
        try {
            configuration = new AnnotationConfiguration();
            sessionFactory = configuration.configure(getConfigurationFileName()).buildSessionFactory();
            // We could also let Hibernate bind it to JNDI:
            // configuration.configure().buildSessionFactory()
        } catch (Throwable ex) {
        	// @TODO HSO: now that we moved this code from static {} initializer to the ctor,
        	//            should we throw or at least log the exception?
        	
            // We have to catch Throwable, otherwise we will miss
            // NoClassDefFoundError and other subclasses of Error
        	
        	// commented out - will be provided dynamically for the tests...
            //log.error("Building SessionFactory failed.", ex);
            //throw new ExceptionInInitializerError(ex);
        }
	}

	/**
	 * Use default configuration and add properties from Properties.
	 * Build session factory from combined configuration.
	 * @param config
	 */
	public void setConfiguration(Properties extraProperties) {
        try {
            Configuration config = new AnnotationConfiguration();
            config.configure(getConfigurationFileName());
            config.addProperties(extraProperties);
            sessionFactory = config.buildSessionFactory();
            configuration = config;
        } catch (Throwable ex) {
            // We have to catch Throwable, otherwise we will miss
            // NoClassDefFoundError and other subclasses of Error
        	logger.log(Level.SEVERE, "Building SessionFactory failed.", ex);
            throw new ExceptionInInitializerError(ex);
        }
    }

	/**
	 * Set your own configuration and build session factory from it.
	 * Used to tests.
	 * @param config
	 */
	public void setConfiguration(Configuration config) {
        try {
        	sessionFactory = config.buildSessionFactory();
        	configuration = config;
        } catch (Throwable ex) {
            // We have to catch Throwable, otherwise we will miss
            // NoClassDefFoundError and other subclasses of Error
        	logger.log(Level.SEVERE, "Building SessionFactory failed.", ex);
            throw new ExceptionInInitializerError(ex);
        }
    }

	/**
     * Returns the SessionFactory used for this static class.
     *
     * @return SessionFactory
     */
    public SessionFactory getSessionFactory() {
                /* Instead of a static variable, use JNDI:
                SessionFactory sessions = null;
                try {
                        Context ctx = new InitialContext();
                        String jndiName = "java:hibernate/HibernateFactory";
                        sessions = (SessionFactory)ctx.lookup(jndiName);
                } catch (NamingException ex) {
                        throw new HibernateUtilException(ex);
                }
                return sessions;
                 */
        return sessionFactory;
    }
    
    /**
     * Returns the original Hibernate configuration.
     *
     * @return Configuration
     */
    public Configuration getConfiguration() {
        return configuration;
    }
    
    /**
     * Rebuild the SessionFactory with the static Configuration.
     *
     */
    public void rebuildSessionFactory()
    throws HibernateUtilException {
        synchronized(sessionFactory) {
            try {
                sessionFactory = getConfiguration().buildSessionFactory();
            } catch (Exception ex) {
                throw new HibernateUtilException(ex);
            }
        }
    }
    
    /**
     * Rebuild the SessionFactory with the given Hibernate Configuration.
     *
     * @param cfg
     */
    public void rebuildSessionFactory(Configuration cfg)
    throws HibernateUtilException {
        synchronized(sessionFactory) {
            try {
                sessionFactory = cfg.buildSessionFactory();
                configuration = cfg;
            } catch (Exception ex) {
                throw new HibernateUtilException(ex);
            }
        }
    }
    
    /**
     * Retrieves the current Session local to the thread.
     * <p/>
     * If no Session is open, opens a new Session for the running thread.
     *
     * @return Session
     */
	public Session getSession()
    throws HibernateUtilException {
        Session s = (Session) threadSession.get();
        try {
            if (s == null) {
                //log.debug("Opening new Session for this thread.");
                //System.err.println("Opening new Session for this thread.");
                if (getInterceptor() != null) {
                    //System.err.println("Using interceptor: " + getInterceptor().getClass());
                    s = getSessionFactory().openSession(getInterceptor());
                } else {
                    //System.err.println("Without interceptor");
                    s = getSessionFactory().openSession();
                }
                threadSession.set(s);
            }
        } catch (HibernateException ex) {
            throw new HibernateUtilException(ex);
        }
        return s;
    }
    
    /**
     * Closes the Session local to the thread.
     */
    public void closeSession()
    throws HibernateUtilException {
        try {
            Session s = (Session) threadSession.get();
            threadSession.set(null);
            // Added to correctly handle situations where persist() alone cause an exception and commitTransaction() or rollbackTransactions() doesn't occur
            // (for example where we have unique index constraints)
            if (threadTransaction.get() != null)
                rollbackTransaction();
            
            if (s != null && s.isOpen()) {
                //System.err.println("Closing Session of this thread.");
                s.close();
            }
        } catch (HibernateException ex) {
            throw new HibernateUtilException(ex);
        }
    }
    
    /**
     * Start a new database transaction.
     */
    public void beginTransaction()
    throws HibernateUtilException {
        Transaction tx = (Transaction) threadTransaction.get();
        try {
            if (tx == null) {
                //System.err.println("Starting new database transaction in this thread.");
                tx = getSession().beginTransaction();
                threadTransaction.set(tx);
            }
        } catch (HibernateException ex) {
            throw new HibernateUtilException(ex);
        }
    }
    
    /**
     * Commit the database transaction.
     */
    public void commitTransaction()
    throws HibernateUtilException {
        Transaction tx = (Transaction) threadTransaction.get();
        try {
            if ( tx != null && !tx.wasCommitted()
            && !tx.wasRolledBack() ) {
                //System.err.println("Committing database transaction of this thread.");
                tx.commit();
            }
            threadTransaction.set(null);
        } catch (HibernateException ex) {
            rollbackTransaction();
            throw new HibernateUtilException(ex);
        }
    }
    
    /**
     * Commit the database transaction.
     */
    public void rollbackTransaction()
    throws HibernateUtilException {
        Transaction tx = (Transaction) threadTransaction.get();
        try {
            threadTransaction.set(null);
            if ( tx != null && !tx.wasCommitted() && !tx.wasRolledBack() ) {
                //System.err.println("Tyring to rollback database transaction of this thread.");
                tx.rollback();
            }
        } catch (HibernateException ex) {
            throw new HibernateUtilException(ex);
        }
    }
    
    /**
     * Reconnects a Hibernate Session to the current Thread.
     *
     * @param session The Hibernate Session to be reconnected.
     */
    @SuppressWarnings("deprecation")
	public void reconnect(Session session)
    throws HibernateUtilException {
        try {
            session.reconnect();
            threadSession.set(session);
        } catch (HibernateException ex) {
            throw new HibernateUtilException(ex);
        }
    }
    
    /**
     * Disconnect and return Session from current Thread.
     *
     * @return Session the disconnected Session
     */
    public Session disconnectSession()
    throws HibernateUtilException {
        
        Session session = getSession();
        try {
            threadSession.set(null);
            if (session.isConnected() && session.isOpen())
                session.disconnect();
        } catch (HibernateException ex) {
            throw new HibernateUtilException(ex);
        }
        return session;
    }
    
    /**
     * Register a Hibernate interceptor with the current thread.
     * <p>
     * Every Session opened is opened with this interceptor after
     * registration. Has no effect if the current Session of the
     * thread is already open, effective on next close()/getSession().
     */
    public void registerInterceptor(Interceptor interceptor) {
        threadInterceptor.set(interceptor);
    }
    
    private Interceptor getInterceptor() {
        Interceptor interceptor =
                (Interceptor) threadInterceptor.get();
        return interceptor;
    }
    
    
	public List getList(Class type) throws HibernateUtilException
	{
		List result = null;
        try{
        	beginTransaction();
            Session session = getSession();
            
            result = session.createCriteria(type).list();
            
            commitTransaction();
        }
        catch (Throwable thr) {
        	throw new HibernateUtilException(thr);
        }
        finally {
            closeSession();
        }
        return result;
	}
    
    
}

