/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.container.archive;

import java.util.Random;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.acs.container.ContainerServices;
import alma.xmlstore.Identifier;
import alma.xmlstore.IdentifierHelper;


/**
 * Encapsulates access to the archive, currently only to the Identifier component
 * of the archive.
 * <p>
 * Also provides two failover mechanisms to be used during development. 
 * <ol>
 * <li>If connection to the archive fails, will try to instantiate and use a user-supplied
 * {@link UniqueIdDepot}
 * <li>if that also fails, will use random numbers instead if real IDs.
 * </ol>
 * <p>
 * This class should not be instantiated before the container has become operational, 
 * having access to the Manager and central logger etc.
 * 
 * @author hsommer
 */
public class ArchiveProxy 
{
	private static ArchiveProxy s_instance;
	
	private Logger m_logger;

	private ContainerServices m_contServ;
	
	private volatile Uid m_uid;

	// if true, use random ids instead of really unique ids from the archive
	private boolean m_substituteRandomIds = true;  		

	

	/**
	 * Singleton accessor
	 * @param managerProxy
	 * @return ArchiveProxy
	 */
	public static synchronized ArchiveProxy getArchiveProxy(ContainerServices contServ, Logger logger)
	{ 
		if (s_instance == null)
		{
			s_instance = new ArchiveProxy(contServ, logger);
		}
		return s_instance;
	}
	
	
	/**
	 * private singleton ctor
	 */
	private ArchiveProxy(ContainerServices contServ, Logger logger)
	{
		if (contServ == null)
		{
			throw new IllegalArgumentException(
				"containerServices must not be null.");
		}
		// multithreading: ensure values are copied to main memory 
		synchronized (this) {
			m_contServ = contServ;
			m_logger = logger;
		}
	}
	

	/**
	 * Gets a Uid, which actually manages a range of unique entity ids rather 
	 * than a single one. 
	 * First tries to get it from the archive, and then, depending on 
	 * <code>m_substituteRandomIds</code>, either uses a random <code>long</code> number 
	 * for the global part of the Uid, or throws an <code>ArchiveNotAvailableException</code>.
	 * 
	 * To get access to the archive, the ACS manager is queried for instances.
	 * The first such instance will be used. The connection to the archive component
	 * will be released right after the Uid string has been retrieved, since
	 * a Uid object will last us for some 2^32 unique ids. 
	 * 
	 * @return Uid
	 * @throws ArchiveNotAvailableException
	 * @throws UniqueIdException
	 */
	private Uid getUid() throws ArchiveNotAvailableException, UniqueIdException
	{
		Uid uid = null;
		
		try
		{
			m_logger.fine("trying to access the identifier archive...");
			
			org.omg.CORBA.Object obj = null;

			// trying the new "default component" mechanism...
			obj = m_contServ.getDefaultComponent("IDL:alma/xmlstore/Identifier:1.0");

// leaving the old code in here, in case we have to quickly move back			
//			// query the manager for instances of the Identifier archive
//			String[] curls = m_contServ.findComponents("*", "IDL:alma/xmlstore/Identifier:1.0");
//			
//			if (curls.length > 0)
//			{
//				// let's just use the first identifier archive in case there are many...
//				String archiveCurl = curls[0];
//				
//				org.omg.CORBA.Object obj = m_contServ.getComponent(archiveCurl);
//				
				Identifier identifierArchive = IdentifierHelper.narrow(obj);
				
				// get the ID space
				m_logger.fine("about to call getIdNamespace()...");
				String uidString = identifierArchive.getIdNamespace();
				m_logger.fine("getIdNamespace() returned " + uidString);
					
				m_contServ.releaseComponent(identifierArchive.name());

				uid = new Uid(uidString);
//			}
//			else
//			{
//				m_logger.warning("MicroArchive/Identifier not available; check your CDB settings.");
//			}
		}
		catch (Exception ex)
		{
			m_logger.log(Level.WARNING, "failed to retrieve unique entity id from the archive...", ex);
		}
		
		// first fallback: user-supplied handler
		if (uid == null)
		{
			String uidDepotClassName = System.getProperty(UniqueIdDepot.IMPL_CLASS_PROPERTY);
			if (uidDepotClassName != null)
			{
				m_logger.fine("will try to get unique id range from user-supplied class " + uidDepotClassName);
				try
				{
					UniqueIdDepot depot = (UniqueIdDepot) Class.forName(uidDepotClassName).newInstance();
					String uidString = depot.getUniqueIdBase();
					uid = new Uid(uidString);
					m_logger.fine("got unique id range starting at " + uid.toString());
					
					// reserve 256 IDs and write back the new range
					int currentLocal = uid.getLocalIdentifier();
					int nextLocal = currentLocal + 256;
					uid.setLocalIdentifierMaxValue(nextLocal);
					String newUidString = uid.toString(uid.getGlobalIdentifier(), nextLocal);
					depot.storeUniqueIdBase(newUidString);
					m_logger.fine("stored back next unique id range starting at " + newUidString);
				}
				catch (Exception e)
				{
					m_logger.log(Level.FINE, "failed to get or store back the unique id range ", e);
				}
			}
		}
		
		// second fallback: random numbers
		if (uid == null)
		{
			if (m_substituteRandomIds)
			{
				m_logger.info("will fake unique id with random numbers...");
				uid = new Uid();
				Random random = new Random(System.currentTimeMillis());		
				uid.setGlobalIdentifier(random.nextLong());
			}
			else 
			{
				throw new ArchiveNotAvailableException("failed to retrieve unique entity id from the archive.");
			}
		}
		return uid;
	}
	
	
	/**
	 * Gets a new unique id.
	 * Depending on <code>setSubstituteRandomIds</code>, the id may only be very
	 * likely unique in case connection to the archive failed and random numbers
	 * will be used instead.
	 *   
	 * @return String
	 * @throws ArchiveNotAvailableException
	 * @throws UniqueIdException
	 */
	public String nextUniqueId() 
		throws ArchiveNotAvailableException, UniqueIdException
	{
		if (m_uid == null)
		{
			m_uid = getUid();
		}
		
		String nextId = m_uid.toString();
		
		if (m_uid.canIncrementLocalIdentifier())
		{
			m_uid.incrementLocalIdentifier();
		}
		else
		{
			// make sure we'll get a new one next time
			m_uid = null;
		}
		
		return nextId;
	}
	
	
	/**
	 * Returns the substituteRandomIds.
	 * @return boolean
	 */
	public boolean doesSubstituteRandomIds()
	{
		return m_substituteRandomIds;
	}

	/**
	 * Controls whether only the archive should be used to retrieve a unique id,
	 * or if random numbers may be used as a failover mechanism during development.
	 * The default setting is <code>true</code>.
	 *   
	 * @param substituteRandomIds  if true, a random number will be used 
	 * 								instead of an id from the archive. 
	 */
	public void setSubstituteRandomIds(boolean substituteRandomIds)
	{
		m_substituteRandomIds = substituteRandomIds;
	}

}
