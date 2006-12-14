/*
 * 	  Created on 20-Sep-2005
 * 
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

import java.net.URI;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.ArchiveIdentifierError.wrappers.AcsJIdentifierUnavailableEx;
import alma.ArchiveIdentifierError.wrappers.AcsJIdentifierUnexpectedEx;
import alma.ArchiveIdentifierError.wrappers.AcsJRangeExhaustedEx;
import alma.ArchiveIdentifierError.wrappers.AcsJRangeLockedEx;
import alma.ArchiveIdentifierError.wrappers.AcsJRangeUnavailableEx;
import alma.ArchiveIdentifierError.wrappers.AcsJRangeUnlockedEx;
import alma.ArchiveIdentifierError.wrappers.AcsJUidAlreadyExistsEx;
import alma.archive.range.IdentifierRange;
import alma.entities.commonentity.EntityRefT;
import alma.entities.commonentity.EntityT;
import alma.xmlstore.IdentifierJ;
import alma.xmlstore.IdentifierPackage.NotAvailable;
import alma.xmlstore.IdentifierPackage.NotFound;

/**
 * Library-style class that facilitates the use of UID ranges obtained from the identifier archive.
 * <ul>
 * <li>The most commonly used feature should be that of assigning a new UID to an XML entity that does not yet have a UID;
 *     the method {@link #assignUniqueEntityId(EntityT, IdentifierJ)} could be used for this, although it is recommended
 *     to instead use the convenience method {@link ContainerServices#assignUniqueEntityId(EntityT)} from the ContainerServices.
 *     Here the caller is not expected to care about to which range of UIDs the new UID belongs, as long as it is a valid and unique ID.
 * <li>For exotic cases (perhaps ObsPrep) where an existing (e.g. locally created) UID must be replaced by a UID that's valid in the archive,
 *     this class offers the method {@link #replaceUniqueEntityId(EntityT, IdentifierJ)}.   
 * <li>Other more specialized methods allow using specific ranges of UIDs, which is intended for cases where one component allocates
 *     a range and then some other component uses these IDs. As of 2006-09, the only known use case for this is an interaction
 *     between Control and Correlator, which will rely only on the C++ version of this class.
 * </ul>
 * Implementation notes: this class has been pulled up from Archive to ACS in order to share the implementation between the ContainerServices
 * and this library, which otherwise could have remained in the Archive modules. We did not want to split this class, therefore even the methods
 * that ACS does not need are all included here.<br>
 * Simon's original implementation has been thoroughly changed: most notably this class is no longer used as a singleton, which would have 
 * caused severe identity and lifecycle problems with logger and identifier archive objects getting shared among independent components.
 * So far we've kept the original design that hides explicitly requested <code>Range</code> objects by only referring to them through their UIDs
 * in methods {@link #assignUniqueEntityId(EntityT, URI)}, {@link #assignUniqueEntityRef(EntityRefT, URI)} etc., but it is not clear how useful this is.      
 * @author simon, hsommer
 */
public class UIDLibrary
{
	private final Logger logger;
	private static volatile Range defaultRange;
	
	private final HashMap<URI, Range> idRanges;
	private final HashMap<URI, Range> refRanges;
	
	/**
	 * Creates the UIDLibrary without making any calls.
	 * @param logger Logger used by this object.
	 */
	public UIDLibrary(Logger logger)
	{
		this.logger = logger; 
		idRanges = new HashMap<URI, Range>();
		refRanges = new HashMap<URI, Range>();
	}

	
	/**
	 * Assigns a UID from the default range to the <code>EntityT</code> castor class 
	 * of an XML-based entity such as a SchedBlock.
	 * <p>
	 * Implementation note: the default range of UIDs is retrieved from the archive at the first call and is then shared among instances
	 * in order to be frugal on UIDs and to minimize archive access.
	 * This means that often the passed in <code>identifier</code> will not be used at all but still must be provided, 
	 * because the calling component can not know whether another component or the container has  
	 * called this method before.
	 * This method is synchronized to avoid the very unlikely situation that <code>defaultRange.hasNextId</code> succeeds for one thread but then later
	 * assigning the UID still fails because of another thread having stolen the last free UID in the meantime.
	 * 
	 * @param identifier  the identifier archive from which a new <code>Range</code> can be obtained if necessary. Use <br>
	 *        <code>ContainerServices#getTransparentXmlComponent(IdentifierJ.class, identRaw, IdentifierOperations.class);</code> <br>
	 *        to create the required XML binding class aware interface from the plain-Corba <code>Identifier</code> object 
	 *        (<code>identRaw</code>) that is obtained, for example, by <br>
	 *        <code>IdentifierHelper.narrow(getDefaultComponent("IDL:alma/xmlstore/Identifier:1.0"))</code>.
	 * @see ContainerServices#assignUniqueEntityId(EntityT)
	 */
	public synchronized void assignUniqueEntityId(EntityT entity, IdentifierJ identifier) 
	throws AcsJUidAlreadyExistsEx, AcsJIdentifierUnavailableEx, AcsJRangeUnavailableEx, AcsJIdentifierUnexpectedEx  {
		try {
			checkDefaultRange(identifier);
			defaultRange.assignUniqueEntityId(entity);
			
			if (logger.isLoggable(Level.FINEST)) {
				logger.finest("Assigned UID '" + entity.getEntityId() + "' to entity of type " + entity.getEntityTypeName());
			}
		} catch (AcsJUidAlreadyExistsEx e) {
			throw e;
		} catch (AcsJRangeUnavailableEx e) {
			throw e;
		} catch (AcsJIdentifierUnavailableEx e) {
			throw e;
		} catch (Throwable e) {
			// AcsJRangeLockedEx and AcsJRangeExhaustedEx should not occur for default range, thanks to method checkDefaultRange
			throw new AcsJIdentifierUnexpectedEx(e);
		}
	}

	/**
	 * Similar to {@link #assignUniqueEntityId(EntityT, IdentifierJ)}, but allows replacing an existing ID. 
	 * Only in very special cases such as ObsPrep replacing locally-generated IDs with archive-generated UIDs
	 * should this method be used. Replacing UIDs can easily corrupt the archive because existing links would no longer hold!
	 * @param identifier  the identifier archive from which a new <code>Range</code> can be obtained if necessary. Use <br>
	 *        <code>ContainerServices#getTransparentXmlComponent(IdentifierJ.class, identRaw, IdentifierOperations.class);</code> <br>
	 *        to create the required XML binding class aware interface from the plain-Corba <code>Identifier</code> object 
	 *        (<code>identRaw</code>) that is obtained, for example, by <br>
	 *        <code>IdentifierHelper.narrow(getDefaultComponent("IDL:alma/xmlstore/Identifier:1.0"))</code>.
	 * @see #assignUniqueEntityId(EntityT, IdentifierJ)
	 */
	public synchronized void replaceUniqueEntityId(EntityT entity, IdentifierJ identifier) 
	throws AcsJRangeUnavailableEx, AcsJIdentifierUnavailableEx, AcsJIdentifierUnexpectedEx {
		if (entity == null) {
			throw new NullPointerException("argument 'entity' must not be null.");
		}
		try {
			String oldUid = entity.getEntityId();
			checkDefaultRange(identifier);
			defaultRange.replaceUniqueEntityId(entity);
			
			logger.info("Replaced old UID '" + oldUid + "' with new UID '" + entity.getEntityId() + "' on an entity of type " + entity.getEntityTypeName());
		} catch (AcsJRangeUnavailableEx e) {
			throw e;
		} catch (AcsJIdentifierUnavailableEx e) {
			throw e;
		} catch (Throwable e) {
			// AcsJRangeLockedEx and AcsJRangeExhaustedEx should not occur for default range, thanks to method checkDefaultRange
			throw new AcsJIdentifierUnexpectedEx(e);
		}
	}
	
	/**
	 * Creates a default range on demand, or sets a new default range if the old range has no more UID
	 * (which happens after pulling Long.MAX UIDs, or sooner if a limit was set).
	 * @param identifier  the identifier archive from which a new <code>Range</code> can be obtained if necessary. Use <br>
	 *        <code>ContainerServices#getTransparentXmlComponent(IdentifierJ.class, identRaw, IdentifierOperations.class);</code> <br>
	 *        to create the required XML binding class aware interface from the plain-Corba <code>Identifier</code> object 
	 *        (<code>identRaw</code>) that is obtained, for example, by <br>
	 *        <code>IdentifierHelper.narrow(getDefaultComponent("IDL:alma/xmlstore/Identifier:1.0"))</code>.
	 */
	protected synchronized void checkDefaultRange(IdentifierJ identifier) throws AcsJRangeUnavailableEx, AcsJIdentifierUnavailableEx {
		if (identifier == null) {
			AcsJIdentifierUnavailableEx ex =  new AcsJIdentifierUnavailableEx();
			ex.setContextInfo("Provided identifier reference is null.");
			throw ex;
		}
		try {
			if (defaultRange == null || !defaultRange.hasNextID()) {
				defaultRange = new Range(identifier.getNewRange());
			}
		} catch (NotAvailable e) {
			AcsJRangeUnavailableEx ex = new AcsJRangeUnavailableEx();
			ex.setRange("default");
			throw ex;
		}
	}
	
	
	/**
	 * Fetches a new restricted range. 
	 * This will return a URI allowing access to the new Range. 
	 * The range is automatically stored in the archive. 
	 * This method should only be used in very special cases, 
	 * see <a href="http://almasw.hq.eso.org/almasw/bin/view/Archive/UidLibrary">Archive/UidLibrary wiki page</a>!
	 * @param identifier  the identifier archive from which a new <code>Range</code> can be obtained if necessary. Use <br>
	 *        <code>ContainerServices#getTransparentXmlComponent(IdentifierJ.class, identRaw, IdentifierOperations.class);</code> <br>
	 *        to create the required XML binding class aware interface from the plain-Corba <code>Identifier</code> object 
	 *        (<code>identRaw</code>) that is obtained, for example, by <br>
	 *        <code>IdentifierHelper.narrow(getDefaultComponent("IDL:alma/xmlstore/Identifier:1.0"))</code>.
	 * @param printLogs Emit logs, iff set to true.
	 * @return the UID of the range, which can be used for example as an argument in {@link #assignUniqueEntityId(EntityT, URI)}.
	 * @throws UniqueIdException  if the range cannot be obtained from the archive. 
	 */
	public URI getNewRestrictedRange(int size, String user, IdentifierJ identifier, boolean printLogs) throws AcsJRangeUnavailableEx, AcsJIdentifierUnexpectedEx {
		Range range = null;
		try
		{
			if (printLogs) logger.finest("UIDLibrary: Fetching a restricted range");
			IdentifierRange idRange = identifier.getNewRestrictedRange(size, user);
			range = new Range(idRange);
		}
		catch (NotAvailable e) {
			throw new AcsJRangeUnavailableEx(e);
		}
		
		URI uri = range.rangeId();
		if (idRanges.containsKey(uri)) {
			AcsJIdentifierUnexpectedEx ex = new AcsJIdentifierUnexpectedEx();
			ex.setContextInfo("Cannot store new range. URI occupied. This should never have happened by design!!");
			throw ex;
		}
		if (printLogs) logger.finest("UIDLibrary: Storing Range under: "+ uri.toASCIIString());
		
		idRanges.put(uri,range);
		
		return uri;
	}

    /**
     * convenience method for the above, printLogs is set to true
     **/
    public URI getNewRestrictedRange(int size, String user, IdentifierJ identifier) throws AcsJRangeUnavailableEx, AcsJIdentifierUnexpectedEx {
	return getNewRestrictedRange(size, user, identifier, true);
    }

	/**
	 * Assigns a uid to the EntityT from the specified range.
	 * This is not permitted with a locked Range object.
	 * This method should only be used in very special cases, 
	 * see <a href="http://almasw.hq.eso.org/almasw/bin/view/Archive/UidLibrary">Archive/UidLibrary wiki page</a>!
	 * <p>
	 * TODO: figure out if this is meant to work only if the Range referenced by uri has been loaded previously by this instance.
	 * If so, put a comment that fetchRange must be called first. Otherwise the fetch method could be called automatically.
	 * 
	 * @param entity  
	 * @param uri  the UID of the Range object
	 * @param printLogs set to true, iff logs should be printed
	 */
	public void assignUniqueEntityId(EntityT entity, URI uri, boolean printLogs) throws AcsJRangeUnavailableEx, AcsJUidAlreadyExistsEx, AcsJRangeLockedEx, AcsJRangeExhaustedEx
	{
		if (idRanges.containsKey(uri)){
			if (printLogs && logger.isLoggable(Level.FINEST)) {
				logger.finest("UIDLibrary: Assigning ID to entity from range " + uri.toASCIIString());
			}
			Range r = idRanges.get(uri);
			r.assignUniqueEntityId(entity);
		}
		else{
			AcsJRangeUnavailableEx ex = new AcsJRangeUnavailableEx();
			ex.setRange(uri.toASCIIString());
			throw ex;
		}
	}
    /**
     * convenience method for the above, printLogs is set to true
     **/	
    public void assignUniqueEntityId(EntityT entity, URI uri) throws AcsJRangeUnavailableEx, AcsJUidAlreadyExistsEx, AcsJRangeLockedEx, AcsJRangeExhaustedEx {
	assignUniqueEntityId(entity, uri, true);
    }

	/**
	 * Fetch an existing range from the archive and deserialise, only certain
	 * operations will be permitted.
	 * This method should only be used in very special cases, 
	 * see <a href="http://almasw.hq.eso.org/almasw/bin/view/Archive/UidLibrary">Archive/UidLibrary wiki page</a>!
	 * @param uri
	 * @param identifier  the identifier archive from which a new <code>Range</code> can be obtained if necessary. Use <br>
	 *        <code>ContainerServices#getTransparentXmlComponent(IdentifierJ.class, identRaw, IdentifierOperations.class);</code> <br>
	 *        to create the required XML binding class aware interface from the plain-Corba <code>Identifier</code> object 
	 *        (<code>identRaw</code>) that is obtained, for example, by <br>
	 *        <code>IdentifierHelper.narrow(getDefaultComponent("IDL:alma/xmlstore/Identifier:1.0"))</code>.
	 * 
	 * @throws AcsJRangeUnavailableEx
	 */
	public void fetchRange(URI uri, String user, IdentifierJ identifier) 
		throws AcsJRangeUnavailableEx
	{
		IdentifierRange idRange = null;
		try{
			logger.finest("UIDLibrary: Fetching range: " 
				+ uri.toASCIIString());
			idRange = identifier.getExistingRange(uri.toASCIIString(),user);
		}
		catch (NotFound e) {
			throw new AcsJRangeUnavailableEx(e);
		}
		
		Range r = new Range(idRange);
		if (!refRanges.containsKey(uri)){
			refRanges.put(uri,r);
		}
		else {
			AcsJRangeUnavailableEx ex = new AcsJRangeUnavailableEx();
			ex.setRange(uri.toASCIIString());
			throw ex;
		}
	}
	
	/**
	 * Assigns a UID to an entity reference. 
	 * This method should only be used in very special cases, 
	 * see <a href="http://almasw.hq.eso.org/almasw/bin/view/Archive/UidLibrary">Archive/UidLibrary wiki page</a>!
	 * Note that this operation is only permitted with a locked range.
	 * @param ref  the schema-generated entity reference
	 * @param uri
	 * @throws UniqueIdException
	 */
	public void assignUniqueEntityRef(EntityRefT ref, URI uri) 
		throws AcsJRangeUnavailableEx, AcsJRangeExhaustedEx, AcsJRangeUnlockedEx
	{
		if (refRanges.containsKey(uri)){
			logger.finest("UIDLibrary: Assigning ID Ref to entity from: " + uri.toASCIIString());
			Range r = refRanges.get(uri);
			r.assignUniqueEntityRef(ref);
		}
		else{
			AcsJRangeUnavailableEx ex = new AcsJRangeUnavailableEx();
			ex.setRange(uri.toASCIIString());
			throw ex;
		}
	}
	
	
}
