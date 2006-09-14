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

import alma.archive.range.IdentifierRange;
import alma.entities.commonentity.EntityRefT;
import alma.entities.commonentity.EntityT;
import alma.xmlstore.IdentifierJ;
import alma.xmlstore.IdentifierPackage.NotAvailable;
import alma.xmlstore.IdentifierPackage.NotFound;

/**
 * @author simon, hsommer
 */
public class UIDLibrary
{
//	private static UIDLibrary _instance = null;
//	private static final AtomicInteger count = new AtomicInteger(0);
	
	
	private final Logger logger;
//	private final ContainerServices cs;
//	private final IdentifierJ identifier;	
	private static volatile Range defaultRange;
	
	private final HashMap<URI, Range> idRanges;
	private final HashMap<URI, Range> refRanges;
	
//	/**
//	 * Return an instance of the UID library
//	 * @param cs
//	 * @param ident
//	 * @return
//	 * @throws ContainerException
//	 */
//	public static synchronized UIDLibrary instance(ContainerServices cs, Identifier ident) 
//		throws UniqueIdException
//	{
//		if (_instance == null)
//		{
//			_instance = new UIDLibrary(cs, ident);
//		}
//		count.incrementAndGet();
//		return _instance;
//	}
//	
//	/**
//	 * TODO: check if this method is useful, given that its sole purpose is to release the bit of memory 
//	 * used for the single instance returned from the {@link #instance(ContainerServices, Identifier)} method.
//	 * If we keep it, then after this call it should be impossible to work with an instance, thus the close-status should be checked.
//	 */
//	public void close()
//	{
//		if (count.decrementAndGet() == 0)
//		{
//			_instance = null; 
//		}
//	}

	/**
	 * Creates the UIDLibrary, fetches the default range from the Identifier interface.
	 * @param logger Logger used by this instance.
	 * @throws UniqueIdException if no UID range could be retrieved, or if <code>ident</code> cannot be wrapped with the XML binding layer
	 */
	public UIDLibrary(Logger logger) //throws UniqueIdException
	{
		this.logger = logger; 
		idRanges = new HashMap<URI, Range>();
		refRanges = new HashMap<URI, Range>();
		
//		try {
//			identifier = (IdentifierJ) cs.getTransparentXmlComponent(
//					IdentifierJ.class, ident, IdentifierOperations.class);
//		} catch (ContainerException e1) {
//			throw new UniqueIdException(e1);
//		}
//		try {
//			logger.finest("UIDLibrary: will retrieve the UID default range.");
//			IdentifierRange idRange = identifier.getNewRange();
//			defaultRange = new Range(idRange);
//		}
//		catch (NotAvailable e) {
//			throw new UniqueIdException(e);
//		}		
	}

	
	/**
	 * Assigns a uid from the default range to the <code>EntityT</code> castor class 
	 * of an XML-based entity such as a SchedBlock.
	 * @throws UniqueIdException
	 * @see {@link ContainerServices#assignUniqueEntityId(EntityT)}
	 */
	public void assignUniqueEntityId(EntityT entity, IdentifierJ indentifier) throws UniqueIdException {
		
		checkDefaultRange(indentifier);
		defaultRange.assignUniqueEntityId(entity);
		
		if (logger.isLoggable(Level.FINEST)) {
			logger.finest("Assigned UID '" + entity.getEntityId() + "' to entity of type " + entity.getEntityTypeName());
		}
	}

	
	/**
	 * Fetches a new restricted range. 
	 * This will return a URI allowing access to the new Range. 
	 * The range is automatically stored in the archive. 
	 * @return the UID of the range, which can be used for example as an argument in {@link #assignUniqueEntityId(EntityT, URI)}.
	 * @throws UniqueIdException  if the range cannot be obtained from the archive. 
	 */
	public URI getNewRestrictedRange(int size, String user, IdentifierJ identifier) throws UniqueIdException {
		Range range = null;
		try
		{
			logger.finest("UIDLibrary: Fetching a restricted range");
			IdentifierRange idRange = identifier.getNewRestrictedRange(size, user);
			range = new Range(idRange);
		}
		catch (NotAvailable e) {
			throw new UniqueIdException(e);
		}
		
		URI uri = range.rangeId();
		if (idRanges.containsKey(uri)) {
			throw new UniqueIdException("Cannot store new range. URI occupied. This should never have happened by design!!");
		}
		logger.finest("UIDLibrary: Storing Range under: "+ uri.toASCIIString());
		
		idRanges.put(uri,range);
		
		return uri;
	}
	
	
	/**
	 * Assigns a uid to the EntityT from the specified range.
	 * This is not permitted with a locked Range object.
	 * <p>
	 * TODO: figure out if this is meant to work only if the Range referenced by uri has been loaded previously by this instance.
	 * If so, put a comment that fetchRange must be called first.
	 *  
	 * @param entity  
	 * @param uri  the UID of the Range object
	 * @throws UniqueIdException
	 */
	public void assignUniqueEntityId(EntityT entity, URI uri) throws UniqueIdException
	{
		if (idRanges.containsKey(uri)){
			if (logger.isLoggable(Level.FINEST)) {
				logger.finest("UIDLibrary: Assigning ID to entity from range " + uri.toASCIIString());
			}
			Range r = idRanges.get(uri);
			r.assignUniqueEntityId(entity);
		}
		else{
			throw new UniqueIdException("Cannot find range: " + uri.toASCIIString());
		}
	}
	
	
	/**
	 * Fetch an existing range from the archive and deserialise, only certain
	 * operations will be permitted.
	 * @param uri
	 * @throws ContainerException
	 */
	public void fetchRange(URI uri, String user, IdentifierJ identifier) 
		throws UniqueIdException
	{
		IdentifierRange idRange = null;
		try{
			logger.finest("UIDLibrary: Fetching range: " 
				+ uri.toASCIIString());
			idRange = identifier.getExistingRange(uri.toASCIIString(),user);
		}
		catch (NotFound e){
			throw new UniqueIdException(e);
		}
		
		Range r = new Range(idRange);
		if (!refRanges.containsKey(uri)){
			refRanges.put(uri,r);
		}
		else{
			throw new UniqueIdException(
				"Range: " + uri.toASCIIString() + " is already in use");
		}
	}
	
	/**
	 * Assign a reference id, this operation is only permitted with deserialised
	 * ranges.
	 * @param ref
	 * @param uri
	 * @throws ContainerException
	 */
	public void assignUniqueEntityRef(EntityRefT ref, URI uri) 
		throws UniqueIdException
	{
		if (refRanges.containsKey(uri)){
			logger.finest("UIDLibrary: Assigning ID Ref to entity from: " 
				+ uri.toASCIIString());
			Range r = refRanges.get(uri);
			r.assignUniqueEntityRef(ref);
		}
		else{
			throw new UniqueIdException(
				"Cannot find range: " + uri.toASCIIString());
		}
	}
	
	/**
	 * Creates a new Range for {@link #defaultRange} as part of lazy instantiation, or if the old Range has no more free UIDs.
	 * Note that the default range object is shared among instances to be frugal on UIDs and to minimize archive access.
	 * <p>
	 * Note on thread safety: a subsequent call that needs to allocate a fresh UID may still fail.
	 * TODO: either put the free-UID-check and the assignments under one thread monitor (e.g. by making all methods synchronized),
	 * or keep calling this method in case of assignment exceptions.
	 * @param indentifier the identifier archive from which a new Range can be obtained.
	 * @throws UniqueIdException
	 */
	protected synchronized void checkDefaultRange(IdentifierJ indentifier) throws UniqueIdException {
		try {
			if (defaultRange == null) {
				defaultRange = new Range(indentifier.getNewRange());
			}
			else if (!defaultRange.hasNextID()) {
				defaultRange = new Range(indentifier.getNewRange());
			}
		} catch (NotAvailable e) {
			throw new UniqueIdException(e);
		}
	}	
	
}
