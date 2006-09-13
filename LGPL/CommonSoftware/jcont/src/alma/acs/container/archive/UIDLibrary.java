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
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.acs.container.ContainerException;
import alma.acs.container.ContainerServices;
import alma.archive.range.IdentifierRange;
import alma.entities.commonentity.EntityRefT;
import alma.entities.commonentity.EntityT;
import alma.xmlstore.Identifier;
import alma.xmlstore.IdentifierJ;
import alma.xmlstore.IdentifierOperations;
import alma.xmlstore.IdentifierPackage.NotAvailable;
import alma.xmlstore.IdentifierPackage.NotFound;

/**
 * @author simon, hsommer
 */
public class UIDLibrary
{
	private static UIDLibrary _instance = null;
	private static final AtomicInteger count = new AtomicInteger(0);
	
	private final Logger logger;
	private final ContainerServices cs;
	private final IdentifierJ identifier;	
	private final Range defaultRange;
	
	private final HashMap<URI, Range> idRanges;
	private final HashMap<URI, Range> refRanges;
	
	/**
	 * Return an instance of the UID library
	 * @param cs
	 * @param ident
	 * @return
	 * @throws ContainerException
	 */
	public static synchronized UIDLibrary instance(ContainerServices cs, Identifier ident) 
		throws ContainerException
	{
		if (_instance == null)
		{
			_instance = new UIDLibrary(cs,ident);
		}
		count.incrementAndGet();
		return _instance;
	}
	
	/**
	 * TODO: check if this method is useful, given that its sole purpose is to release the bit of memory 
	 * used for the single instance returned from the {@link #instance(ContainerServices, Identifier)} method.
	 */
	public void close()
	{
		if (count.decrementAndGet() == 0)
		{
			_instance = null; 
		}
	}

	/**
	 * Creates the UIDLibrary, fetches the default range from the Identifier interface.
	 * @param cs
	 * @param ident
	 * @throws ContainerException
	 */
	private UIDLibrary(ContainerServices cs, Identifier ident)
		throws ContainerException
	{
		this.cs = cs;
		this.logger = cs.getLogger(); 
		idRanges = new HashMap<URI, Range>();
		refRanges = new HashMap<URI, Range>();
		
		identifier = (IdentifierJ) cs.getTransparentXmlComponent(
				IdentifierJ.class, ident, IdentifierOperations.class);
		try {
			logger.finest("UIDLibrary: Building default range class");
			IdentifierRange idRange = identifier.getNewRange();
			defaultRange = new Range(idRange);
		}
		catch (NotAvailable e){
			throw new ContainerException(e);
		}		
	}

	
	/**
	 * Assigns a uid from the default range to the <code>EntityT</code> castor class 
	 * of an XML-based entity such as a SchedBlock.
	 * @throws ContainerException
	 * @see {@link ContainerServices#assignUniqueEntityId(EntityT)}
	 */
	public void assignUniqueEntityId(EntityT entity) throws ContainerException {
		logger.finest("Will assign default UID to entity of type " + entity.getEntityTypeName());
		defaultRange.assignUniqueEntityId(entity);
	}

	/**
	 * Fetch a new restricted range, this will return a URI allowing access to
	 * the new Range. The range is automatically stored in the archive. 
	 * @return
	 */
	public URI getNewRestrictedRange(int size, String user)
		throws ContainerException
	{
		Range range = null;
		try
		{
			logger.finest("UIDLibrary: Fetching a restricted range");
			IdentifierRange idRange = identifier.getNewRestrictedRange(size, user);
			range = new Range(idRange);
		}
		catch (NotAvailable e) {
			throw new ContainerException(e);
		}
		
		URI uri = range.rangeId();
		if (idRanges.containsKey(uri)) {
			throw new ContainerException("Cannot store new range. URI occupied. This should never have happened by design!!");
		}
		logger.finest("UIDLibrary: Storing Range under: "+ uri.toASCIIString());
		
		idRanges.put(uri,range);
		
		return uri;
	}
	
	
	/**
	 * Assign a uid to the EntityT from the specified range, not permitted with
	 * deserialised Ranges
	 * @param entity
	 * @param uri
	 * @throws ContainerException
	 */
	public void assignUniqueEntityId(EntityT entity, URI uri) 
		throws ContainerException
	{
		if (idRanges.containsKey(uri)){
			if (logger.isLoggable(Level.FINEST)) {
				logger.finest("UIDLibrary: Assigning ID to entity from range " + uri.toASCIIString());
			}
			Range r = idRanges.get(uri);
			r.assignUniqueEntityId(entity);
		}
		else{
			throw new ContainerException(
				"Cannot find range: " + uri.toASCIIString());
		}
	}
	
	/**
	 * Fetch an existing range from the archive and deserialise, only certain
	 * operations will be permitted.
	 * @param uri
	 * @throws ContainerException
	 */
	public void fetchRange(URI uri, String user) 
		throws ContainerException
	{
		IdentifierRange idRange = null;
		try{
			logger.finest("UIDLibrary: Fetching range: " 
				+ uri.toASCIIString());
			idRange = identifier.getExistingRange(uri.toASCIIString(),user);
		}
		catch (NotFound e){
			throw new ContainerException(e);
		}
		
		Range r = new Range(idRange);
		if (!refRanges.containsKey(uri)){
			refRanges.put(uri,r);
		}
		else{
			throw new ContainerException(
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
		throws ContainerException
	{
		if (refRanges.containsKey(uri)){
			logger.finest("UIDLibrary: Assigning ID Ref to entity from: " 
				+ uri.toASCIIString());
			Range r = refRanges.get(uri);
			r.assignUniqueEntityRef(ref);
		}
		else{
			throw new ContainerException(
				"Cannot find range: " + uri.toASCIIString());
		}
	}
}
