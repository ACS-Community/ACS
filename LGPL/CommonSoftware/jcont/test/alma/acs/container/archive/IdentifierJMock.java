/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
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

import java.util.logging.Logger;

import org.apache.commons.lang.StringUtils;

import alma.ACS.ComponentStates;
import alma.acs.util.IsoDateFormat;
import alma.archive.range.IdentifierRange;
import alma.archive.range.IdentifierRangeEntityT;
import alma.archive.range.RangeT;
import alma.xmlstore.IdentifierJ;
import alma.xmlstore.IdentifierPackage.NotAvailable;
import alma.xmlstore.IdentifierPackage.NotFound;

/**
 * @author hsommer
 *
 */
public class IdentifierJMock implements IdentifierJ {

	private final Logger logger;
	private final long archiveid;
	private long rangeid;
	private final int archiveIdLength = 2;
	
	public IdentifierJMock(Logger logger, long archiveid, long rangeid) {
		this.logger = logger;
		this.archiveid = archiveid;
		this.rangeid = rangeid;
	}
	
	/* (non-Javadoc)
	 * @see alma.xmlstore.IdentifierJ#getExistingRange(java.lang.String, java.lang.String)
	 */
	public IdentifierRange getExistingRange(String identifier, String user) throws NotFound {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see alma.xmlstore.IdentifierJ#getIdNamespace()
	 */
	public String getIdNamespace() throws NotAvailable {
		// TODO Auto-generated method stub
		return null;
	}

	/** 
	 * This is the only method that's currently implemented
	 * @see alma.xmlstore.IdentifierJ#getNewRange()
	 */
	public IdentifierRange getNewRange() throws NotAvailable {		
		//Create the entity information
		IdentifierRangeEntityT entityt = new IdentifierRangeEntityT();
		//The id of the range is the 0 document id in that range.
		entityt.setEntityId(createUid());

		IdentifierRange range = new IdentifierRange();
		range.setIdentifierRangeEntity(entityt);
		
		//set the time stamp
		String ts = IsoDateFormat.formatCurrentDate();  // todo: pass in time externally?
		range.setCreatedTimeStamp(ts);
		
		range.setIsLocked(false);
		
		String archiveIdString = Long.toHexString(archiveid);
		archiveIdString = "X"+StringUtils.leftPad(archiveIdString,archiveIdLength, '0'); 
		range.setArchiveID(archiveIdString);
		
		RangeT ranget = new RangeT();
		ranget.setRangeID(Long.toHexString(rangeid));
		ranget.setBaseDocumentID("1");
		range.setRange(ranget);
		
		rangeid++;
//		this.setRangeId(rangeid);
		
		return range;
	}

	/* (non-Javadoc)
	 * @see alma.xmlstore.IdentifierJ#getNewRestrictedRange(int, java.lang.String)
	 */
	public IdentifierRange getNewRestrictedRange(int number, String user) throws NotAvailable {
		IdentifierRange range = this.getNewRange();
		RangeT ranget = range.getRange();
		ranget.setMaxDocumentID(Integer.toHexString(number));
		return range;
	}

	/* (non-Javadoc)
	 * @see alma.xmlstore.IdentifierJ#getUIDs(short)
	 */
	public String[] getUIDs(short number) throws NotAvailable {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see alma.ACS.ACSComponentOperations#componentState()
	 */
	public ComponentStates componentState() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see alma.ACS.ACSComponentOperations#name()
	 */
	public String name() {
		// TODO Auto-generated method stub
		return null;
	}

	
	private String createUid()
	{
		String uid = "uid://X" + 
			StringUtils.leftPad(Long.toHexString(archiveid),archiveIdLength, '0') +
			"/X" + Long.toHexString(rangeid) +"/X0";
		return uid;
	}

	public boolean checkUIDsyntax(String identifier) {
		// TODO Auto-generated method stub
		return false;
	}

}
