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
 
package alma.acs.logging;

/**
 * Contains constants for ACS Core Levels.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface ACSCoreLevel
{
    /**
     * Unknown (ACS level).
     */
	public final int ACS_LEVEL_UNKNOWN = 0;
	
	/**
	 * Messages indicating function-calling sequence (ACS level).
	 */
	public final int ACS_LEVEL_TRACE = 2;
	
	/**
	 * Messages that contain information normally of use only when
	 * debugging a program (ACS level).
	 */
	public final int ACS_LEVEL_DEBUG = 3;
	
	/**
	 * Information messages (ACS level).
	 */
	public final int ACS_LEVEL_INFO = 4;
	
	/**
	 * Conditions that are not error conditions, but that may require
	 * special handling (ACS level).
	 */
	public final int ACS_LEVEL_NOTICE = 5;
	
	/**
	 * Warning messages (ACS level).
	 */
	public final int ACS_LEVEL_WARNING = 6;
	
	/**
	 * Error messages (ACS level).
	 */
	public final int ACS_LEVEL_ERROR = 8;
	
	/**
	 * Critical conditions, such as hard device errors (ACS level).
	 */
	public final int ACS_LEVEL_CRITICAL = 9;
	
	/**
	 * A condition that should be corrected immediately, such as a
	 * corrupted system database (ACS level).
	 */
	public final int ACS_LEVEL_ALERT = 10;
	
	/**
	 * A panic condition. This is normally broadcast to all users (ACS level).
	 */
	public final int ACS_LEVEL_EMERGENCY = 11;
}
