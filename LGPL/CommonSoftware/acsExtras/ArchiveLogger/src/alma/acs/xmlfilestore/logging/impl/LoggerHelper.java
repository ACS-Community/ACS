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

package alma.acs.xmlfilestore.logging.impl;

import java.util.logging.Logger;

import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ComponentHelper;
import alma.acs.xmlfilestore.logging.ArchiveLoggerImpl;
import alma.xmlFileStore.LoggerOperations;
import alma.xmlFileStore.LoggerPOATie;

/**
 * Component helper class. Generated for convenience, but can be modified by the
 * component developer. Must therefore be treated like any other Java class
 * (CVS, ...).
 * <p>
 * To create an entry for your component in the Configuration Database, copy the
 * line below into a new entry in the file
 * $ACS_CDB/MACI/Components/Components.xml and modify the instance name of the
 * component and the container:
 * <p>
 * Name="LOGGER_1" Code="alma.xmlstore.LoggerImpl.LoggerHelper"
 * Type="IDL:alma/xmlstore/Logger:1.0" Container="frodoContainer"
 * <p>
 * 
 * @author alma-component-helper-generator-tool
 */
public class LoggerHelper extends ComponentHelper {
	
	/** 
	 * Avoid that xmlEntryReceived gets logged by container when entering and leaving
	 * 
	 */
	private String[] methodsNotLogged = { "xmlEntryReceived" };
	protected String[] _getComponentMethodsExcludedFromInvocationLogging() {
		return methodsNotLogged;
	}
	/*
	 * No method of this component should be logged by ORB, see: alma.acs.container.ComponentHelper
	 */
	protected boolean requiresOrbCentralLogSuppression() {
    	return true;
    }
	/**
	 * Constructor
	 * 
	 * @param containerLogger
	 *            logger used only by the parent class.
	 */
	public LoggerHelper(Logger containerLogger) {
		super(containerLogger);
	}

	/**
	 * @see alma.acs.container.ComponentHelper#_createComponentImpl()
	 */
	protected ComponentLifecycle _createComponentImpl() {
		return new ArchiveLoggerImpl();
	}

	/**
	 * @see alma.acs.container.ComponentHelper#_getPOATieClass()
	 */
	protected Class _getPOATieClass() {
		return LoggerPOATie.class;
	}

	/**
	 * @see alma.acs.container.ComponentHelper#getOperationsInterface()
	 */
	protected Class _getOperationsInterface() {
		return LoggerOperations.class;
	}

}