/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2008
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
package alma.jconttest.DummyComponentWrapperImpl;

import java.util.logging.Level;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.ComponentImplBase;
import alma.jconttest.DummyComponent;
import alma.jconttest.DummyComponentHelper;
import alma.jconttest.DummyComponentWrapperOperations;
import alma.jconttest.util.JconttestUtil;

/**
 * @author rtobar
 * created Jan 11, 2008 5:10:00 PM
 */
public class DummyComponentWrapperImpl extends ComponentImplBase implements DummyComponentWrapperOperations {

	private static final String DUMMYCOMP_TYPENAME = "IDL:alma/jconttest/DummyComponent:1.0";
	
	
	public boolean callDummyCompWithTimeAndName(int timeInMillisec, String targetComponentName) throws CouldntPerformActionEx {
		try {
			return _callDummyComponentWithTime(timeInMillisec, targetComponentName);
		} catch (AcsJCouldntPerformActionEx ex) {
			throw ex.toCouldntPerformActionEx();
		}
	}

	/**
	 * This method is a wrapper for the {@link alma.jconttest.DummyComponent#callThatTakesSomeTime} method.
	 * It returns true if a <code>org.omg.CORBA.TIMEOUT</code> exception is thrown as result of this call. The timeout
	 * should be provoked by the <code>jacorb.connection.client.pending_reply_timeout</code> property value.
	 * 
	 * @param timeInMillisec The time which is used in the <code>DummyComponent#callThatTakesSomeTime</code> method call.
	 * @return If a <code>org.omg.CORBA.TIMEOUT</code> exception is thrown as result of this call. If there is no value for the <code>jacorb.connection.client.pending_reply_timeout</code> property, then true is returned.
	 */
	public boolean callDummyComponentWithTime(int timeInMillisec) throws CouldntPerformActionEx {
		try {
			return _callDummyComponentWithTime(timeInMillisec, null);
		} catch (AcsJCouldntPerformActionEx ex) {
			throw ex.toCouldntPerformActionEx();
		}
	}

	
	public boolean _callDummyComponentWithTime(int timeInMillisec, String targetComponentName) 
		throws AcsJCouldntPerformActionEx {
		
		DummyComponent dummyComponent = null;
		try {
			org.omg.CORBA.Object compObj = null;
			if (targetComponentName != null) {
				compObj = m_containerServices.getComponent(targetComponentName);
			}
			else {
				compObj = m_containerServices.getDefaultComponent(DUMMYCOMP_TYPENAME);
			}
			dummyComponent = DummyComponentHelper.narrow(compObj);
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Failed to access target component.", e);
			throw new AcsJCouldntPerformActionEx();
		}
		targetComponentName = dummyComponent.name();
		
		// If the given time is less than 0, then we call "callThatTakesSomeTime" with a time larger than the ORB-level timeout.
		if( timeInMillisec < 0 ) {
			JconttestUtil util = new JconttestUtil(m_containerServices);
			int orbLevelTimeoutMillis = util.getSystemLevelOrbTimeoutMillis();
			timeInMillisec = orbLevelTimeoutMillis + 1000;
		}

		try {
			dummyComponent.callThatTakesSomeTime(timeInMillisec);
			m_logger.info("Did NOT get org.omg.CORBA.TIMEOUT in call " + 
					targetComponentName + "#callThatTakesSomeTime(" + timeInMillisec + ")");
			return false;
		} catch (org.omg.CORBA.TIMEOUT e) {
			m_logger.info("Got org.omg.CORBA.TIMEOUT in call " + 
					targetComponentName + "#callThatTakesSomeTime(" + timeInMillisec + ")");
			return true;
		}
	}
	
}
