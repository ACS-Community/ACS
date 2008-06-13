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

import alma.acs.component.ComponentImplBase;
import alma.jconttest.DummyComponentWrapperOperations;
import alma.jconttest.DummyComponent;
import alma.jconttest.DummyComponentHelper;

/**
 * @author rtobar
 * created Jan 11, 2008 5:10:00 PM
 */
public class DummyComponentWrapperImpl extends ComponentImplBase implements DummyComponentWrapperOperations {

	private static final String DUMMYCOMP_TYPENAME = "IDL:alma/jconttest/DummyComponent:1.0";
	private static final String PROPERTY_NAME = "jacorb.connection.client.pending_reply_timeout";
	
	public boolean callDummyCompWithTimeAndName(int timeInMillisec, String name) {
		
		DummyComponent dummyComponent = null;
		try {
			org.omg.CORBA.Object compObj = m_containerServices.getComponent(name);
			dummyComponent = DummyComponentHelper.narrow(compObj);
		} catch (Exception e) {
			e.printStackTrace();
		}

		// We get the property value from the ORB configuration for the component
		// Need to cast the ORB object since it declared as org.omg.orb.ORB, and it
		// doesn't have the getConfiguration() method
		int timeout = 0;
		if( m_containerServices.getAdvancedContainerServices().getORB() instanceof org.jacorb.orb.ORB ) {
			try {
				timeout = ((org.jacorb.orb.ORB)m_containerServices.getAdvancedContainerServices().getORB()).getConfiguration().getAttributeAsInteger(PROPERTY_NAME);
			} catch (org.apache.avalon.framework.configuration.ConfigurationException e){
				e.printStackTrace();
			}
		}
		
		// If the given time is less than 0, then let's call the method with the timeout that is
		// configured in the ORB. Otherwise, use the time given as parameter.
		if( timeInMillisec < 0 )
			timeInMillisec = timeout + 1000;

        try {
            dummyComponent.callThatTakesSomeTime((int)(timeInMillisec));
        } catch (org.omg.CORBA.TIMEOUT e) {
            return true;
        }
        return false;
	}
	/**
	 * This method is a wrapper for the {@link alma.jconttest.DummyComponent#callThatTakesSomeTime} method.
	 * It returns if a <code>org.omg.CORBA.TIMEOUT</code> exception is thrown as result of this call. The timeout
	 * should be provoked by the <code>jacorb.connection.client.pending_reply_timeout</code> property value.
	 * 
	 * @param timeInMillisec The time which is used in the <code>DummyComponent</code> method call.
	 * @return If a <code>org.omg.CORBA.TIMEOUT</code> exception is thrown as result of this call. If there is no value for the <code>jacorb.connection.client.pending_reply_timeout</code> property, then true is returned.
	 */
	public boolean callDummyComponentWithTime(int timeInMillisec) {
		
		DummyComponent dummyComponent = null;
		try {
			org.omg.CORBA.Object compObj = m_containerServices.getDefaultComponent(DUMMYCOMP_TYPENAME);
			dummyComponent = DummyComponentHelper.narrow(compObj);
		} catch (Exception e) {
			e.printStackTrace();
		}

		// We get the property value from the ORB configuration for the component
		// Need to cast the ORB object since it declared as org.omg.orb.ORB, and it
		// doesn't have the getConfiguration() method
		int timeout = -1;
		if( m_containerServices.getAdvancedContainerServices().getORB() instanceof org.jacorb.orb.ORB ) {
			try {
				timeout = ((org.jacorb.orb.ORB)m_containerServices.getAdvancedContainerServices().getORB()).getConfiguration().getAttributeAsInteger(PROPERTY_NAME);
			} catch (org.apache.avalon.framework.configuration.ConfigurationException e){
				e.printStackTrace();
			}
		}
		
		// If the given time is less than 0, then let's call the method with the timeout that is
		// configured in the ORB. Otherwise, use the time given as parameter.
		if( timeInMillisec < 0 )
			timeInMillisec = timeout;

		if( timeout > 0 ) {
			try {
				dummyComponent.callThatTakesSomeTime((int)(timeInMillisec));
			} catch (org.omg.CORBA.TIMEOUT e) {
				return true;
			}
			return false;
		}

		return true;
	}

}
