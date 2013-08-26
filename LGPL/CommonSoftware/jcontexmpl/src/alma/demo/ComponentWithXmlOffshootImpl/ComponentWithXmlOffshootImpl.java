/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
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
package alma.demo.ComponentWithXmlOffshootImpl;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentImplBase;
import alma.demo.ComponentWithXmlOffshootJ;
import alma.demo.XmlOffshootJ;
import alma.demo.XmlOffShootImpl.XmlOffShootImpl;

public class ComponentWithXmlOffshootImpl extends ComponentImplBase implements
		ComponentWithXmlOffshootJ {

	XmlOffshootJ m_offshoot;

	@Override
	public XmlOffshootJ getOffshoot() {
		if( m_offshoot == null ) {
			try {
				m_offshoot = new XmlOffShootImpl(m_containerServices);
				m_containerServices.activateOffShoot(m_offshoot, XmlOffshootJ.class);
			} catch (AcsJContainerServicesEx e) {
				e.printStackTrace();
			}
		}
		return m_offshoot;
	}

	@Override
	public void cleanUp() {
		try {
			m_containerServices.deactivateOffShoot(m_offshoot);
			m_offshoot = null;
		} catch (AcsJContainerServicesEx e) {
			e.printStackTrace();
		}
	}

	@Override
	public void deactivateOffshoot() {
		if( m_offshoot != null ) {
			try {
				m_containerServices.deactivateOffShoot(m_offshoot);
				m_offshoot = null;
			} catch (AcsJContainerServicesEx e) {
				e.printStackTrace();
			}
		}
	}

}