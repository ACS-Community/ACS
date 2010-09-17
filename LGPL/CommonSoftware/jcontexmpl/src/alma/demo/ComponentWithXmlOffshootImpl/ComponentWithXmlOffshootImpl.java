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