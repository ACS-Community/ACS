package alma.demo.ComponentGetterImpl;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentImplBase;
import alma.demo.ComponentGetterOperations;

public class ComponentGetterImpl extends ComponentImplBase implements ComponentGetterOperations {

	private static String COMPONENT_NAME = "COMP_TO_GET";

	@Override
	public void getOtherComponent() {
		try {
			m_containerServices.getComponent(COMPONENT_NAME);
		} catch (AcsJContainerServicesEx e) {
			e.printStackTrace();
		}
		m_logger.info("Retrieved " + COMPONENT_NAME + "' successfully");
	}

}
