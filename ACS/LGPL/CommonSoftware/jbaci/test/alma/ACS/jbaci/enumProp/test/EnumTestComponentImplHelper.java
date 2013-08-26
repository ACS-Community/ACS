/**
 * 
 */
package alma.ACS.jbaci.enumProp.test;

import java.util.logging.Logger;

import jbaciEnumPropTest.jbaciEnumTestComponentOperations;
import jbaciEnumPropTest.jbaciEnumTestComponentPOATie;

import org.omg.PortableServer.Servant;

import alma.ACS.ACSComponentOperations;
import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ComponentHelper;
import alma.maciErrType.wrappers.AcsJComponentCreationEx;

/**
 * @author msekoranja
 *
 */
public class EnumTestComponentImplHelper extends ComponentHelper {

	/**
	 * Passes a logger to the callback object.
	 * @param containerLogger
	 */
	public EnumTestComponentImplHelper(Logger containerLogger)
	{
		super(containerLogger);
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ComponentHelper#_createComponentImpl()
	 */
	@Override
	protected ComponentLifecycle _createComponentImpl()
			throws AcsJComponentCreationEx {
		return new EnumTestComponentImpl();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ComponentHelper#_getOperationsInterface()
	 */
	@Override
	protected Class<? extends ACSComponentOperations> _getOperationsInterface() {
		return jbaciEnumTestComponentOperations.class;
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ComponentHelper#_getPOATieClass()
	 */
	@Override
	protected Class<? extends Servant> _getPOATieClass() {
		return jbaciEnumTestComponentPOATie.class;
	}

}
