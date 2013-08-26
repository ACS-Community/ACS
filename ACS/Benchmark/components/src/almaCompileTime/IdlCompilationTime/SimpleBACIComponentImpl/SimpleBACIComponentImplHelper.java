package almaCompileTime.IdlCompilationTime.SimpleBACIComponentImpl;

import java.util.logging.Logger;

import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ComponentHelper;

/**
 * @author alma-component-helper-generator-tool
 */
public class SimpleBACIComponentImplHelper extends ComponentHelper
{
	/**
	 * Passes a logger to the callback object.
	 * @param containerLogger
	 */
	public SimpleBACIComponentImplHelper(Logger containerLogger)
	{
		super(containerLogger);
	}

    /**
     * Gets an instance of the implementation class of the LampAccess component.
     * @return ComponentLifecycle
	 * @see alma.acs.container.ComponentHelper#_createComponentImpl()
	 */
	protected ComponentLifecycle _createComponentImpl()
	{
		return new SimpleBACIComponentImpl();
	}

	/**
	 * Gets an instance of the POATie class of the LampAccess component.
	 * @return Class
	 * @see alma.acs.container.ComponentHelper#_getPOATieClass()
	 */
	protected Class _getPOATieClass()
	{
		return alma.IdlCompilationTime.SimpleBACIComponentPOATie.class;
	}

	/**
	 * Gets an instance of the operations of the LampAccess component.
	 * @return Class
	 * @see alma.acs.container.ComponentHelper#getOperationsInterface()
	 */
	protected Class _getOperationsInterface()
	{
		return alma.IdlCompilationTime.SimpleBACIComponentOperations.class;
	}

}
