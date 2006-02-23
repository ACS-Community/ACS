package alma.demo.LampCallbackImpl;

import java.util.logging.Logger;

import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ComponentHelper;
import alma.demo.LampCallbackOperations;
import alma.demo.LampCallbackPOATie;

/**
 * @author alma-component-helper-generator-tool
 */
public class LampCallbackHelper extends ComponentHelper
{
	/**
	 * Passes a logger to the callback object.
	 * @param containerLogger
	 */
	public LampCallbackHelper(Logger containerLogger)
	{
		super(containerLogger);
	}

	/**
	 * Gets an instance of the implementation class of the LampCallback component.
	 * @return ComponentLifecycle
	 * @see alma.acs.container.ComponentHelper#_createComponentImpl()
	 */
	protected ComponentLifecycle _createComponentImpl()
	{
		return new LampCallbackImpl();
	}

	/**
	 * Gets an instance of the POATie class of the LampCallback component.
	 * @return Class
	 * @see alma.acs.container.ComponentHelper#_getPOATieClass()
	 */
	protected Class _getPOATieClass()
	{
		return LampCallbackPOATie.class;
	}

	/**
	 * Gets an instance of the operations of the LampAccess component.
	 * @return Class
	 * @see alma.acs.container.ComponentHelper#getOperationsInterface()
	 */
	protected Class _getOperationsInterface()
	{
		return LampCallbackOperations.class;
	}
}
