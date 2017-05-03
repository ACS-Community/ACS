package jbaci.test;

import java.util.logging.Logger;

import org.omg.PortableServer.Servant;

import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ComponentHelper;
import alma.jbaci.AlarmTestServerOperations;
import alma.jbaci.AlarmTestServerPOATie;

public class AlarmTestServerImplHelper extends ComponentHelper {
	/**
	 * Passes a logger to the callback object.
	 * @param containerLogger
	 */
	public AlarmTestServerImplHelper(Logger containerLogger)
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
		return new AlarmTestServerImpl();
	}

	/**
	 * Gets an instance of the POATie class of the LampAccess component.
	 * @return Class
	 * @see alma.acs.container.ComponentHelper#_getPOATieClass()
	 */
	protected Class _getPOATieClass()
	{
		return AlarmTestServerPOATie.class;
	}

	/**
	 * Gets an instance of the operations of the LampAccess component.
	 * @return Class
	 * @see alma.acs.container.ComponentHelper#getOperationsInterface()
	 */
	protected Class _getOperationsInterface()
	{
		return AlarmTestServerOperations.class;
	}
}
