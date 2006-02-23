package si.ijs.acs.objectexplorer;

import si.ijs.acs.objectexplorer.engine.*;
/**
 * Listens to the events of an OETree
 *
 *
 * @author Miha Kadunc
 */
public interface OperationInvocator {
	public void invokeOperation(Operation op, Object[] params);
}
