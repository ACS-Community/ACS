package si.ijs.acs.objectexplorer.engine;

/**
 * This interface represents the remote object.
 * Name of the interface signifies that the
 * instances are able to return metadata about themselves,
 * such as listings of all remote methods and attributes
 * that they declare. All introspectable instances have
 * a name. How this name is mapped to the remote reference
 * depends on the engine implementation.
 */
public interface SimpleIntrospectable {
/**
 * This method returns all attributes declared
 * by this introspectable instance. Before this
 * method is called, the introspectable must have
 * been connected.
 *
 * @return an array of declared attribute objects
 */
Attribute[] getAttributes();
/**
 * Returns the name of this introspectable. Note that
 * the implementing object should overload the
 * <code>toString()</code> method to return the same
 * value to provide GUI with a standardized way to
 * display the introspectable in components such as
 * lists or trees.
 * 
 * @return name of <code>this</code>
 */
String getName();
/**
 * Returns all operations declared by this introspectable.
 * Before this method is called the introspectable instance
 * must have been connected. The operations exclude
 * attribute mutator and accessor methods.
 *
 * @return operations declared by the introspectable.
 */
Operation[] getOperations();
}
