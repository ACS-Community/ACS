package si.ijs.acs.objectexplorer.engine;

/**
 * Attribute is a class that represents an attribute
 * (or Java property) design pattern on remote objects.
 * Attributes are characterized by a pair of functions, called
 * accessor and mutator functions. An attribute has a type.
 * An accessor is used to retrieve the value of a given type,
 * and the mutator is used to change the value. A read only
 * attribute is an attribute that only declares an accessor method.
 * Attributes are treated separately from normal operations because
 * their explicit declaration is often chosen for special design
 * reasons and thus they have a special status for the final user.
 */
public abstract class Attribute {
	private DataType type = null;
	private String name = null;
	private boolean readOnly = false;
	private SimpleIntrospectable introspectable = null;
/**
 * Constructs a new instance of this class. All parameters
 * must be non-<code>null</code>.
 *
 * @param name the name of the attribute
 * @param introspectable the remote instance that contains this
 		  attribute
 * @param type a <code>DataType</code> instance that represents
 * 		  the type of this attribute. Note that the class should
 *	   	  declare public constructors with all parameters neccessary
 *		  for the construction of the value, if applicable. GUI
 *	   	  can then use introspection to create new instances and pass
 *		  them to mutator methods.
 * @param readOnly <code>true</code> if this attribute only supports
 *		  accessor method.
 */
public Attribute(String name, SimpleIntrospectable introspectable, DataType type, boolean readOnly) {
	super();
	if (name == null) throw new NullPointerException("name");
	if (introspectable == null) throw new NullPointerException("introspectable");
	if (type == null) throw new NullPointerException("type");
	this.name = name;
	this.introspectable = introspectable;
	this.type = type;
	this.readOnly = readOnly;
}
/**
 * Returns the <code>DataType</code> object that represents
 * the type of this attribute. The value is the same
 * as that passed into the constructor.
 *
 * @return type of <code>this</code>
 */
public DataType getAttributeType() {
	return type;
}
/**
 * Returns the container for this attribute. The function
 * returns the parameter passed into the constructor.
 *
 * @return remote object declaring the attribute
 */
public SimpleIntrospectable getIntrospectable() {
	return introspectable;
}
/**
 * The method invokes remote accessor method on the
 * introspectable instance. The return value is
 * packed into the standard <code>RemoteCall</code>
 * structure.
 * 
 * @return remote call data structure that will return
 *	  	   <code>true</code> when <code>isAttributeAccess()</code>
 *  	   is invoked.
 */
public abstract RemoteCall invokeAccessor();
/**
 * NOT SUPPORTED yet. Will be used for changing the value of 
 * a writable attribute.
 * @param value java.lang.Object the new value of the attribute
 */
public abstract RemoteCall invokeMutator(Object value);
/**
 * Returns <code>true</code> if the attribute is read
 * only. Returns the same value as that passed into the
 * constructor.
 * 
 * @return boolean readOnly flag.
 */
public boolean isReadOnly() {
	return readOnly;
}
/**
 * Overloaded to return the name of the attribute as
 * passed into the constructor. This should be the
 * display name in the GUI.
 * 
 * @return name of the attribute
 */
public String toString() {
	return name;
}
}
