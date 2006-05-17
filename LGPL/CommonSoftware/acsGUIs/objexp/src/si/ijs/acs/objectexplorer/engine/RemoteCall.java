package si.ijs.acs.objectexplorer.engine;

/**
 * <code>RemoteCall</code> interface defines the data
 * structure that packs the data about a single invocation
 * of an <code>Operation</code> instance. If the invocation
 * was synchronous and therefore produced no
 * <code>Invocation</code> instances, a single remote call
 * instance will fully describe and parametrize the remote call.
 * If the remote call was asynchronous and therefore produced
 * an <code>Invocation</code> instance, the remote call itself
 * will be parametrized by an instance of this object, while the
 * asynchronous responses (monitors, alarms, events etc.) will be
 * described by instances of <code>RemoteResponse</code> class.
 * An instance of this interface contains all information to
 * fully log the call.
 * Instances of this interface can describe either ordinary
 * <code>Operation</code> invocations or also <code>Attribute</code>
 * access - ie. calls to mutators and accessors. The type of the
 * data structure is determined by the return value of the
 * <code>isAttributeAccess()</code> method.
 */
public interface RemoteCall {
/**
 * If <code>isAttributeAccess()</code> returns <code>true</code>,
 * this method will return the reference to the attribute
 * object on which either accessor or mutator was invoked.
 * Otherwise the method will return <code>null</code>.
 *
 * @return attribute instance on which the operation was invoked
 */
Attribute getAttribute();
/**
 * In addition to the declared return value that each
 * operation can provide, an operation may also transfer
 * data as parameters to the operation which really count
 * as return values. Examples are CORBA OUT parameters.
 * Syntactically these are parameters, but in fact they are
 * only placeholders where the called remote function may
 * place the results. This method returns an array of objects
 * equal in length to the arrays describing parameters returned
 * by the <code>Operation</code> instances: for each parameter,
 * there is an entry in the array. For type IN parameters, the
 * fields are <code>null</code>. For other types of parameters
 * (auxilliary return values) the array contains the additional
 * return values produced by the remote call.
 * 
 * @return java.lang.Object[]
 */
java.lang.Object[] getAuxReturnValues();
/**
 * Returns the simple introspectable instance on which the
 * call was made.
 * 
 * @return introspectable instance (declared simple because
 *		   remote calls may be made on transient objects)
 */
SimpleIntrospectable getIntrospectable();
/**
 * Returns the operation that parametrizes the
 * reflective information about the remote call.
 * 
 * @return operation instance
 */
Operation getOperation();
/**
 * This method returns an array of objects representing the parameters
 * passed to the remote method. <b>Note: the method returns explicit
 * parameters, ie. parameters that were entered by the user before
 * they were processed by the engine. </b>
 *
 * @return an array of objects that GUI sent to the engine as
 * 		   parameters to the remote call
 */
Object[] getParameters();
/**
 * Returns the sequence number of the remote call. Enigne
 * is responsible for generation of unique sequences of numbers
 * that tag each call. The SNs can be used for logging to
 * link remote calls and the remote response objects.
 *
 * @return sequence number
 */
int getSN();
/**
 * Returns the return value that the remote call produced.
 * This is <code>null</code> if the remote call either returned
 * <code>null</code>, or the call was interrupted by either a timeout
 * or an exception being thrown.
 * 
 * @return return value of the remote call
 */
Object getSyncReturnValue();
/**
 * If the remote call or the invocation process generated
 * an exception, this method will return it. Otherwise
 * this method will return <code>null</code>.
 * 
 * @return exception thrown during the invocation process
 */
Throwable getThrowable();
/**
 * Returns <code>true</code> iff this remote call
 * represents attribute access, ie. the invocation
 * of either accessor or mutator on the remote attribute.
 *
 * @return true if either mutator or accessor methods are
 *		   parametrized by this call
 */
boolean isAttributeAccess();
/**
 * Returns <code>true</code> if, during the invocation of this method,
 * the engine determined that the remote object did not respond within
 * the timeout period. Timeout is defined by the engine.
 * 
 * @return <code>true</code> if the method invocation was
 *		   interrupted by the engine because of the detected
 *		   timeout condition
 */
boolean isTimeout();
/**
 * Returns <code>true</code> if, during the invocation of this method,
 * the engine determined that the remote object responded with error completion.
 * 
 * @return <code>true</code> if the method invocation was
 *		   completed with error
 */
boolean isErrorResponse();
}
