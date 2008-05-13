package si.ijs.acs.objectexplorer.engine;

/**
 * Introspectable extends the <code>SimpleIntrospectable</code>
 * interface and adds the connection capability to the parent
 * interface. While the simple introspectable cannot
 * connect to the remote object by itself and needs to obtain
 * the remote reference from another introspectable, instances
 * of this interface have the described capability. For example,
 * it is impossible to establish connection to transient
 * objects, such as those represented by <code>Invocations</code>.
 * These are for instance control objects that are returned when
 * subscription is created in publish / subscribe design pattern.
 * The reference to transient objects is obtained by creating a
 * transient object on an introspectable.
 * Introspectables also follow a life cycle convention: when
 * an introspectable is returned by the remote access interface
 * it must be connected before its introspective functions
 * are called. It must be disconnected when it is no longer
 * used. GUI defines when a remote object is no longer needed.
 * Note that there is no one-to-one correspondence between
 * the introspectable instances and tree nodes, because tree
 * nodes can exist that are not introspectables
 * (as artefacts of the naming service, for instance).
 */
public interface Introspectable extends SimpleIntrospectable {
/**
 * When this method is called, the engine implementation
 * must obtain all relevant information from the remote
 * system to initialize this instance. After the call
 * completes, the introspective methods must return
 * correct introspective information. If this method
 * is called on an already connected object, it should
 * return NOP.
 */
void connect();
/**
 * Call when the introspectable is no longer used.
 * The call will release the introspectable resources.
 * A call on an already disconnected object does NOP.
 * GUI determines, when the object is no longer used.
 * Note that as a side effect, the introspectable that
 * is being disconnected must also destroy all
 * invocations that it has spawned. Note also, that
 * an introspectable must destroy all of its nodes
 * before it destroys itself.
 */
void disconnect();
/**
 * This method is needed by the OE GUI to render tree cells according to
 * the connection state of the node, and to show an appropriate popup menu
 *
 * @return boolean connection state flag
 */
boolean isConnected();
/**
 * This method is needed by the OE GUI to render tree cells according to
 * the connection state of the node, and to show an appropriate popup menu
 *
 * @return boolean non-sticky (objexp is not an owner) state flag
 */
boolean isNonSticky();
}
