/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.cdb.dal;

import javax.naming.Binding;
import javax.naming.Name;
import javax.naming.NamingException;

import abeans.models.meta.TargetPart;
import abeans.pluggable.DefaultRemoteInfo;

/**
 * This class is a CDBDAL plug implementation of <code>RemoteInfo</code> interface.
 * As such, it represents the target in the remote layer, be it an action, a query, 
 * a namespace etc. The meaning of the target is defined by the contract between the 
 * entity submitting the remote request (usually modeling layer) and the entity processing
 * it (usually the plug). 
 * The most important feature of the <code>RemoteInfo</code> implementation is to produce
 * a consistent representation of that remote target. The consistency request is imposed 
 * on the agreement between different name forms that can exist for the same target. In
 * other words, target can be represented as an Abeans URI, for instance
 * <p>
 * <b>abeans-CDBDAL://dina.ijs.si:xxxx/PSBEND_M.01/current
 * </p>
 * or it may be represented as a JNDI <code>javax.naming.Name</code> sequence:
 * <p>
 * [0]=abeans-CDBDAL [1]=dina.ijs.si:xxxx [2]=PSBEND_M.01 [3]=current
 * </p>
 * or it may be a simple string.
 * The default implementation allows all these conversions and is able to construct a new name 
 * from components like <code>remoteName</code>, URI <code>authority</code>, <code>plugType</code>
 * and so on. For detailed documentation see <code>RemoteInfo</code> interface javadoc.
 * <p>
 * This implementation buffers the <code>URI</code>, so once it has been created for the 
 * first time, the invocations of <code>toURI()</code> return immediately. Moreover, this
 * class implements <code>javax.naming.Name</code> directly.
 * </p>
 * <p>
 * As an instance of <code>javax.naming.Name</code> this instance is unmodifiable, i.e. all
 * methods adding or removing a name component will fail with an exception.
 * </p>
 *
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 * @see		DefaultRemoteInfo
 * @see		RemoteInfo
 */
public class CDBDALRemoteInfo extends DefaultRemoteInfo
{

	/**
	 * Creates a new remote info object from the name of the remote entity, the authority and
	 * the plug type.
	 * This constructor will usually be used to name connectable entities.
	 * 
	 * @param		remoteName			the name of the connectable entity; this name may contain
	 * 									slash hierarchical separators, non-<code>null</code>
	 * @param		authority			an optional namespace authority (such as a name server,
	 * 									context server, device manager etc); i.e. that is the
	 * 									entity in the scope of which the <code>remoteName</code>
	 * 									becomes a unique remote target designation; may be
	 * 									<code>null</code> iff the plug can either provide a 
	 * 									default value or the system runs on top of a global
	 * 									namespace
	 * @param		plugType			the name of the plug under which this remote info is
	 * 									being issued; this is the plug where the remote target
	 * 									runs; non-<code>null</code>
	 */
	public CDBDALRemoteInfo(
		String remoteName,
		String authority,
		String plugType) throws NamingException
	{
		super(remoteName, authority, plugType);
	}

	/**
	 * Creates a new remote info object from a sequence of JNDI names <b>and</b> the corresponding
	 * sequence of <code>TargetPart</code> instances that define the meaning of each successive
	 * name component. This constructor is mostly used to build <code>RemoteInfo</code> instances
	 * from names obtained from <code>abeans.models.DistributedDirectory</code>.
	 * 
	 * @param		name				an array of names; non-<code>null</code>, each component
	 * 									non-<code>null</code>; each component is non-hierarchical
	 * 									(does not contain slash separators)
	 * @param		targets				an array, non-<code>null</code>, each component
	 * 									non-<code>null</code> of the same length as <code>name</code>;
	 * 									each entry in this array describes the meaning of the name
	 * 									component with the same index
	 */
	public CDBDALRemoteInfo(Name name, TargetPart[] targets) throws NamingException
	{
		super(name, targets);
	}

	/**
	 * This constructor is used to create a name from the tree path obtained from
	 * the <code>abeans.models.DistributedDirectory</code>. The binding array must contain,
	 * in each binding, a name and an instance of <code>ModelingElementDescriptor</code>. From
	 * the descriptors, this instance will extract the target part information that will be of help
	 * when identifying the meaning of each name component.
	 * 
	 * @param		bindings			the array of bindings (name - descriptor pairs) that represent
	 * 									some target in the distributed directory; a name for that
	 * 									target will be created by this constructor; non-<code>null</code>,
	 * 									each component is non-<code>null</code>.
	 */
	public CDBDALRemoteInfo(Binding[] bindings) throws NamingException
	{
		super(bindings);
	}

	/**
	 * Constructor for CDBDALRemoteInfo.
	 * @param remoteName
	 * @param component
	 * @param query
	 * @param authority
	 * @param plugType
	 */
	public CDBDALRemoteInfo(
		String remoteName,
		String query,
		String authority,
		String plugType) throws NamingException
	{
		super(remoteName, query, authority, plugType);
	}

}
