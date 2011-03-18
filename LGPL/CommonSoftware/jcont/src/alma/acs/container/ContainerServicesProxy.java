package alma.acs.container;

import java.util.concurrent.ThreadFactory;

import org.omg.CORBA.Object;
import org.omg.PortableServer.Servant;

import com.cosylab.CDB.DAL;

import si.ijs.maci.ComponentSpec;

import alma.ACS.OffShoot;
import alma.ACS.OffShootOperations;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentDescriptor;
import alma.acs.component.ComponentQueryDescriptor;
import alma.acs.component.ComponentStateManager;
import alma.acs.logging.AcsLogger;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.AcsEventSubscriber;
import alma.entities.commonentity.EntityT;

/**
 * A proxy for {@linkplain ContainerServices} that is specifically meant to isolate implementations of the 
 * Alma OMC's <code>PluginContainerServices</code> interface (which extends ContainerServices) 
 * from changes in the ACS container services.
 * <p>
 * PluginContainerServicesImpl should extend ContainerServicesProxy and implement only the methods from PluginContainerServices
 * that are not part of ContainerServices.
 * <p>
 * Note that the OMC is the only known case outside of ACS that should implement or extend ContainerServices. 
 * 
 * @author hsommer
 */
public class ContainerServicesProxy implements ContainerServices
{
	protected final ContainerServices delegate;
	
	public ContainerServicesProxy(ContainerServices delegate) {
		this.delegate = delegate;
	}
	
	/**
	 * @throws RuntimeException, see {@link #check()}
	 * @see alma.acs.container.ContainerServicesBase#getName()
	 */
	@Override
	public String getName() {
		return delegate.getName();
	}

	/**
	 * @throws RuntimeException, see {@link #check()}
	 * @see alma.acs.container.ContainerServicesBase#getLogger()
	 */
	@Override
	public AcsLogger getLogger() {
		return delegate.getLogger();
	}

	/**
	 * @see alma.acs.container.ContainerServicesBase#getCDB()
	 */
	@Override
	public DAL getCDB() throws AcsJContainerServicesEx {
		check();
		return delegate.getCDB();
	}

	/**
	 * @see alma.acs.container.ContainerServicesBase#activateOffShoot(org.omg.PortableServer.Servant)
	 */
	@Override
	public <T extends Servant & OffShootOperations> OffShoot activateOffShoot(T cbServant)
			throws AcsJContainerServicesEx {
		check();
		return delegate.activateOffShoot(cbServant);
	}

	/**
	 * @see alma.acs.container.ContainerServices#activateOffShoot(alma.acs.container.OffShootOperations, java.lang.Class)
	 */
	@Override
	public <T extends OffShootOperations> OffShoot activateOffShoot(T offshootImpl, Class<T> idlOpInterface)
			throws AcsJContainerServicesEx {
		check();
		return delegate.activateOffShoot(offshootImpl, idlOpInterface);
	}

	/**
	 * @throws AcsJContainerServicesEx
	 * @see alma.acs.container.ContainerServicesBase#deactivateOffShoot(java.lang.Object)
	 */
	@Override
	public void deactivateOffShoot(java.lang.Object offshootImpl) throws AcsJContainerServicesEx {
		check();
		delegate.deactivateOffShoot(offshootImpl);
	}

	/**
	 * @see alma.acs.container.ContainerServicesBase#getAdvancedContainerServices()
	 */
	@Override
	public AdvancedContainerServices getAdvancedContainerServices() {
		return delegate.getAdvancedContainerServices();
	}

	/**
	 * @see alma.acs.container.ContainerServicesBase#getThreadFactory()
	 */
	@Override
	public ThreadFactory getThreadFactory() {
		return delegate.getThreadFactory();
	}


	/**
	 * @throws RuntimeException, see {@link #check()}
	 * @see alma.acs.container.ContainerServices#getComponentStateManager()
	 */
	@Override
	public ComponentStateManager getComponentStateManager() {
		return delegate.getComponentStateManager();
	}

	/**
	 * @see alma.acs.container.ContainerServices#getComponent(java.lang.String)
	 */
	@Override
	public Object getComponent(String componentUrl) throws AcsJContainerServicesEx {
		check();
		return delegate.getComponent(componentUrl);
	}

	/**
	 * @see alma.acs.container.ContainerServices#getComponentNonSticky(java.lang.String)
	 */
	@Override
	public Object getComponentNonSticky(String curl) throws AcsJContainerServicesEx {
		check();
		return delegate.getComponentNonSticky(curl);
	}

	/**
	 * @see alma.acs.container.ContainerServices#getDefaultComponent(java.lang.String)
	 */
	@Override
	public Object getDefaultComponent(String componentIDLType) throws AcsJContainerServicesEx {
		check();
		return delegate.getDefaultComponent(componentIDLType);
	}

	/**
	 * @see alma.acs.container.ContainerServices#getCollocatedComponent(java.lang.String, java.lang.String)
	 */
	@Override
	public Object getCollocatedComponent(String compUrl, String targetCompUrl) throws AcsJContainerServicesEx {
		check();
		return delegate.getCollocatedComponent(compUrl, targetCompUrl);
	}

	/**
	 * @see alma.acs.container.ContainerServices#getCollocatedComponent(alma.acs.component.ComponentQueryDescriptor, boolean, java.lang.String)
	 */
	@Override
	public Object getCollocatedComponent(ComponentQueryDescriptor compSpec, boolean markAsDefaul, String targetCompUrl)
			throws AcsJContainerServicesEx {
		check();
		return delegate.getCollocatedComponent(compSpec, markAsDefaul, targetCompUrl);
	}

	/**
	 * @see alma.acs.container.ContainerServices#getDynamicComponent(alma.acs.component.ComponentQueryDescriptor, boolean)
	 */
	@Override
	public Object getDynamicComponent(ComponentQueryDescriptor compSpec, boolean markAsDefault)
			throws AcsJContainerServicesEx {
		check();
		return delegate.getDynamicComponent(compSpec, markAsDefault);
	}

	/**
	 * @deprecated
	 * @see alma.acs.container.ContainerServices#getDynamicComponent(alma.acs.container.ComponentSpec, boolean)
	 */
	@Override
	public Object getDynamicComponent(ComponentSpec compSpec, boolean markAsDefault) throws AcsJContainerServicesEx {
		check();
		return delegate.getDynamicComponent(compSpec, markAsDefault);
	}

	/**
	 * @throws RuntimeException, see {@link #check()}
	 * @see alma.acs.container.ContainerServices#findComponents(java.lang.String, java.lang.String)
	 */
	@Override
	public String[] findComponents(String curlWildcard, String typeWildcard) throws AcsJContainerServicesEx {
		check();
		return delegate.findComponents(curlWildcard, typeWildcard);
	}

	/**
	 * @throws RuntimeException, see {@link #check()}
	 * @see alma.acs.container.ContainerServices#getComponentDescriptor(java.lang.String)
	 */
	@Override
	public ComponentDescriptor getComponentDescriptor(String componentUrl) throws AcsJContainerServicesEx {
		check();
		return delegate.getComponentDescriptor(componentUrl);
	}

	/**
	 * @see alma.acs.container.ContainerServices#releaseComponent(java.lang.String)
	 */
	@Override
	public void releaseComponent(String componentUrl) {
		delegate.releaseComponent(componentUrl);
	}

	public void releaseComponent(String componentUrl, ComponentReleaseCallback callback) {
		delegate.releaseComponent(componentUrl, callback);
	}
	
	/**
	 * @see alma.acs.container.ContainerServices#registerComponentListener(alma.acs.container.ContainerServices.ComponentListener)
	 */
	@Override
	public void registerComponentListener(ComponentListener listener) {
		delegate.registerComponentListener(listener);
	}

	/**
	 * @see alma.acs.container.ContainerServices#getReferenceWithCustomClientSideTimeout(org.omg.CORBA.Object, double)
	 */
	@Override
	public Object getReferenceWithCustomClientSideTimeout(Object originalCorbaRef, double timeoutSeconds)
			throws AcsJContainerServicesEx {
		check();
		return delegate.getReferenceWithCustomClientSideTimeout(originalCorbaRef, timeoutSeconds);
	}

	/**
	 * @see alma.acs.container.ContainerServices#assignUniqueEntityId(alma.acs.container.EntityT)
	 */
	@Override
	public void assignUniqueEntityId(EntityT entity) throws AcsJContainerServicesEx {
		check();
		delegate.assignUniqueEntityId(entity);
	}

	/**
	 * @see alma.acs.container.ContainerServices#getTransparentXmlWrapper(java.lang.Class, java.lang.Object, java.lang.Class)
	 */
	@Override
	public <T, F> T getTransparentXmlWrapper(Class<T> transparentXmlIF, F objectReference, Class<F> flatXmlIF)
			throws AcsJContainerServicesEx {
		check();
		return delegate.getTransparentXmlWrapper(transparentXmlIF, objectReference, flatXmlIF);
	}

	/**
	 * @see alma.acs.container.ContainerServices#createNotificationChannelSubscriber(String)
	 */
	public AcsEventSubscriber createNotificationChannelSubscriber(String channelName) throws AcsJContainerServicesEx {
		return delegate.createNotificationChannelSubscriber(channelName);
	}

	/**
	 * @see alma.acs.container.ContainerServices#createNotificationChannelSubscriber(String, String)
	 */
	public AcsEventSubscriber createNotificationChannelSubscriber(String channelName, String channelNotifyServiceDomainName) throws AcsJContainerServicesEx {
		return delegate.createNotificationChannelSubscriber(channelName, channelNotifyServiceDomainName);
	}

	/**
	 * @see ContainerServices#createNotificationChannelPublisher(String)
	 */
	@Override
	public AcsEventPublisher createNotificationChannelPublisher(String channelName) throws AcsJContainerServicesEx {
		return delegate.createNotificationChannelPublisher(channelName);
	}

	/**
	 * @see ContainerServices#createNotificationChannelPublisher(String, String)
	 */
	@Override
	public AcsEventPublisher createNotificationChannelPublisher(String channelName, String channelNotifyServiceDomainName) throws AcsJContainerServicesEx {
		return delegate.createNotificationChannelPublisher(channelName, channelNotifyServiceDomainName);
	}

	@Override
	public void raiseAlarm(String faultFamily, String faultMember, int faultCode) throws AcsJContainerServicesEx {
		delegate.raiseAlarm(faultFamily, faultMember, faultCode);
	}

	@Override
	public void clearAlarm(String faultFamily, String faultMember, int faultCode) throws AcsJContainerServicesEx {
		delegate.clearAlarm(faultFamily, faultMember, faultCode);
	}


	/**
	 * Here the subclass can implement checks that get executed before any delegation call is made. 
	 * See <code>protected void checkPluginRunning() throws PluginContainerException</code> in the OMC.
	 * @throws RuntimeException
	 */
	protected void check() throws AcsJContainerServicesEx {
		// overload if you need the checks
	}
}
