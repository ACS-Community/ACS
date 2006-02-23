/*
 * @@COPYRIGHT@@
 */
 
package abeans.pluggable.acs;

import java.util.logging.Level;

import abeans.core.Root;
import abeans.core.UnableToInstallComponentException;
import abeans.core.defaults.MessageLogEntry;
import abeans.datatypes.views.DefaultViewService;
import abeans.datatypes.views.ViewService;
import abeans.framework.ApplicationContext;

import com.cosylab.abeans.AbeansEngine;
import com.cosylab.lifecycle.LifecycleAdapter;
import com.cosylab.lifecycle.LifecycleEvent;
import com.cosylab.util.CommonException;

/**
 * Default ACS Abeans engine.
 *
 * <p>
 * When this engine is initialized, it trys to install itself as default
 * ApplicationContext to <code>ViewService</code> and when it is
 * destroyed, it trys to remove itself from <code>ViewService</code>.
 * </p>
 * 
 * @author	Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ACSAbeansEngine extends AbeansEngine
{

	/**
	 * Default <code>ApplicationContext</code> of <code>ACSAbeansEngine</code>(s).
	 * This member is static to make it easiy accessible to other objects.
	 * NOTE: this will work is JVM is not shareable, if two Abeans applications
	 * live in the same JVM you should be aware of this limitation.
	 * All consequence instatiotions of ACSAbeansEngines will override this member.
	 */
	private static ApplicationContext defaultApplicationContext = null;

	/**
	 * Default constructor, used by visual composition.
	 */
	public ACSAbeansEngine()
	{
		this("ACS Abeans Application");
	}
	
	/**
	 * Constructor for AbeansDALDemoEngine.
	 */
	public ACSAbeansEngine(String name)
	{
		super(name, "BACI");

		// install lifecycle adapter
		addLifecycleListener(new LifecycleAdapter()
		{
		
			// this AppCtx becomes default context for View service 
			public void initialized(LifecycleEvent event) throws CommonException {
				
				ViewService vs = null;

				try
				{
					vs = DefaultViewService.getViewService();
					if (vs != null)
						vs.setDefaultApplicationContext(ACSAbeansEngine.this.getApplicationContext());
						
				} catch (UnableToInstallComponentException e) {
					new MessageLogEntry(ACSAbeansEngine.this, "Failed to install default ViewService.",	e, Level.WARNING).dispatch();
				}

			}

			// if this AppCtx is default context for View service, then is removed 
			public void destroying(LifecycleEvent event)throws CommonException
			{
				ViewService vs = (ViewService)Root.getComponentManager().getComponent(ViewService.class);

				if (vs != null && vs.getDefaultApplicationContext() == ACSAbeansEngine.this.getApplicationContext())
					vs.setDefaultApplicationContext(null);
			}
			
		});

	}

	/**
	 * @see com.cosylab.abeans.AbeansEngine#userFrameworkInitialize()
	 */
	protected void userFrameworkInitialize()
	{
		defaultApplicationContext = getApplicationContext();
	}

	/**
	 * @see com.cosylab.abeans.AbeansEngine#userDestroy()
	 */
	protected void userDestroy()
	{
	}

	/**
	 * @see com.cosylab.abeans.AbeansEngine#userAllInitializationsDone()
	 */
	protected void userAllInitializationsDone()
	{
		new MessageLogEntry(this, "userAllInitializationsDone", "All initializations done.", Level.INFO).dispatch();
	}

	/**
	 * Returns default application context.
	 * 
	 * NOTE: this will work is JVM is not shareable, if two Abeans applications
	 * live in the same JVM you should be aware of this limitation.
	 * All consequence instatiotions of ACSAbeansEngines will override this member.
	 * @return default application context.
	 */
	public static ApplicationContext getDefaultApplicationContext() {
		return defaultApplicationContext;
	}

}
