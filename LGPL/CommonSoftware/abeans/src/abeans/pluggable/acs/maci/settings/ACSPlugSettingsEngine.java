/*
 * @@COPYRIGHT@@
 */
 
package abeans.pluggable.acs.maci.settings;

import abeans.core.Root;
import abeans.pluggable.acs.maci.ACSPlug;

import com.cosylab.abeans.AbeansEngine;

/**
 * ACS Plug Settings application engine.
 *
 * @author	Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ACSPlugSettingsEngine extends AbeansEngine
{

	/**
	 * ACS plug to be managed.
	 */
	ACSPlug plug;

	/**
	 * Constructor for ACSPlugSettingsEngine.
	 */
	public ACSPlugSettingsEngine()
	{
		super("ACSPlugSettingsEngine");
	}

	/**
	 * @see com.cosylab.abeans.AbeansEngine#userFrameworkInitialize()
	 */
	protected void userFrameworkInitialize()
	{
		plug = (ACSPlug)Root.getPlugs().getPlug(ACSPlug.PLUG_NAME);
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
	}

	/**
	 * Returns the name of the model that this application will use.
	 * It always return <code>null</code> because the application will not use
	 * any model, but will use Abeans Engine database directly.
	 * 
	 * @return <code>null</code>, no model will be used
	 * @see abeans.framework.ApplicationEngine#getModelName()
	 */
	public String getModelName()
	{
		return null;
	}

	/**
	 * Return ACS plug to be managed.
	 * @return ACS plug to be managed, might be <code>null</code>.
	 */
	public ACSPlug getPlug()
	{
		return plug;
	}

}
