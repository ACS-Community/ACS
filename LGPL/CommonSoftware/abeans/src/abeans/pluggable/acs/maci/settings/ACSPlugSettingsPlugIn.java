/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.maci.settings;

import abeans.framework.ApplicationContext;
import abeans.pluggable.Plug;
import abeans.pluggable.acs.maci.ACSPlug;

import com.cosylab.abeans.AbeansAction;
import com.cosylab.abeans.AbeansEngine;
import com.cosylab.abeans.LaunchAction;
import com.cosylab.abeans.plugins.AbeansSystemMenuPlugIn;
import com.cosylab.gui.core.CosyApplicationPanel;
import com.cosylab.gui.core.PlugIn;
import com.cosylab.gui.core.PlugInException;
import com.cosylab.gui.core.PlugInManager;
import com.cosylab.gui.plugins.ActionPlugIn;

/**
 * ACS Plug Setting plug-in.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class ACSPlugSettingsPlugIn implements PlugIn
{
	/**
	 * Parent.
	 */
	private CosyApplicationPanel parent = null;
	
	/**
	 * @see PlugIn#installPlugIn(PlugInManager)
	 */
	public void installPlugIn(PlugInManager manager) throws PlugInException
	{
		assert (manager != null);

		// try to install AbeansSystemMenuPlugIn
		if (manager.getPlugIn(AbeansSystemMenuPlugIn.class) == null)
			manager.installPlugIn(AbeansSystemMenuPlugIn.class);

			
		// get ActionPlugIn
		ActionPlugIn act = (ActionPlugIn)manager.getPlugIn(ActionPlugIn.class);
		
		// if null, try to install
		if (act == null)
		{
			manager.installPlugIn(ActionPlugIn.class); 
			act = (ActionPlugIn)manager.getPlugIn(ActionPlugIn.class);
		}
		
		// check
		if (act == null)
			throw new PlugInException(this, "Will not install '" + getName() + "' because 'ActionPlugIn' cannot be installed.", this.getClass(), null);

		// add plug actions

		ApplicationContext ctx = ((AbeansEngine)parent.getApplication()).getApplicationContext();
		Plug[] plgs = ctx.getPlugs();
		for (int i = 0; i < plgs.length; i++)
		{
			if (plgs[i].getName().equals(ACSPlug.PLUG_NAME))
				act.put(new LaunchAction("ACS Plug", ACSPlugSettings.class.getName(), parent.findLaunchable().getLauncher(), AbeansAction.TYPE_ABEANS_PLUG));
		}
		
	}

	/**
	 * @see CosyComponent#getName()
	 */
	public String getName()
	{
		return getClass().getName();
	}

	/**
	 * @see CosyComponent#getCosyPanelParent()
	 */
	public CosyApplicationPanel getCosyPanelParent()
	{
		return parent;
	}

	/**
	 * @see CosyComponent#destroy()
	 */
	public void destroy()
	{
	}

	/**
	 * @see CosyComponent#setCosyPanelParent(CosyApplicationPanel)
	 */
	public void setCosyPanelParent(CosyApplicationPanel panel)
	{
		this.parent = panel;
	}

}
