/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.manager.gui;

import java.awt.Image;

import com.cosylab.gui.framework.LaunchableDescriptor;

/**
 * <code>LaunchableDescriptor</code> for Manager application.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ManagerDescriptor implements LaunchableDescriptor
{

	/**
	 * @see com.cosylab.gui.framework.LaunchableDescriptor#checkOtherInstances()
	 */
	public boolean checkOtherInstances()
	{
		return false;
	}

	/**
	 * @see com.cosylab.gui.framework.LaunchableDescriptor#shareJVM()
	 */
	public boolean shareJVM()
	{
		return false;
	}

	/**
	 * @see com.cosylab.gui.framework.LaunchableDescriptor#runAsInternalFrame()
	 */
	public boolean runAsInternalFrame()
	{
		return false;
	}

	/**
	 * @see com.cosylab.gui.framework.LaunchableDescriptor#separateAppletFrame()
	 */
	public boolean separateAppletFrame()
	{
		return false;
	}

	/**
	 * @see com.cosylab.gui.framework.LaunchableDescriptor#getIconImage()
	 */
	public Image getIconImage()
	{
		return null;
	}

}
