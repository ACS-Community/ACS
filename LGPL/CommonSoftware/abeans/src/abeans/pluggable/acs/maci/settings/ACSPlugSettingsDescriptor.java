/*
 * @@COPYRIGHT@@
 */
 
package abeans.pluggable.acs.maci.settings;

import com.cosylab.gui.framework.DefaultLaunchableDescriptor;

/**
 * ACS Plug Settings application descriptor.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class ACSPlugSettingsDescriptor extends DefaultLaunchableDescriptor
{

	/**
	 * Constrcutor.
	 */
	public ACSPlugSettingsDescriptor()
	{
		// checkOtherInstances, shareJVM, runAsInternalFrame, separateAppletFrame
		super(false, true, false, true);
	}

}
