/*
 *    ALMA - Atacama Large Millimeter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2009
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package cl.utfsm.acs.acg.gui;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "cl.utfsm.acs.acg";

	// The shared instance
	private static Activator plugin;

	// Shared images across the plugin
	public static final String IMG_ALARM_SYSTEM = "image.alert";
	public static final String IMG_SOURCE = "image.add";
	public static final String IMG_ALARM = "image.alarm";
	public static final String IMG_LINK  = "image.link";
	public static final String IMG_TICKET  = "image.ticket";
	public static final String IMG_FAULTMEMBERS  = "image.faultmembers";
	public static final String IMG_FAULTCODES  = "image.faultcodes";
	public static final String IMG_REDUCTIONS = "image.reductions";

	/**
	 * The constructor
	 */
	public Activator() {
		plugin = this;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	/**
	 * Returns an image descriptor for the image file at the given
	 * plug-in relative path
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#initializeImageRegistry(org.eclipse.jface.resource.ImageRegistry)
	 */
	protected void initializeImageRegistry(ImageRegistry reg) {
		reg.put(IMG_ALARM_SYSTEM, getImageDescriptor("icons/alert.gif"));
		reg.put(IMG_SOURCE, getImageDescriptor("icons/add.gif"));
        reg.put(IMG_ALARM, getImageDescriptor("icons/alarm.gif"));
        reg.put(IMG_LINK, getImageDescriptor("icons/link.png"));
        reg.put(IMG_REDUCTIONS, getImageDescriptor("icons/reduction.png"));
        reg.put(IMG_TICKET, getImageDescriptor("icons/ticket.png"));
        reg.put(IMG_FAULTMEMBERS, ImageDescriptor.createFromURL(
        		FileLocator.find(Platform.getBundle("org.eclipse.ui"),
        				new Path("icons/full/obj16/generic_elements.gif"),
        				null)));
        reg.put(IMG_FAULTCODES, ImageDescriptor.createFromURL(
        		FileLocator.find(Platform.getBundle("org.eclipse.ui"),
        				new Path("icons/full/eview16/problems_view.gif"),
        				null)));
	}

}