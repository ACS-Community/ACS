/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.eventbrowser.views;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import alma.acs.eventbrowser.Application;
import alma.acs.eventbrowser.model.AbstractNotifyServiceElement;
import alma.acs.eventbrowser.model.ChannelParticipantName;
import alma.acs.eventbrowser.model.ChannelConsumers;
import alma.acs.eventbrowser.model.ChannelSuppliers;
import alma.acs.eventbrowser.model.INames;
import alma.acs.eventbrowser.model.NotifyServices;
import alma.acs.eventbrowser.model.ChannelData;
import alma.acs.eventbrowser.model.MCStatistics;
import alma.acs.eventbrowser.model.NotifyServiceData;
import alma.acs.eventbrowser.model.SlowestConsumers;

/** This class implements the Eclipse version of the Adapter pattern, as described in pp. 75ff of
 * "Eclipse Rich Client Platform," second edition, by McAffer, Lemieux and Aniszczik. These adapters are
 * used to separate the hierarchical model of NotifyService / ChannelData / MCStatistics ... from the 
 * ChannelTreeView (i.e., the GUI). Each adapter must also be (and is!) registered with
 * Platform.getAdapterManager().registerAdapters() in order for the TreeViewer's Content and Label
 * Providers to be able to use them.
 * @author jschwarz
 *
 */
public class EventGuiAdapterFactory implements IAdapterFactory {

	public EventGuiAdapterFactory() {

	}
	private IWorkbenchAdapter channelAdapter = new IWorkbenchAdapter() {
		public Object getParent(Object o) {
			return ((AbstractNotifyServiceElement)o).getParent();
		}
		public String getLabel(Object o) {
			return ((AbstractNotifyServiceElement)o).getName();
		}
		public ImageDescriptor getImageDescriptor(Object object) {
			String imageKey = ISharedImages.IMG_OBJ_FOLDER;
			return AbstractUIPlugin.imageDescriptorFromPlugin(
					Application.PLUGIN_ID, imageKey);
		}
		public Object[] getChildren(Object o) {
			ChannelData cd = (ChannelData)o;
			return cd.getStatistics().toArray();
		}
	};
	private IWorkbenchAdapter notifyServiceAdapter = new IWorkbenchAdapter() {

		@Override
		public Object[] getChildren(Object o) {
			return ((NotifyServiceData)o).getChannels().toArray();
		}

		@Override
		public ImageDescriptor getImageDescriptor(Object object) {
			String imageKey = ISharedImages.IMG_OBJ_FOLDER;
			return AbstractUIPlugin.imageDescriptorFromPlugin(
					Application.PLUGIN_ID, imageKey);
		}

		@Override
		public String getLabel(Object o) {
			return ((AbstractNotifyServiceElement)o).getName();
		}

		@Override
		public Object getParent(Object o) {
			// TODO Auto-generated method stub
			return null;
		}
		
	};
	
	private IWorkbenchAdapter rootOfNotifyServicesAdapter = new IWorkbenchAdapter() {
		
		@Override
		public Object getParent(Object o) {
			return null; // No parent, we're at the top of the tree
		}
		
		@Override
		public String getLabel(Object o) {
			return "Notify Services";
		}
		
		@Override
		public ImageDescriptor getImageDescriptor(Object object) {
			// TODO Auto-generated method stub
			return null;
		}
		
		@Override
		public Object[] getChildren(Object o) {
			return ((NotifyServices)o).getServices().toArray();
		}
	};
	
	private IWorkbenchAdapter statisticsAdapter = new IWorkbenchAdapter() {

		@Override
		public Object[] getChildren(Object o) {
			if (o instanceof INames)
				return ((INames)o).getNames();
			return new Object[0];
		}

		@Override
		public ImageDescriptor getImageDescriptor(Object object) {
			String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
			return AbstractUIPlugin.imageDescriptorFromPlugin(
					Application.PLUGIN_ID, imageKey);
		}

		@Override
		public String getLabel(Object o) {
			return ((MCStatistics)o).getStatistics();
		}

		@Override
		public Object getParent(Object o) {
			return ((MCStatistics)o).getParent();
		}
		
	};
	
	private IWorkbenchAdapter nameAdapter = new IWorkbenchAdapter() {

		@Override
		public Object[] getChildren(Object o) {
			// TODO Auto-generated method stub
			return new Object[0];
		}

		@Override
		public ImageDescriptor getImageDescriptor(Object object) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public String getLabel(Object o) {
			// TODO Auto-generated method stub
			return ((ChannelParticipantName)o).getName();
		}

		@Override
		public Object getParent(Object o) {
			// TODO Auto-generated method stub
			return ((ChannelParticipantName)o).getParent();
		}
		
	};


	@Override
	public Object getAdapter(Object adaptableObject, Class
			adapterType) {
		if (adaptableObject instanceof ChannelData && adapterType == IWorkbenchAdapter.class)
			return channelAdapter;
		if (adaptableObject instanceof NotifyServiceData && adapterType == IWorkbenchAdapter.class)
			return notifyServiceAdapter;
		if (adaptableObject instanceof NotifyServices && adapterType == IWorkbenchAdapter.class)
			return rootOfNotifyServicesAdapter;
		if (adaptableObject instanceof MCStatistics && adapterType == IWorkbenchAdapter.class)
			return statisticsAdapter;
		if (adaptableObject instanceof ChannelParticipantName && adapterType == IWorkbenchAdapter.class)
			return nameAdapter;	
		return null;
	}

	@Override
	public Class[] getAdapterList() {
		return new Class[] {IWorkbenchAdapter.class};
	}

}
