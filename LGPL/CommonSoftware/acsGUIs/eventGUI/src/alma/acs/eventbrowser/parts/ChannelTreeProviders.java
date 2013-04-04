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
package alma.acs.eventbrowser.parts;

import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

import alma.acs.eventbrowser.model.AbstractNotifyServiceElement;
import alma.acs.eventbrowser.model.ChannelData;
import alma.acs.eventbrowser.model.ChannelParticipantName;
import alma.acs.eventbrowser.model.EventModel;
import alma.acs.eventbrowser.model.INames;
import alma.acs.eventbrowser.model.MCStatistics;
import alma.acs.eventbrowser.model.NotifyServiceData;
import alma.acs.eventbrowser.model.NotifyServices;


/**
 * Code is derived from the older class alma/acs/eventbrowser/views/EventGuiAdapterFactory.java, 
 * although here in E4 we do not yet use an adapter pattern but a straight 'instanceof'-based check 
 * for the different levels of the tree hierarchy.
 * TODO: Check http://wiki.eclipse.org/E4/EAS/Adapting_Objects if we can also do it in e4.
 * 
 * TODO: Find replacement for E3 org.eclipse.ui.model.BaseWorkbenchContentProvider
 *       Check out org.eclipse.jface.databinding.viewers.ObservableListTreeContentProvider.
 *       See local project 'NotifyDomainEmf' about using EMF generated domain model classes.
 */
public class ChannelTreeProviders {
	
	static class ChannelTreeContentProvider implements ITreeContentProvider {
		
		private final EventModel eventModel;

		ChannelTreeContentProvider(EventModel eventModel) {
			this.eventModel = eventModel;
		}
		
		@Override
		public void dispose() {
		}

		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// cannot happen -- we always work on the same domain model (regardless of selections in other parts etc)
		}

		@Override
		public Object[] getElements(Object inputElement) {
			NotifyServices allServices = eventModel.getNotifyServiceTotals();
			return allServices.getServices().toArray();
		}

		@Override
		public Object[] getChildren(Object parentElement) {
			if (parentElement instanceof NotifyServices) {
				// the invisible root node. Not sure if this gets called ever.
				return ((NotifyServices)parentElement).getServices().toArray();
			}
			else if (parentElement instanceof NotifyServiceData) {
				// a notify service node
				return ((NotifyServiceData)parentElement).getChannels().toArray();
			}
			else if (parentElement instanceof ChannelData) {
				// NC node
				return ((ChannelData)parentElement).getStatistics().toArray();
			}
			else if (parentElement instanceof MCStatistics) {
				// NC connection and performance details.
				// This could be ChannelConsumers, ChannelSuppliers, ChannelQueueSize, SlowestConsumers
				if (parentElement instanceof INames) {
					// this could be ChannelConsumers, ChannelSuppliers, or SlowestConsumers
					return ((INames)parentElement).getNames();
				}
				return new Object[0];
			}
			return null;
		}

		@Override
		public Object getParent(Object element) {
			if (element instanceof NotifyServices) {
				return null;
			}
			else if (element instanceof NotifyServiceData) {
				return null;
			}
			else if (element instanceof ChannelData) {
				return ((ChannelData)element).getParent();
			}
			else if (element instanceof MCStatistics) {
				return ((MCStatistics)element).getParent();
			}
			else if (element instanceof ChannelParticipantName) {
				return ((ChannelParticipantName)element).getParent();
			}
			return null;
		}

		@Override
		public boolean hasChildren(Object element) {
			// If this is a performance problem, we already had it before, 
			// in org.eclipse.ui.model.BaseWorkbenchContentProvider.hasChildren(Object)
			Object[] children = getChildren(element);
			return ( children != null && children.length > 0 );
		}
		
	}
	
	
	/**
	 * TODO: Inherit from StyledLabelProvider, see http://www.vogella.com/articles/EclipseJFaceTree/article.html 
	 */
	static class ChannelTreeLabelProvider extends LabelProvider {
		@Override
		public String getText(Object element) {
			if (element instanceof NotifyServices) {
				// the invisible root node. Not sure if this gets called ever.
				return "Notify Services";
			}
			else if (element instanceof NotifyServiceData) {
				// a notify service node
				return ((AbstractNotifyServiceElement)element).getName();
			}
			else if (element instanceof ChannelData) {
				// NC node
				return ((AbstractNotifyServiceElement)element).getName();
			}
			else if (element instanceof MCStatistics) {
				return ((MCStatistics)element).getStatistics();
			}
			else if (element instanceof ChannelParticipantName) {
				return ((ChannelParticipantName)element).getName();
			}
			else {
				return "unexpected tree node " + element.getClass();
			}
		}

		@Override
		public Image getImage(Object element) {
			/*
			 * NotifyService:
			 * 			String imageKey = ISharedImages.IMG_OBJ_FOLDER;
			 * 			return AbstractUIPlugin.imageDescriptorFromPlugin(Application.PLUGIN_ID, imageKey);
			 * Channel: 
			 * 			String imageKey = ISharedImages.IMG_OBJ_FOLDER;
			 * 			return AbstractUIPlugin.imageDescriptorFromPlugin(Application.PLUGIN_ID, imageKey);
			 * 
			 * Statistics:
			 * 			String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
			 *			return AbstractUIPlugin.imageDescriptorFromPlugin(Application.PLUGIN_ID, imageKey);
			 */
			Image ret = null; // TODO: instanceof ... getImage(...)
			return ret;
		}

		/**
		 * Helper Method to load the images.
		 * Todo: cache in a map
		 */
		private Image getImage(String file) {
			Bundle bundle = FrameworkUtil.getBundle(ChannelTreeLabelProvider.class);
			URL url = FileLocator.find(bundle, new Path("icons/" + file), null);
			ImageDescriptor image = ImageDescriptor.createFromURL(url);
			return image.createImage();
		}
	}


}
