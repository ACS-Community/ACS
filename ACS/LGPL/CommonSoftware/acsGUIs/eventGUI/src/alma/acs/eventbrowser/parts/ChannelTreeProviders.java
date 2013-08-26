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
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.StyledCellLabelProvider;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.StyledString.Styler;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

import alma.acs.eventbrowser.model.ChannelConsumers;
import alma.acs.eventbrowser.model.ChannelData;
import alma.acs.eventbrowser.model.ChannelParticipantName;
import alma.acs.eventbrowser.model.ChannelSuppliers;
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
		
		@Override
		public void dispose() {
		}

		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// cannot happen -- we always work on the same domain model (regardless of selections in other parts etc)
		}

		@Override
		public Object[] getElements(Object inputElement) {
			if (inputElement instanceof NotifyServices) {
				NotifyServices allServices = (NotifyServices) inputElement;
				return allServices.getServices().toArray();
			}
			System.out.println("*** ChannelTreeContentProvider#getElements(Object) called with unexpected arg=" + inputElement.getClass().getName());
			return null;
		}

		@Override
		public Object[] getChildren(Object parentElement) {
			if (parentElement instanceof NotifyServiceData) {
				// a notify service node
				return ((NotifyServiceData)parentElement).getChannels().toArray();
			}
			else if (parentElement instanceof ChannelData) {
				// NC node
				return ((ChannelData)parentElement).getStatistics().toArray();
			}
			else if (parentElement instanceof MCStatistics) {
				return ((MCStatistics)parentElement).getChildren();
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
	 * See http://www.vogella.com/articles/EclipseJFaceTree/article.html 
	 */
	static class ChannelTreeLabelProvider extends StyledCellLabelProvider /* implements IFontProvider*/ {
//		private FontRegistry registry = new FontRegistry();
//		private final String systemFontSymbolicName = Display.getCurrent().getSystemFont().getFontData()[0].getName();
		private final Map<String, Image> iconMap = new HashMap<String, Image>();
		
		private final Styler ERROR_HIGHLIGHT_STYLER;
		
		ChannelTreeLabelProvider() {
			super();
			ColorRegistry colorRegistry = JFaceResources.getColorRegistry();
			String ERROR_HIGHLIGHT_STYLER_NAME = "custom_foreground_color";
			colorRegistry.put(ERROR_HIGHLIGHT_STYLER_NAME, new RGB(255, 0, 0));
			ERROR_HIGHLIGHT_STYLER = StyledString.createColorRegistryStyler(ERROR_HIGHLIGHT_STYLER_NAME, null);
		}
		
		@Override
		public void update(ViewerCell cell) {
			Object element = cell.getElement();
			Image image = null;
			StyledString text = new StyledString();

			if (element instanceof NotifyServiceData) {
				// a notify service node
				image = getImageFromFile("fldr_obj.gif");
				NotifyServiceData notifyServiceData = (NotifyServiceData) element;
				text.append(notifyServiceData.getName());
				if (!notifyServiceData.isReachable()) {
					text.append("  Unreachable!", ERROR_HIGHLIGHT_STYLER);
				}
			} 
			else if (element instanceof ChannelData) {
				// NC node
				ChannelData channelData = (ChannelData) element;
				// both 'Channel' and 'Class' start with a 'C'...
				if (channelData.isNewNc()) {
					image = getImageFromFile("newclass_wiz.gif");
				}
				else {
					image = getImageFromFile("class_obj.gif");
				}
				text.append(channelData.getName());
			}
			else if (element instanceof MCStatistics) {
				text.append(((MCStatistics)element).getStatistics());
				if (element instanceof ChannelConsumers) {
					// TODO find better icon
					image = getImageFromFile("import_brkpts.gif");
				}
				else if (element instanceof ChannelSuppliers) {
					// TODO find better icon
					image = getImageFromFile("export_brkpts.gif");
				}
			}
			else if (element instanceof ChannelParticipantName) {
				text.append(((ChannelParticipantName)element).getName());
			}
			else {
				text.append("unexpected tree node " + element.getClass(), ERROR_HIGHLIGHT_STYLER);
			}
			cell.setImage(image);
			cell.setText(text.toString());
			cell.setStyleRanges(text.getStyleRanges());
			super.update(cell);
		}

		/**
		 * Helper Method to load the images.
		 */
		private Image getImageFromFile(String file) {
			Image ret = iconMap.get(file);
			if (ret == null) {
				Bundle bundle = FrameworkUtil.getBundle(ChannelTreeLabelProvider.class);
				URL url = FileLocator.find(bundle, new Path("icons/" + file), null);
				ImageDescriptor image = ImageDescriptor.createFromURL(url);
				ret = image.createImage();
				iconMap.put(file, ret);
			}
			return ret;
		}

// Uncomment this to show new NCs in bold font. Probably the Class / new Class icons we have are better though.
//		@Override
//		public Font getFont(Object element) {
//			if (element instanceof ChannelData) {
//				ChannelData channelData = (ChannelData)element;
//				if (channelData.isNewNc()) {
//					return registry.getBold(systemFontSymbolicName);
//				}
//			}
//			return null;
//		}
	}

}
