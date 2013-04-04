/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2013
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

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.inject.Inject;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.e4.core.contexts.IEclipseContext;
import org.eclipse.e4.core.di.annotations.Optional;
import org.eclipse.e4.core.services.events.IEventBroker;
import org.eclipse.e4.core.services.statusreporter.StatusReporter;
import org.eclipse.e4.ui.di.Focus;
import org.eclipse.e4.ui.di.UIEventTopic;
import org.eclipse.e4.ui.model.application.ui.menu.MPopupMenu;
import org.eclipse.e4.ui.workbench.modeling.ESelectionService;
import org.eclipse.e4.ui.workbench.swt.modeling.EMenuService;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

import alma.acs.eventbrowser.model.EventModel;
import alma.acs.eventbrowser.model.MCStatistics;
import alma.acs.eventbrowser.handlers.NotifyServiceUpdateJob;
import alma.acs.eventbrowser.parts.ChannelTreeProviders.ChannelTreeContentProvider;
import alma.acs.eventbrowser.parts.ChannelTreeProviders.ChannelTreeLabelProvider;
import alma.acs.eventbrowser.status.StatusLineWriter;


public class ChannelTreePart {

	private TreeViewer viewer;

	@Inject
	private ESelectionService selectionService;
	
	/**
	 * Blocking (popup) status report.
	 */
	@Inject 
	private StatusReporter statusReporter;
	
	private StatusLineWriter statusLineWriter;
	
	private EventModel eventModel;

	private long howOften = 10000l; // Default is every 10 seconds
	
	private class NameComparator extends ViewerComparator {
		@Override
		public int compare(Viewer viewer, Object e1, Object e2) {
			if (e1 instanceof MCStatistics && e2 instanceof MCStatistics)
				return 0; // leave the statistics in the order I added them in ChannelData!!
			return super.compare(viewer, e1, e2);
		}
	}


	/**
	 * The constructor.
	 */
	public ChannelTreePart() {
	}
	
	/**
	 * See http://www.vogella.com/articles/EclipseJFaceTree/article.html
	 */
	@PostConstruct
	public void postConstruct(Composite parent, final IEclipseContext context, EMenuService menuService, IEventBroker eventBroker) {
		try {
			eventModel = EventModel.getInstance();
		} catch (Throwable thr) {
			thr.printStackTrace();
			IStatus someStatus = statusReporter.newStatus(IStatus.ERROR, "Connection with NCs failed.", thr);
			statusReporter.report(someStatus, StatusReporter.SHOW);
			throw new RuntimeException(thr);
		}
//		eventModel.getLogger().info("ChannelTreePart got EventModel instance.");

		statusLineWriter = new StatusLineWriter(eventBroker);
		
		viewer = new TreeViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		viewer.setContentProvider(new ChannelTreeContentProvider(eventModel));
		viewer.setLabelProvider(new ChannelTreeLabelProvider());
		// Expand the tree. '2' means to show only the visible top-level nodes.
		viewer.setAutoExpandLevel(2);

		viewer.setComparator(new NameComparator());

		// Provide the input to the ContentProvider
		viewer.setInput(eventModel.getNotifyServiceTotals());

		// TODO: Expand with doubleclick as part of the eventGUI improvements
//		viewer.addDoubleClickListener(new IDoubleClickListener() {
//			@Override
//			public void doubleClick(DoubleClickEvent event) {
//				IStructuredSelection thisSelection = (IStructuredSelection) event.getSelection();
//				Object selectedNode = thisSelection.getFirstElement();
//				viewer.setExpandedState(selectedNode, !viewer.getExpandedState(selectedNode));
//				System.out.println("double click in ChannelTreePart#viewer");
//			}
//		});
		
		// Attach a selection listener to our tree that will post selections to the ESelectionService
		viewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection selection = (IStructuredSelection) event.getSelection();
				selectionService.setSelection(selection.size() == 1 ? selection.getFirstElement() : selection.toArray());
			}
		});
		
		// TODO: Take care of selections and help system. Here's the E3 code:
//		getSite().setSelectionProvider(viewer);
//		// Create the help context id for the viewer's control
//		PlatformUI.getWorkbench().getHelpSystem().setHelp(viewer.getControl(), "alma.acs.eventbrowser.viewer");

		hookContextMenu(menuService);

		
		// TODO: this could be used by handlers etc, currently it's not
		context.set(ChannelTreePart.class, this);

	}
	
	
	private void hookContextMenu(EMenuService menuService) {
		// For the case of popup (mouse) menues we must reference the menu ID from the file Application.e4xmi
		MPopupMenu menu = menuService.registerContextMenu(viewer.getTree(), "alma.acs.eventgui.popupmenu.channeltree");
		if (menu == null) {
			System.out.println("Damn, failed to register popup menu for the ChannelTreePart tree.");
		}
	}
	
	
	@PreDestroy
	public void preDestroy() {
		//TODO Your code here
	}
	
	
	@Focus
	public void setFocus() {
		viewer.getTree().setFocus();
	}

	@Inject
	@Optional
	private void refreshNotify(@UIEventTopic(NotifyServiceUpdateJob.REFRESH_UI_SIGNAL_NAME) String s) {
//		System.out.println("ChannelTreePart#refreshNotify() called via IEventBroker");
		viewer.refresh();
//		viewer.expandAll();
	}

}