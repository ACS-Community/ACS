/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarmsystemprofiler.parts;

import javax.annotation.PostConstruct;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;

import alma.acs.alarmsystemprofiler.document.LostSourcesContainer;

/**
 * The view with lost alarms
 * 
 * @author acaproni
 *
 */
public class LostSourcesPart extends ViewPart {

	/**
	 * The list showing the Ids
	 */
	private ListViewer listViewer;
	
	/**
	 * The ID of this view
	 */
	public static final String ID = "alma.acs.alarmsanalyzer.lost";

	public LostSourcesPart() {
	}

	public void createPartControl(Composite parent) {
		listViewer = new ListViewer(parent,SWT.BORDER| SWT.H_SCROLL| SWT.V_SCROLL);
		listViewer.setContentProvider(LostSourcesContainer.getInstance());
		listViewer.setSorter(new ViewerSorter());
		listViewer.setInput(null);
	}

	@Override
	public void setFocus() {
		listViewer.getControl().setFocus();
	}

	@PostConstruct
	public void postConstruct(Composite parent) {
		createPartControl(parent);
		LostSourcesContainer.getInstance().setViewer(listViewer);
		listViewer.setInput(LostSourcesContainer.getInstance());
	}
	
}
