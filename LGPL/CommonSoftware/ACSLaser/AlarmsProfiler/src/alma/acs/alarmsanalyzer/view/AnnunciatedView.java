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
package alma.acs.alarmsanalyzer.view;

import org.eclipse.swt.widgets.Composite;

import alma.acs.alarmsanalyzer.document.AnnunciatedContainer;

public class AnnunciatedView extends SupAnnCommonView {
	
	/**
	 * The ID of this view
	 */
	public static final String ID = "alma.acs.alarmsanalyzer.annunciated";

	public AnnunciatedView() {}
	
	@Override
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		AnnunciatedContainer.getInstance().setTableViewer(tableViewer);
		tableViewer.setInput(AnnunciatedContainer.getInstance());
	}
}
