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

import java.util.Date;

import org.eclipse.ui.part.ViewPart;

/**
 * The base class for the views showing a chart.
 * 
 * @author acaproni
 *
 */
public abstract class ChartViewBase extends ViewPart {

	/**
	 * Refresh the content of the graph by setting the passed series of doubles
	 * 
	 * @param values The values to show in the Y axes
	 * @param times The times to show in the X axes
	 */
	public abstract void refreshChart(final double[] values, final Date[] times);
}
