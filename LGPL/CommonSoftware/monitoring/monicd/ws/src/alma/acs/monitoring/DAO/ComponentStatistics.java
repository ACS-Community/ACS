/*
 * ALMA - Atacama Large Millimiter Array
 * Copyright (c) European Southern Observatory, 2011 
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */
package alma.acs.monitoring.DAO;


/**
 * Holds and prints the summary statistics of one numerical blob, which is the series of values from one monitor point
 * over the collector time interval.
 */
public class ComponentStatistics
{
	public Number min;
	public Number max;
	public Number mean;
	public Number stdDev;

	public ComponentStatistics() {
		// NumberFormat format = DecimalFormat.getInstance();
		// format.setRoundingMode(RoundingMode.HALF_UP);
		// format.setMinimumFractionDigits(0);
		// format.setMaximumFractionDigits(2);
	}

	/**
	 * Todo: use formatting mask?
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("min: ");
		builder.append(min.toString());
		builder.append(" max: ");
		builder.append(max.toString());
		builder.append(" mean: ");
		builder.append(mean.toString());
		builder.append(" stdDev: ");
		builder.append(stdDev.toString());
		builder.append("\n");
		return builder.toString();
	}
}
