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

import java.math.BigDecimal;

public class ComponentStatistics {
    public BigDecimal min;
    public BigDecimal max;
    public BigDecimal mean;
    public BigDecimal stdDev;

    public ComponentStatistics() {
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("min: ");
        builder.append(min);
        builder.append(" max: ");
        builder.append(max);
        builder.append(" mean: ");
        builder.append(mean);
        builder.append(" stdDev: ");
        builder.append(stdDev);
        builder.append("\n");
        return builder.toString();
    }
}
