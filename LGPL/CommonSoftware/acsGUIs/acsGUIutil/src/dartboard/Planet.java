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
package dartboard;

/**
 * Insert the type's description here.
 * Creation date: (11/7/00 9:02:16 PM)
 * @author: 
 */
public abstract class Planet extends Dart {
	private double rightAscension;
	private double declination;
/**
 * Planet constructor comment.
 * @param x int
 * @param y int
 */
public Planet() {
	super();
}
/**
 * Insert the method's description here.
 * Creation date: (11/9/00 5:35:00 PM)
 * @return double
 */
public double getDeclination() {
	return declination;
}
/**
 * Insert the method's description here.
 * Creation date: (11/9/00 5:34:35 PM)
 * @return double
 */
public double getRightAscension() {
	return rightAscension;
}
/**
 * Insert the method's description here.
 * Creation date: (11/9/00 5:35:00 PM)
 * @param newDeclination double
 */
public void setDeclination(double newDeclination) {
	declination = newDeclination;
}
/**
 * Insert the method's description here.
 * Creation date: (11/9/00 5:34:35 PM)
 * @param newRightAscension double
 */
public void setRightAscension(double newRightAscension) {
	rightAscension = newRightAscension;
}
}
