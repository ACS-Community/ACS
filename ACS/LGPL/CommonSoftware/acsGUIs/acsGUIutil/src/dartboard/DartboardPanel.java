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

import java.awt.*;
import javax.swing.JPanel;

/**
 * Insert the type's description here.
 * Creation date: (11/7/00 10:57:43 PM)
 * @author: 
 */
public class DartboardPanel extends JPanel {
	private static final int GRID = 0;
	private static final int LEGEND = 1;
	private static final int SUN = 2;
	private static final int MOON = 3;
	private static final int TELESCOPE_DESTINATION = 4;
	private static final int TELESCOPE = 5;
	private static final int WIND = 6;
	
	private Dartboard dartboard;
	private Dart[] darts;
//	private PlanetPanel planetPanel;
	
	public void setSettable(boolean settable) {
	}

/**
 * DartboardPanel constructor comment.
 */
public DartboardPanel() {
	super();
	initialize();
}

private void initialize() {
	setLayout(new BorderLayout());
	dartboard = new Dartboard();
//	planetPanel = new PlanetPanel();
	add(dartboard, "Center");
//	add(planetPanel, "South");

	darts = new Dart[7];
	darts[GRID] = new Grid();
	darts[LEGEND] = new Legend();
	darts[SUN] = new Sun();
	darts[MOON] = new Moon();
	darts[TELESCOPE_DESTINATION] = new TelescopeDestination();
	darts[TELESCOPE] = new Telescope();
	darts[WIND] = new Wind();
	
	dartboard.setDarts(darts);
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setMoonAzimuth(double azimuth) {
	darts[MOON].setAzimuth(azimuth); 
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setMoonDeclination(double declination) {
//	planetPanel.getMoonDeclinationLabel().setText(Double.toString(declination));
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setMoonDeclination(String declination) {
//	planetPanel.getMoonDeclinationLabel().setText(declination);
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setMoonElevation(double elevation) {
	darts[MOON].setElevation(elevation);
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setMoonPhase(int phase) {
	((Moon)darts[MOON]).setPhase((short)phase);
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setMoonRightAscension(double rightAscension) {
//	planetPanel.getMoonRightAscensionLabel().setText(Double.toString(rightAscension));
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setMoonRightAscension(String rightAscension) {
//	planetPanel.getMoonRightAscensionLabel().setText(rightAscension);
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setSunAzimuth(double azimuth) {
	darts[SUN].setAzimuth(azimuth); 
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setSunDeclination(double declination) {
//	planetPanel.getSunDeclinationLabel().setText(Double.toString(declination));
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setSunDeclination(String declination) {
//	planetPanel.getSunDeclinationLabel().setText(declination);
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setSunElevation(double elevation) {
	darts[SUN].setElevation(elevation);
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setSunRightAscension(double rightAscension) {
//	planetPanel.getSunRightAscensionLabel().setText(Double.toString(rightAscension));
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setSunRightAscension(String rightAscension) {
//	planetPanel.getSunRightAscensionLabel().setText(rightAscension);
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setTelescopeAzimuth(double azimuth) {
	darts[TELESCOPE].setAzimuth(azimuth);
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setTelescopeDestinationAzimuth(double azimuth) {
	darts[TELESCOPE_DESTINATION].setAzimuth(azimuth);
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setTelescopeDestinationElevation(double elevation) {
	darts[TELESCOPE_DESTINATION].setElevation(elevation);
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setTelescopeElevation(double elevation) {
	darts[TELESCOPE].setElevation(elevation);
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setWindDirection(double direction) {
	((Wind)darts[WIND]).setDirection(direction);
}

// This method should be protected. But how to call it from 
// an inner class then?
public void setWindSpeed(double speed) {
	((Wind)darts[WIND]).setSpeed(speed);
}

	/**
	 * Set the dartboard in error state.
	 * Normally the dartboard is not in error state.
	 * If it enters the error state, it delegates each widget to show
	 * him self accordingly (for example the grid is show in red)
	 *  
	 * @param error if true the dartboard enter the error state
	 *              otherwise it resumes its standard functionality
	 */
	public void setError(boolean error) {
		darts[GRID].setError(error);
		darts[LEGEND].setError(error);
		darts[MOON].setError(error);
		darts[SUN].setError(error);
		darts[TELESCOPE].setError(error);
		darts[TELESCOPE_DESTINATION].setError(error);
		// Force a refresh 
		this.repaint();
	}


}
