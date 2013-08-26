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
/**
 * Insert the type's description here.
 * Creation date: (11/12/00 6:23:25 PM)
 * @author: Administrator
 */
public class Legend extends Dart {
	private static int xArrow[] = {0,-6,10,-6};
	private static int yArrow[] = {0,-4,0,4};
	private static int xArrow1[] = {0,6,-10,6};
	
	private Polygon arrow1 = new Polygon(xArrow, yArrow, xArrow.length);
	private Polygon arrow2 = new Polygon(yArrow, xArrow1, xArrow.length);
/**
 * Legend constructor comment.
 */
public Legend() {
	super();
	setPosition(-10, 45);
}
protected void drawDart(Graphics g) {
	g.setColor(Color.black);

	// needed only for this Dart, the Legend
	Font f = g.getFont();
	g.setFont(new Font("SansSerif", Font.BOLD, 12));

	g.drawLine(0,0,20,0);
	g.drawLine(0,0,0,-20);
	
	g.translate(20,0);
	g.fillPolygon(arrow1);
	g.translate(-20,-20);
	g.fillPolygon(arrow2);
	g.translate(0,20);
		
	g.drawString("N", 6, -20);
	g.drawString("E", 23, -4);
	
	g.setFont(f);
}

public void setError(boolean err) {}
}
