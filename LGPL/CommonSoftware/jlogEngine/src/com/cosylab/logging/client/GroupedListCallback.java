/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package com.cosylab.logging.client;

/**
 * Implemented in LogTableDataModel.
 * Creation date: (11/30/2001 22:09:49)
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public interface GroupedListCallback {
	public static final int GL_INSERT = 0;
	public static final int GL_DELETE = 1;
	public static final int GL_EXPAND = 2;
	public static final int GL_COLLAPSE = 3;
	public static final int GL_UPDATED = 4;
/**
 * Insert the method's description here.
 * Creation date: (11/30/2001 22:10:42)
 * @param changeType int
 * @param param1 int
 * @param param2 int
 */
void groupedListChanged(int changeType, int param1, int param2);
}
