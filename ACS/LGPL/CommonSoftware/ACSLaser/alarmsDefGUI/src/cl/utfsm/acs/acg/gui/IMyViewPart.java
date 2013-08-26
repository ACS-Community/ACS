/*
 *    ALMA - Atacama Large Millimeter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2009
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package cl.utfsm.acs.acg.gui;

/**
 * Interface to be implemented by all Views used by ACG.
 * @author rtobar
 */
public interface IMyViewPart {

	/**
	 * Enables or disables the View for user interaction.
	 * @param v If true, the view should be enabled. Else, it should be disabled
	 */
	public void setEnabled(boolean v);

	/**
	 * Refreshes the contents of the views. This method is intended to be used
	 * by external classes when reloading the CDB.
	 */
	public void refreshContents();
	
	public void setReadOnly(boolean v);
	
	public void fillWidgets();
}
