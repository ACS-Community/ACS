/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
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
package cl.utfsm.acs.acg.core;

public class ObjectState {
	boolean _new;
	boolean _create;
	boolean _update;
	boolean _delete;
	public ObjectState(boolean newObj){
		_create = false;
		_update = false;
		_delete = false;
		_new = newObj;
	}
	public void create(){
		_create = true;
		_update = false;
		_delete = false;
	}
	public void update(){
		_create = false;
		_update = true;
		_delete = false;
	}
	public void delete(){
		_create = false;
		_update = false;
		_delete = true;
	}
	
	/*
	 * Actions: 0 --> Do nothing
	 * 			1 --> Create entry
	 * 			2 --> Update entry
	 * 			3 --> Delete entry
	 */
	public int getAction(){
		if(_new) {
			if(_create || _update)
				return 1;
			if(_delete)
				return 0;
		}
		else {
			if(_create || _update)
				return 2;
			if(_delete)
				return 3;
		}
		return -1;
	}
}