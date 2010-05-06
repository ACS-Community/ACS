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