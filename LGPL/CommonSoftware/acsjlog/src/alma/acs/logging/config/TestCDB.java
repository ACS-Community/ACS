package alma.acs.logging.config;

import com.cosylab.CDB.DALChangeListener;
import com.cosylab.CDB.DALOperations;
import com.cosylab.CDB.DAO;
import com.cosylab.CDB.RecordDoesNotExist;
import com.cosylab.CDB.XMLerror;

public class TestCDB implements DALOperations {

	public TestCDB() {
		super();
		// TODO Auto-generated constructor stub
	}

	public String get_DAO(String curl) throws XMLerror, RecordDoesNotExist {
		// TODO Auto-generated method stub
		return null;
	}

	public DAO get_DAO_Servant(String curl) throws XMLerror, RecordDoesNotExist {
		// TODO Auto-generated method stub
		return null;
	}

	public void shutdown() {
		// TODO Auto-generated method stub

	}

	public int add_change_listener(DALChangeListener listener) {
		// TODO Auto-generated method stub
		return 0;
	}

	public void listen_for_changes(String curl, int listenerID) {
		// TODO Auto-generated method stub

	}

	public void remove_change_listener(int listenerID) {
		// TODO Auto-generated method stub

	}

	public String list_nodes(String name) {
		// TODO Auto-generated method stub
		return null;
	}

}
