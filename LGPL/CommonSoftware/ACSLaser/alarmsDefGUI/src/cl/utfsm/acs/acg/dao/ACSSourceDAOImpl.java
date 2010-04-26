package cl.utfsm.acs.acg.dao;

import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import cern.laser.business.data.Source;

public class ACSSourceDAOImpl extends com.cosylab.acs.laser.dao.ACSSourceDAOImpl {
	public ACSSourceDAOImpl(Logger log, ConcurrentHashMap<String, Source> sources) {
		super(log, sources);
	}
}
