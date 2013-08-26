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
package cl.utfsm.acs.acg.core;


import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServices;
import alma.cdbErrType.CDBExceptionEx;
import alma.cdbErrType.CDBRecordAlreadyExistsEx;
import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.cdbErrType.CDBXMLErrorEx;

import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.dao.CategoryDAO;
import cern.laser.business.dao.SourceDAO;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.WDAL;
import com.cosylab.CDB.WDALHelper;
//import com.cosylab.acs.laser.dao.ACSAlarmDAOImpl;
//import com.cosylab.acs.laser.dao.ACSCategoryDAOImpl;
//import com.cosylab.acs.laser.dao.ACSSourceDAOImpl;
//import com.cosylab.acs.laser.dao.ConfigurationAccessor;
//import com.cosylab.acs.laser.dao.ConfigurationAccessorFactory;

import cl.utfsm.acs.acg.dao.ACSAlarmDAOImpl;
import cl.utfsm.acs.acg.dao.ACSAlarmSystemDAOImpl;
import cl.utfsm.acs.acg.dao.ACSCategoryDAOImpl;
import cl.utfsm.acs.acg.dao.ACSSourceDAOImpl;
import cl.utfsm.acs.acg.dao.ConfigurationAccessor;
import cl.utfsm.acs.acg.dao.ConfigurationAccessorFactory;

/**
 * This class deals with the complexity of the usage of the DAOs
 * implemented in the {@link com.cosylab.acs.laser.dao} package.
 * It handles an instance for each of the DAOImpl classes of the Alarm System.
 * The other classes should retrieve the DAOImpl classes through this class.
 * 
 * @author rtobar
 */
public class DAOManager {
	private ACSAlarmSystemDAOImpl _alarmSystemDAOImpl;
	private AlarmDAO _alarmDAOImpl;
	private SourceDAO _sourceDAOImpl;
	private CategoryDAO _categoryDAOImpl;
	private ContainerServices _contServ;
	private ConfigurationAccessor _conf;

	public DAOManager(ContainerServices contServ) {
		_contServ = contServ;
	}

	public void connect() throws AcsJContainerServicesEx {
		//_conf = ConfigurationAccessorFactory.getInstance(_contServ);
		_conf = ConfigurationAccessorFactory.getInstance(WDALHelper.narrow(_contServ.getCDB()));
	}
	
	public ACSAlarmSystemDAOImpl getAlarmSystemDAO() {
		if( _conf == null ) {
			throw new IllegalStateException("DAOManager not connected to DAL");
		}
		if( _alarmSystemDAOImpl == null ) {
			_alarmSystemDAOImpl = new ACSAlarmSystemDAOImpl(_contServ.getLogger());
			((ACSAlarmSystemDAOImpl)_alarmSystemDAOImpl).setConfAccessor(_conf);
		}
		return _alarmSystemDAOImpl;
	}

	public AlarmDAO getAlarmDAO() {
		if( _conf == null ) {
			throw new IllegalStateException("DAOManager not connected to DAL");
		}
		if( _alarmDAOImpl == null ) {
			_alarmDAOImpl = new ACSAlarmDAOImpl(_contServ.getLogger());
			((ACSAlarmDAOImpl)_alarmDAOImpl).setConfAccessor(_conf);
		}
		return _alarmDAOImpl;
	}

	public SourceDAO getSourceDAO() {
		if( _conf == null ) {
			throw new IllegalStateException("DAOManager not connected to DAL");
		}
		if( _sourceDAOImpl == null ) {
			if( _alarmDAOImpl == null )
				_alarmDAOImpl = getAlarmDAO();
			_sourceDAOImpl = new ACSSourceDAOImpl(_contServ.getLogger(), ((ACSAlarmDAOImpl)_alarmDAOImpl).getSources());
			((ACSSourceDAOImpl)_sourceDAOImpl).setConfAccessor(_conf);
		}
		return _sourceDAOImpl;
	}

	public CategoryDAO getCategoryDAO() throws IllegalStateException {
		if( _conf == null ) {
			throw new IllegalStateException("DAOManager not connected to DAL");
		}
		if( _categoryDAOImpl == null ) {
			if( _alarmDAOImpl == null )
				_alarmDAOImpl = getAlarmDAO();
			_categoryDAOImpl = new ACSCategoryDAOImpl(_contServ.getLogger(), (ACSAlarmDAOImpl)_alarmDAOImpl);
			((ACSCategoryDAOImpl)_categoryDAOImpl).setConfAccessor(_conf);
		}
		return _categoryDAOImpl;
	}

	public void backupCDB(){
		DAL dal = null;
		WDAL wdal = null;
		try{
			dal = _contServ.getCDB();
		}catch(AcsJContainerServicesEx e){
			return;
		}
		wdal = WDALHelper.narrow(dal);
		if(wdal == null)
			return;
		String src = "Alarms";
		String dst;
		int i = 1;
		do{
			dst = src + ".bkp." + i;
			i++;
		} while (nodeExists(wdal,"",dst));
		copyNode(wdal,src,dst);
	}
	
	private boolean nodeExists(DAL dal, String path, String dst){
		String[] nodes = dal.list_nodes(path).split(" ");
		for (int i = 0; i < nodes.length; i++) {
			if(nodes[i].compareTo(dst) == 0)
				return true;
		}
		return false;
	}
	
	private void copyNode(WDAL wdal, String src, String dst){
		String[] nodes = wdal.list_nodes(src).split(" ");
		String[] daos = wdal.list_daos(src).split(" ");
		String xml = null;
		if(daos.length > 0 && daos[0].compareTo("")!=0){
			try {
				xml = wdal.get_DAO(src);
				wdal.add_node(dst, xml);
			} catch (CDBXMLErrorEx e) {
				System.out.println("XML Parsing Error");
			} catch (CDBRecordDoesNotExistEx e) {
				System.out.println("The Record" + src + " doesn't exist");
			}catch (CDBRecordAlreadyExistsEx e) {
				System.out.println("The Record" + dst + " already exists");
			}catch (CDBExceptionEx e){
				System.out.println("Another CDB Error");
			}
			for (int i = 0; i < nodes.length; i++) {
				copyNode(wdal, src+"/"+nodes[i],dst+"/"+nodes[i]);
			}
		}
	}
}
