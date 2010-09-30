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
import alma.alarmsystem.alarmmessage.generated.AlarmSystemConfiguration;
import alma.alarmsystem.alarmmessage.generated.ConfigurationProperty;

import java.util.Hashtable;
import java.util.List;
import java.util.Set;

import cl.utfsm.acs.acg.dao.ACSAlarmSystemDAOImpl;

/**
 * Class representing the whole instance of the Alarm System configuration.
 * It includes the other entities managers of this package.
 * @see cl.utfsm.acs.acg.core
 * @author rtobar
 */
public class AlarmSystemManager implements EntityManager {

	/* Singleton instance */
	private static AlarmSystemManager _instance;
	
	/**
	 * In the CDB there can be several kinds of configuration-property
	 * types for the Alarm System. Currently only the "Implementation" 
	 * type is used to choose between the ACS Alarm System and the CERN 
	 * Alarm System, but more of them could be added. To represent this,
	 * an enum (ConfigurationPropertyType) was defined, which has each
	 * type available.
	 * For each of the values of this enum, a new enum is used to contain
	 * the different values this configuration-property can have. For the
	 * "Implementation" type the enum is AlarmSystemType.
	 * To store the values of the system a Hashtable is used where the 'key'
	 * is the ConfigurationPropertyType and the 'value' is an Object of
	 * the corresponding type. For example if the ConfigurationPropertyType
	 * was "Implementation" then the Object type would be AlarmSystemType.
	 * 
	 * Note that this schema could be also subsituted
	 * by a <String,String> Hashtable, since in a future there could be more
	 * configuration-property types for the Alarm System tuning.
	 */
	
	private UserAuthenticator.Role _role;
	private AcsInformation _acsInfo;
	private DAOManager _daoManager;
	private AlarmManager _alarmManager;
	private SourceManager _sourceManager;
	private CategoryManager _categoryManager;
	private ReductionManager _reductionManager;
	private ACSAlarmSystemDAOImpl _alarmSystemDAO;
	private Hashtable<String, String> _configurationProperty;

	private AlarmSystemManager(UserAuthenticator.Role role) {
		_role = role;
		_configurationProperty = new Hashtable<String,String>();
		_alarmSystemDAO = null;
		//_configurationProperty.put(ConfigurationPropertyType.Implementation, AlarmSystemType.ACS);
	}

	public static AlarmSystemManager getInstance(UserAuthenticator.Role role) {
		if( _instance == null ) {
			if( role == null ) {
				throw new IllegalArgumentException("Role passed is null");
			}
			_instance = new AlarmSystemManager(role);
		}
		return _instance;
	}

	public static AlarmSystemManager getInstance() {
		if( _instance == null ) {
			throw new IllegalStateException("The instance has not been initialized with an authorization");
		}
		return _instance;
	}
	/**
	 * Connects the application to the ACS Manager and initializes the DAO Manager
	 * for ACG.
	 * @throws Exception If the connection to the Manager is unsuccessful 
	 */
	public void connectToManager() throws Exception {
		_acsInfo = new AcsInformation("Alarm Configuration GUI");
		_daoManager = new DAOManager(_acsInfo.getContainerServices());
	}

	/**
	 * Gets a reference to the CDB DAL.
	 * @throws AcsJContainerServicesEx If there is an error while getting the
	 * DAL reference.
	 */
	public void connectToDAL() throws AcsJContainerServicesEx {
		if( _acsInfo == null || _daoManager == null )
			throw new IllegalStateException("The manager connection has not been initialized yet");
		_daoManager.connect();
	}

	public void loadFromCDB() {
		if( _acsInfo == null || _daoManager == null )
			throw new IllegalStateException("The manager connection has not been initialized yet");
		_configurationProperty.clear();
		if(_alarmSystemDAO == null)
			_alarmSystemDAO = _daoManager.getAlarmSystemDAO();
		/* Make sure that we have all the managers here... */
		getAlarmManager();
		getSourceManager();
		getCategoryManager();
		getReductionManager();

		/* Now let them load their stuff from the DAOs */
		List<ConfigurationProperty> cps = _alarmSystemDAO.loadConfigurations();
		for (ConfigurationProperty cp : cps) {
			_configurationProperty.put(cp.getName(), cp.getContent());
		}
		_alarmManager.loadFromCDB();
		_sourceManager.loadFromCDB();
		_categoryManager.loadFromCDB();
		_reductionManager.loadFromCDB();
	}
	
	public String checkCDB() {
		String error = "";
		error += _alarmManager.checkCDB();
		error += _categoryManager.checkCDB();
		error += _reductionManager.checkCDB();
		return error;
	}
	
	public void saveToCDB() {
		if( _acsInfo == null || _daoManager == null )
			throw new IllegalStateException("The manager connection has not been initialized yet");

		/* Make sure that we have all the managers here... */
		if(_alarmSystemDAO == null)
			_alarmSystemDAO = _daoManager.getAlarmSystemDAO();
		getAlarmManager();
		getSourceManager();
		getCategoryManager();
		getReductionManager();
		
		/* Backup the CDB */
		_daoManager.backupCDB();

		/* Now let them save their stuff through the DAOs */
		Set<String> keys = _configurationProperty.keySet();
		String[] cps = new String[keys.size()];
		keys.toArray(cps);
		AlarmSystemConfiguration asc = new AlarmSystemConfiguration();
		for (String sCP : cps) {
			ConfigurationProperty cp = new ConfigurationProperty();
			cp.setName(sCP);
			cp.setContent(_configurationProperty.get(sCP));
			asc.addConfigurationProperty(cp);
		}
		_alarmSystemDAO.flushConfiguration(asc);
		_alarmManager.saveToCDB();
		//_sourceManager.saveToCDB();
		_categoryManager.saveToCDB();
		_reductionManager.saveToCDB();
	}

	public void disconnectFromManager() {
		_acsInfo.disconnect();
		_acsInfo = null;
	}

	public AlarmManager getAlarmManager() {
		if( _alarmManager == null ) {
			_alarmManager = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		}
		return _alarmManager;
	}

	public SourceManager getSourceManager() {
		if( _sourceManager == null ) {
			_sourceManager = SourceManager.getInstance(_daoManager.getSourceDAO());
		}
		return _sourceManager;
	}
	public CategoryManager getCategoryManager() throws IllegalStateException {
		if( _categoryManager == null ) {
			_categoryManager = CategoryManager.getInstance(_daoManager.getCategoryDAO());
			getAlarmManager(); // Needs to be done to load the Alarms into the DAO
		}
		return _categoryManager;
	}

	public ReductionManager getReductionManager() {
		if( _reductionManager == null ) {
			_reductionManager = ReductionManager.getInstance(_daoManager.getAlarmDAO());
		}
		return _reductionManager;
	}

	public UserAuthenticator.Role getRole() {
		return _role;
	}

	/**
	 * This method modifies the current value of a given 
	 * configuration-property. As this 'value' is stored in a Hashtable
	 * it first checks if a previous 'value' existed (most likely) and 
	 * removes it (if it existed) to prevent more than one 'value' for 
	 * the same 'key'. 
	 * @param ct The configuration-property type that will be modified.
	 * @param at The new value for the given configuration-property type.
	 */
	public void setConfigurationProperty(String ct, String val){
		if(_configurationProperty.get(ct) != null)
			_configurationProperty.remove(ct);
		_configurationProperty.put(ct, val);
	}
	
	/**
	 * This method retrieves the current value of a given 
	 * configuration-property.
	 * @param ct The configuration-property type that will be queried.
	 * @return
	 */
	public String getConfigurationPrperty(String ct) {
		return _configurationProperty.get(ct);
	}
	
	/**
	 * Destroys the singleton instance of this class. This is needed to renew the internal references to
	 * the DAOs if a new connection to the DAL and the ACS Manager has been performed
	 */
	public static void destroy() {
		_instance = null;
	}
}
