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

import java.util.Iterator;
import java.util.List;

import alma.acs.alarmsystem.generated.FaultFamily;

import cern.laser.business.dao.SourceDAO;
import cern.laser.business.data.Source;

/**
 * The SourceManager class is responsible of handling all the references
 * 
 * @author rtobar
 */
public class SourceManager implements EntityManager {

	/**
	 * The singleton instance shared across the project
	 */
	private static SourceManager _instance;

	private SourceDAO _sourceDAO;
	private Source[] _sources;

	private SourceManager(SourceDAO sourceDAO) {
		_sourceDAO = sourceDAO;
		_sources = new Source[0];
	}

	public static SourceManager getInstance(SourceDAO sourceDAO) {
		if( _instance == null ) {
			_instance = new SourceManager(sourceDAO);
		}
		return _instance;
	}

	public void loadFromCDB() {
		_sources = _sourceDAO.findAllSources();
	}

	public Source[] getAllSources() {
		return _sources;
	}

	public Source getSource(String name) {
		for (int i = 0; i < _sources.length; i++) {
			if( _sources[i].getName().compareTo(name) == 0 )
				return _sources[i];
		}
		return null;
	}

	/**
	 * Destroys the singleton instance of this class. This is needed to renew the internal reference to
	 * the SourceDAO if a new connection to the DAL and the ACS Manager has been performed
	 */
	public static void destroy() {
		_instance = null;
	}

	/**
	 * This method allow to add a new source to the source List
	 * @param source The source to be added
	 * @throws IllegalOperationException If the source already exists
	 */
	public void addSource(Source source) throws IllegalOperationException {

		// First we check if the source already exists
		for (int i = 0; i < _sources.length; i++) {
			if( _sources[i].getName().compareTo(source.getName()) == 0 )
				throw new IllegalOperationException("The source " + source.getName() + " already exists");
		}

		Source[] newSources = new Source[_sources.length+1];
		System.arraycopy(_sources, 0, newSources, 0, _sources.length);
		newSources[_sources.length] = source;
		_sources = newSources;
		
	}

	/**
	 * This method allows to delete a <code>Source</code> from the configuration of the Alarm System
	 * 
	 * @param source The source to be deleted
	 * @return true if the source was deleted correctly,
	 * 		   false otherwise
	 * @throws IllegalOperationException when there are not sources to be deleted
	 */
	public boolean deleteSource(Source source) throws IllegalOperationException {

		AlarmManager am = AlarmSystemManager.getInstance().getAlarmManager();
		List<FaultFamily> allAlarms = am.getAllAlarms();
		for (Iterator<FaultFamily> iterator = allAlarms.iterator(); iterator.hasNext();) {
			FaultFamily faultFamily = (FaultFamily) iterator.next();
			if (faultFamily.getAlarmSource().compareTo(source.getName()) == 0) {
				throw new IllegalOperationException("Cannot delete source: sources in use by the FaultFamily "+faultFamily.getName());
				
			}
		}
		
		if (_sources.length == 0)
			return false;

		Source[] newSources = new Source[_sources.length - 1];

		for (int i = 0; i < _sources.length; i++) {
			if (_sources[i].getName().compareTo(source.getName()) == 0){
				for(i++; i< _sources.length; i++)
					newSources[i-1] = _sources[i];
				_sources = newSources;
				return true;
			}
			else
				if( i+1 != _sources.length )
					newSources[i] = _sources[i];
		}
		return false;
	}
	
	public void saveToCDB(){
		for (int i = 0; i < _sources.length; i++) {
			_sourceDAO.updateSource(_sources[i]);
		}
	}
}