/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2008 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.logging.archive.zoom;

import java.io.FileNotFoundException;

import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * The manager of the zoom feature.
 * <P>
 * A <code>ZoomManager</code> objects is the entity that perform the zooming
 * by delegating to other classes of the <code>zoom</code> package.
 * <P>
 * The min and max levels, if not passed in the constructor, are initialized
 * from java properties and if those properties does not exist from default values.
 * <P>
 * <B>Note</B>: only one loading operation is possible at the same time.
 * 
 * @author acaproni
 *
 */
public class ZoomManager {
	
	/**
	 * The name of the property of the min log level of the zoom 
	 */
	public static final String MIN_LEVEL_PROPERTY_NAME="jlog.archive.zoom.MinLevel";
	
	/**
	 * The name of the property of the min log level of the zoom 
	 */
	public static final String MAX_LEVEL_PROPERTY_NAME="jlog.archive.zoom.MaxLevel";
	
	/**
	 * The name of the property containing the folder where ARCHIVES writes
	 * files into.
	 * <P> 
	 * <I>Note</I>: this way of getting the folder could change in further releases.
	 */
	public static final String FILES_LOCATION_PROPERTY_NAME="jlog.archive.zoom.filesFolder";

	/**
	 * The files manager to get logs from a set of XML files
	 */
	private FilesManager filesManager=null;
	
	/**
	 * The minimum level of logs to read from files
	 */
	private LogTypeHelper minLevel=LogTypeHelper.values()[0];
	
	/**
	 * The maximum level of logs to read from files
	 */
	private LogTypeHelper maxLevel=LogTypeHelper.DEBUG;
	
	/**
	 * Signal if a loading is in progress
	 */
	private volatile boolean loadingLogs=false;
	
	/**
	 * Constructor.
	 * <P>
	 * The folder is retrieved from a java property and the levels are set to 
	 * defaults.
	 * 
	 * @see {@link FilesManager}
	 */
	public ZoomManager() {
		String folder = System.getProperty(FILES_LOCATION_PROPERTY_NAME);
		if (folder!=null) {
			try {
				filesManager = new FilesManager(folder);
			} catch (ZoomException e) {
				// It was not possible to instantiate the files manger.
				// The reason could be that the folder was not valid.
				//
				// This error is recoverable if the user set a new folder of XML files
				// so we can go ahead but the zoom will be not available.
				System.out.println("Error instantiating the FilesManager: "+e.getMessage());
				System.out.println("Zoom disabled at startup.");
				filesManager=null;
			}
		}
		String minLvl = System.getProperty(MIN_LEVEL_PROPERTY_NAME);
		if (minLvl!=null && !minLvl.isEmpty()) {
			LogTypeHelper lvl=LogTypeHelper.fromLogTypeDescription(minLvl);
			if (lvl!=null) {
				minLevel=lvl;
			} else {
				System.out.println("Property "+MIN_LEVEL_PROPERTY_NAME+" is wrong: default value used instead.");
			}
		} else {
			System.out.println("Default value used for "+MIN_LEVEL_PROPERTY_NAME+": "+minLevel);
		}
		String maxLvl = System.getProperty(MAX_LEVEL_PROPERTY_NAME);
		if (maxLvl!=null && !maxLvl.isEmpty()) {
			LogTypeHelper lvl=LogTypeHelper.fromLogTypeDescription(maxLvl);
			if (lvl!=null) {
				maxLevel=lvl;
			} else {
				System.out.println("Property "+MAX_LEVEL_PROPERTY_NAME+" is wrong: default value used instead.");
			}
		}else {
			System.out.println("Default value used for "+MAX_LEVEL_PROPERTY_NAME+": "+maxLevel);
		}
	}
	
	/**
	 * Constructor 
	 * 
	 * @param folder The folder with XML files of logs
	 * @param min The min log level of logs to read
	 * @param max The max log level of logs to read
	 * 
	 * @throws ZoomException If the folder or the levels are invalid
	 */
	public ZoomManager(String folder, LogTypeHelper min, LogTypeHelper max) throws ZoomException {
		filesManager=new FilesManager(folder);
		try {
			setLevels(min, max);
		} catch (Exception e) {
			throw new ZoomException("Error setting levels",e);
		}
	}
	
	/**
	 * Check if the zoom feature is available.
	 * <P>
	 * The zoom is available if the files manager and the levels are valid.
	 * In particular, the files manager must have a valid folder with a valid set 
	 * of XML files.
	 * The folder is valid if there are XML files to read.
	 *  
	 * @return <code>true</code> if the zoom is available; <code>false</code> otherwise.
	 */
	public boolean isAvailable() {
		if (filesManager==null || !filesManager.isOperational()) {
			return false;
		}
		if (minLevel==null && maxLevel!=null) {
			return true;
		}
		if (minLevel!=null && maxLevel==null) {
			return false;
		}
		return (minLevel.ordinal()<=maxLevel.ordinal());
	}
	
	/**
	 * Set the folder to read XML files of logs from.
	 * 
	 * @param folder The folder of XML files of logs
	 * 
	 * @throws ZoomException If the folder is invalid.
	 */
	public void setFilesRepository(String folder) throws ZoomException {
		if (folder==null || folder.isEmpty()) {
			throw new IllegalArgumentException("The folder can't be null nor empty");
		}
		filesManager= new FilesManager(folder);
	}
	
	/**
	 * Set the levels of the logs to read while zooming
	 * 
	 * @param min The minimum log level (can be <code>null</code>)
	 * @param max The max log level (must be greater then the <code>min</code>;
	 * 				can't be <code>null</code>)
	 */
	public void setLevels(LogTypeHelper min, LogTypeHelper max) {
		if (max==null) {
			throw new IllegalArgumentException("The max level can't be null");
		}
		if (min!=null && (min.ordinal()>max.ordinal())) {
			throw new IllegalArgumentException("Invalid levels ["+min+", "+max+"]");
		}
		minLevel=min;
		maxLevel=max;
	}
	
	/**
	 * Return the path of the folder containing XML log files.
	 * <P>
	 * If the folder is invalid
	 * @return The repository of XML files of logs;
	 * 			<code>null</code> if no folder (or an invalid one) is in use
	 * 
	 */
	public String getRepository() {
		if (filesManager==null) {
			return null;
		}
		return filesManager.filesFolder;
	}

	/**
	 * 
	 * @return The min level for zoomin
	 */
	public LogTypeHelper getMinLevel() {
		return minLevel;
	}

	/**
	 * 
	 * @return The max level for zoomin
	 */
	public LogTypeHelper getMaxLevel() {
		return maxLevel;
	}
	
	/**
	 * Load the logs.
	 * 
	 * @see {@link FilesManager}
	 */
	public void zoom(String startDate, 
			String endDate, 
			ACSRemoteLogListener logListener,
			ZoomProgressListener zoomListener,
			ACSRemoteErrorListener errorListener) throws FileNotFoundException, ZoomException {
		if (!isAvailable()) {
			throw new ZoomException("Zoom not available");
		}
		if (loadingLogs) {
			throw new ZoomException("A zoom is already in progress");
		}
		loadingLogs=true;
		filesManager.getLogs(startDate, endDate, logListener, minLevel, maxLevel, zoomListener, errorListener);
		loadingLogs=false;
	}

	/**
	 * Return <code>true</code> if a loading is in progress.
	 * 
	 * @return <code>true</code> if a loading is in progress
	 */
	public boolean isLoadingLogs() {
		return loadingLogs;
	}
	
	/**
	 * Interrupt the zoom.
	 * <P>
	 * The method does nothing if no zoom is currently in progress.
	 */
	public void stopZoom() {
		if (filesManager!=null) {
			filesManager.stopLoading();
		}
	}
}
