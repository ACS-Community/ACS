/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.lasercore.test.stress;

import java.util.HashSet;
import java.util.Vector;

import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.alarmsystem.CERNAlarmService;
import alma.alarmsystem.Category;
import alma.alarmsystem.corbaservice.CernAlarmServiceUtils;
import alma.acs.lasercore.test.stress.category.AlarmView;
import alma.acs.lasercore.test.stress.category.CategoryListener;
import alma.acs.lasercore.test.stress.category.CategorySubscriber;

/**
 * A client that listen alarms from all the categories
 * 
 * @author acaproni
 *
 */
public class CategoryClient {
	
	// Container services
	private ContainerServices contSvc;
	
	// The category root topic
	private String categoryRootTopic;
	
	/**
	 * The categories 
	 * Each category is a notification channel we have to listen to.
	 * The list of the categories is read from the AlarmServise component
	 */
	private Category[] categories;
	
	// The consumers to listen to alarms from the categories
	private CategorySubscriber[] consumers;
	
	/**
	 * The alarm service 
	 */
	private CERNAlarmService alarm;
	
	private HashSet<CategoryListener> listeners = new HashSet<CategoryListener>();
	
	/**
	 * Constructor 
	 * 
	 * @param svc ACS ContainerServices used to access the alarm service component
	 */
	public CategoryClient(ContainerServices svc) throws Exception {
		if (svc==null) {
			throw new IllegalArgumentException("Invalid ContainerServices");
		}
		contSvc=svc;
		try {
			initialize();
		} catch (Throwable t) {
			throw new Exception("Error building CategoryClient",t);
		}
	}
	
	/**
	 * Connect the AlarmSrevice component
	 */
	private void getAlarmService() throws Exception {
		CernAlarmServiceUtils alarmtUtils= new CernAlarmServiceUtils(contSvc);
		alarm=alarmtUtils.getCernAlarmService();
	}
	
	/**
	 * Read the categories and the category root topic from the component
	 * 
	 * @return The categories
	 */
	private void getCategories() throws Exception {
		if (alarm==null) {
			throw new IllegalStateException("No alarm component connected");
		}
		categoryRootTopic=alarm.getCategoryRootTopic();
		categories=alarm.getCategories();
	}
	
	/**
	 * Create the consumers for the passed categories
	 * 
	 *   
	 * @param categoriesToConnect The categories to connect to
	 */
	public void connect(Category[] categoriesToConnect) throws Exception {
		if (categoriesToConnect==null ||categoriesToConnect.length==0) {
			contSvc.getLogger().log(AcsLogLevel.INFO,"No categories to connect to");
			return;
		}
		consumers=new CategorySubscriber[categoriesToConnect.length];
		int t=0;
		Vector<String> failingConnections = new Vector<String>();
		for (Category category: categoriesToConnect) {
			try {
				consumers[t++]=new CategorySubscriber(contSvc,categoryRootTopic,category.path,this);
				contSvc.getLogger().log(AcsLogLevel.DEBUG,"Connected to "+categoryRootTopic+"."+category.path);
			} catch (Throwable throwable) {
				failingConnections.add("Error subscribing to "+categoryRootTopic+"."+category.path+": "+throwable.getMessage());
			}
		}
		if (failingConnections.size()>0) {
			System.err.println("Error connecting categories: ");
			for (String str: failingConnections) {
				System.err.println("\t"+str);
			}
			throw new Exception("Error connecting categories");
		}
	}
	
	/**
	 * Connect to all available categories
	 * 
	 */
	public void connect() throws Exception {
		connect(categories);
	}
	
	/**
	 * Disconnect all the categories
	 */
	public void disconnect() {
		for (CategorySubscriber cat: consumers) {
			cat.close();
		}
		alarm=null;
	}
	
	/**
	 * Dumps the category
	 *
	 */
	private void dumpCategories() {
		if (categories==null) {
			contSvc.getLogger().log(AcsLogLevel.DEBUG,"Categories null");
			return;
		}
		if (categories.length==0) {
			contSvc.getLogger().log(AcsLogLevel.DEBUG, "Categories empty");
			return;
		}
		System.out.println("Category root topic="+categoryRootTopic);
		for (Category cat: categories) {
			contSvc.getLogger().log(AcsLogLevel.DEBUG, "Category name="+cat.name+"\tpath="+cat.path+"\tdescription="+cat.description);
		}
	}
	
	/**
	 * Initialize the client.
	 * It connects to the alarm component, get the list of the categories and release 
	 * the component.
	 * 
	 */
	private void initialize() throws Exception {
		// Get the AlarmService
		try {
			getAlarmService();
		} catch (Exception e) {
			contSvc.getLogger().log(AcsLogLevel.ERROR,"Error getting the AlarmSystem component: "+e.getMessage());
			throw new Exception("Error getting the AlarmSystem component: "+e.getMessage(),e);
		}
		if (alarm==null) {
			contSvc.getLogger().log(AcsLogLevel.ERROR,"Unknown error getting the AlarmService");
			throw new Exception("Unknown error getting the AlarmService");
		}
		// Read the available categories
		try {
			getCategories();
		} catch (Exception e) {
			contSvc.getLogger().log(AcsLogLevel.ERROR,"Error getting the categories from AlarmService");
			throw new Exception("Error getting the categories from AlarmService",e);
		}
		dumpCategories();
		if (categories==null || categories.length==0) {
			contSvc.getLogger().log(AcsLogLevel.INFO,"No alarm categories to subscribe to");
		}
	}
	
	/**
	 * Add a listener for the alarms.
	 * 
	 * Add the listeners to the set of listeners to be notified when
	 * a new alarms is received from the categories.
	 * 
	 * @param newListener The listener for alarms from categories
	 */
	public void addAlarmListener(CategoryListener newListener) {
		if (newListener==null) {
			throw new IllegalArgumentException("Invalid null listener");
		}
		synchronized(listeners) {
			listeners.add(newListener);
		}
	}
	
	/**
	 * Remove a listener from the list of listeners to be notified
	 * when a new alarm is received 
	 * 
	 * @param listener The not null listener to remove
	 * @return true if thle list of isteners contained the specified listener
	 */
	public boolean removeListener(CategoryListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null listener");
		}
		boolean ret;
		synchronized(listeners) {
			ret=listeners.remove(listener);
		}
		return ret;
	}
	
	/**
	 * This method is called by categories when a new message arrives and dispatches
	 * the alarm to the listeners.
	 * 
	 * @param newAlarm The alarm to send to the listeners
	 */
	public synchronized void dispatchAlarm(AlarmView newAlarm) {
		if (newAlarm==null) {
			throw new IllegalArgumentException("Invalid null alarm to dispatch");
		}
		synchronized(listeners) {
			for (CategoryListener listener: listeners) {
				listener.alarmReceived(newAlarm);
			}
		}
	}
	
	/**
	 * Close all the subscribers
	 */
	public void close() {
		for (CategorySubscriber cs: consumers) {
			cs.close();
		}
	}
}
