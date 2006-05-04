/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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
package alma.acs.logging;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class LogConfig {

	private List<LogConfigSubscriber> subscriberList;
	
	public LogConfig() {
		subscriberList = new ArrayList<LogConfigSubscriber>();
	}

	/**
	 * Initializes the values based on various CDB settings, properties etc.
	 * This method can be called more than once, e.g. if any of the values has changed.
	 */
	void initialize() {
		
		notifySubscribers();		
	}
	
	static interface LogConfigSubscriber {
		void initializeLogConfig(LogConfig logConfig);
	}
	
	void addSubscriber(LogConfigSubscriber subscriber) {
		subscriberList.add(subscriber);
	}
	
	void notifySubscribers() {
		for (Iterator<LogConfigSubscriber> iter = subscriberList.iterator(); iter.hasNext();) {
			LogConfigSubscriber subscriber = iter.next();
			subscriber.initializeLogConfig(this);
		}
	}
}
