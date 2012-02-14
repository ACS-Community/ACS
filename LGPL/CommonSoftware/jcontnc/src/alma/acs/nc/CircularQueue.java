/*
 * ALMA - Atacama Large Millimiter Array (c) Associated Universities Inc., 2002
 * (c) National Radio Astronomy Observatory, 2009 Copyright by NRAO (in the framework of
 * the ALMA collaboration), All rights reserved
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

package alma.acs.nc;

import java.util.Vector;

import org.omg.CosNotification.StructuredEvent;

import alma.acs.nc.refactored.NCPublisher;

/**
 * Circular Queue that can buffer events while the Notify Service is down.
 * <p>
 * Currently we store the event data twice: Both the StructuredEvent with Corba Anys inside (incl. the user data), 
 * and also the user data separately. This design relieves us from transforming the user data back 
 * from Corba Any to IDL-generated struct.
 * If this becomes a memory issue, we may have to change it.
 * 
 * @see NCPublisher#publishCORBAEvent(StructuredEvent, T) 
 * @author Jorge Avarias <javarias [at] nrao.edu>
 */
public class CircularQueue<T> {
	
	public class Data {
		public Data(StructuredEvent corbaData, T userData) {
			this.corbaData = corbaData;
			this.userData = userData;
		}
		public StructuredEvent corbaData;
		public T userData;
	}
	
	private Vector<Data> queue;
	private int length;
	
	
	/**
	 * Initializes the Queue with custom value <b>size</b>
	 * 
	 * @param size the size of the queue
	 */
	public CircularQueue(int size){
		queue = new Vector<Data>(size+1);
	}
	
	/**
	 * Initializes the Queue with default value = 100
	 */
	public CircularQueue() {
		this(100);
	}
	
	/**
	 * Insert a structured event at the end of the queue. If the queue is
	 * full, inserts the structured event and remove the first event in the
	 * queue
	 * 
	 * @param e the event to be inserted in the queue
	 * @return <code>null</code> if all went well, 
	 *         or otherwise the data for another event that had to be pushed out of the queue 
	 *         if the queue was full.
	 */
	
	public Data push(StructuredEvent e, T userData) {
		Data ret = null;
		queue.add(new Data(e, userData));
		length++;
		if(length > queue.capacity()-1) {
			ret = queue.remove(0);
			length--;
		}
		return ret;
	}
	
	/**
	 * Remove all the elements of the queue
	 */
	public void clear(){
		queue.clear();
		length = 0;
	}
	
	/**
	 * Remove the first element in the queue and return it. If there is no
	 * elements in the queue returns <b>null</b>
	 *
	 * @return the first element in the queue
	 */
	public Data pop(){
		Data ret = null;
		try{
			ret = queue.remove(0);
			length--;
		}catch (IndexOutOfBoundsException ex)
		{}
		return ret;
	}
	
}
