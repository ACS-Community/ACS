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

/**
 * Circular Queue implemented to maintain temporally structured events when 
 * the Notify Service is down.
 * 
 * @see SimpleSupplier.publishCORBAEvent
 * 
 * @author Jorge Avarias <javarias [at] nrao.edu>
 */
public class CircularQueue {
	private Vector<StructuredEvent> queue;
	private int length;
	
	
	/**
	 * Initializes the Queue with custom value <b>size</b>
	 * 
	 * @param size the size of the queue
	 */
	public CircularQueue(int size){
		queue = new Vector<StructuredEvent>(size+1);
		size = 0;
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
	 */
	
	public void push(StructuredEvent e){
		queue.add(e);
		length++;
		if(length > queue.capacity()-1){
			queue.remove(0);
			length--;
		}
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
	public StructuredEvent pop(){
		StructuredEvent e = null;
		try{
			e = queue.remove(0);
			length--;
		}catch (IndexOutOfBoundsException ex)
		{}
		return e;
	}
}
