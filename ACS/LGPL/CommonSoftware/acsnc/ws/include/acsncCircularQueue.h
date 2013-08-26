/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) National Radio Astronomy Observatory, 2009
*    Copyright by NRAO (in the framework of the ALMA collaboration)
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
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*  
*
* "@(#) $Id:"
*/ 


#ifndef _NC_CIRCULAR_QUEUE_H_
#define _NC_CIRCULAR_QUEUE_H_

#include <deque>
#include <iostream>
#include <orbsvcs/CosNotificationC.h>

namespace nc{
   /**
    * Circular Queue implemented to maintain temporally structured events when 
    * the Notify Service is down.
    * 
    */
   class CircularQueue{
      private:
         std::deque<CosNotification::StructuredEvent> queue;
         unsigned int length;
         const unsigned int max_size;

     public:
         /**
          * Initializes the Queue with custom value <b>size</b>, by default
          * the size value is <b>100</b>
          * 
          * @param size the size of the queue
          */
         CircularQueue(unsigned int size = 100);

         /**
          * Clears the internal queue
          */
         ~CircularQueue();

        /**
         * Insert a structured event at the end of the queue. If the queue is
         * full, inserts the structured event and remove the first event in the
         * queue
         * 
         * @param e the event to be inserted in the queue
			* @throws EventDroppedException if the queue drop a message after to 
			* push the event e
         */
         void push(CosNotification::StructuredEvent e);
         
         /**
          * Remove all the elements of the queue
          */
         void clear();

         /**
          * Removes the first element in the queue and return it. If there is no
          * elements in the queue returns <b>null</b>
          */
         void pop();

         /**
          * Returns the first element of the circular queue, without removing it
          * from the queue.
          *
          * @return the first element in the queue
          */
         CosNotification::StructuredEvent *front();

         unsigned int size() { return queue.size(); }
   };

	class EventDroppedException: std::exception
	{
	};
}

#endif
