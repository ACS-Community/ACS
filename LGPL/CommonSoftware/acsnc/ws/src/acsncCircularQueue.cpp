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

#include "acsncCircularQueue.h"

using namespace nc;

CircularQueue::CircularQueue(unsigned int size):
   max_size(size)
{
}

void CircularQueue::push(CosNotification::StructuredEvent e)
{
   queue.push_back(e);
   length++;
   if (length > max_size -1){
      queue.pop_front();
      length--;
		EventDroppedException ex;
		throw ex;
   }
}

void CircularQueue::clear()
{
   queue.clear();
   length = 0;
}

CosNotification::StructuredEvent *CircularQueue::pop()
{
   CosNotification::StructuredEvent *e = NULL;
   if(queue.size() > 0){
      CosNotification::StructuredEvent tmp = queue[0];
      e = new CosNotification::StructuredEvent(tmp);
      length--;
   }
   return e;
}
