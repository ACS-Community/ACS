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
