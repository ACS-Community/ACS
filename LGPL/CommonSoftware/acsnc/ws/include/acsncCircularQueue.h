#ifndef _NC_CIRCULAR_QUEUE_H_
#define _NC_CIRCULAR_QUEUE_H_

#include <deque>
#include <orbsvcs/CosNotificationC.h>

namespace nc{
   class CircularQueue{
      private:
         std::deque<CosNotification::StructuredEvent> queue;
         unsigned int length;
         const unsigned int max_size;

     public:
         CircularQueue(unsigned int size = 100);
         void push(CosNotification::StructuredEvent e);
         void clear();
         CosNotification::StructuredEvent *pop();
   };
}

#endif
