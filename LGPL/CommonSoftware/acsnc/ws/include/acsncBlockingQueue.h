#ifndef ACS_NC_BLOCKING_QUEUE_H_
#define ACS_NC_BLOCKING_QUEUE_H_
#include <queue>
#include <exception>
#include <pthread.h>
#include <tao/corba.h>

#define ACS_NC_CONSUMER_MAX_BUFFER_SIZE 100

namespace nc {

template<class T>
struct event_info {
    T event;
    double maxProcessTime;
    std::string type_name;
    event_info(T &event, double &maxProcessTime, std::string & type_name);
    event_info(T &event, double &maxProcessTime, const char *type_name);
};

template<class T>
class blocking_queue {
private:
    std::queue<event_info<T> > buffer;
    pthread_mutex_t mutex;
    pthread_cond_t cond;

public:
    blocking_queue();
    ~blocking_queue();
    void push (event_info<T> &data);
    /**
     * @throws interrupted_blocking_queue if the queue was interrupted
     */
    event_info<T> pop();
    unsigned int size();
    void unblock();
};

class interrupted_blocking_queue: public std::exception {
};

}
#include "acsncBlockingQueue.i"

#endif
