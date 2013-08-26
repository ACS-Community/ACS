#ifndef ACS_NC_BLOCKING_QUEUE_I_
#define ACS_NC_BLOCKING_QUEUE_I_
#include "acsncBlockingQueue.h"

namespace nc {
template<class T>
blocking_queue<T>::blocking_queue(){
    pthread_mutex_init(&mutex, NULL);
    pthread_cond_init(&cond, NULL);
}
template<class T>
blocking_queue<T>::~blocking_queue(){
    unblock();
}

template<class T>
void blocking_queue<T>::push(event_info<T> &data) {
    pthread_mutex_lock(&mutex);
    buffer.push(data);
    pthread_mutex_unlock(&mutex);
    pthread_cond_signal(&cond);
}

template<class T>
event_info<T> blocking_queue<T>::pop() {
    pthread_mutex_lock(&mutex);
    if(buffer.size() == 0)
        pthread_cond_wait(&cond, &mutex);
    if (buffer.size() == 0) {
        pthread_mutex_unlock(&mutex);
        interrupted_blocking_queue ex;
        throw ex;
    }
    event_info<T> ret_val = buffer.front();
    buffer.pop();
    pthread_mutex_unlock(&mutex);
    return ret_val;
}

template<class T>
void blocking_queue<T>::pop_no_block() {
    pthread_mutex_lock(&mutex);
    if(buffer.size() == 0) {
        return;
    }
    buffer.pop();
    pthread_mutex_unlock(&mutex);
    return;
}

template<class T>
unsigned int blocking_queue<T>::size() {
    return buffer.size();
}

template<class T>
void blocking_queue<T>::unblock() {
    pthread_cond_broadcast(&cond);
}

template<class T>
event_info<T>::event_info(T &event, double &maxProcessTime, std::string & type_name) :
        event(event), maxProcessTime(maxProcessTime), type_name(type_name) {
}

template<class T>
event_info<T>::event_info(T &event, double &maxProcessTime, const char *type_name) :
        event(event), maxProcessTime(maxProcessTime), type_name(type_name) {
}

}
#endif
