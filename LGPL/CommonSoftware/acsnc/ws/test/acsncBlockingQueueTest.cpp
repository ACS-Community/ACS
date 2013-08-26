#include <iostream>
#include "acsncBlockingQueue.h"
#include <typeinfo>

void *producer (void *arg);

int main(int argc, char * argv[]) {
    bool stop = false;
    pthread_t t;
    nc::blocking_queue<int> buffer;
    pthread_create(&t, NULL, producer, reinterpret_cast<void *>(&buffer));
    while(!stop) {
        try {
            std::cout << "popping item: " << buffer.pop().event << std::endl;
        } catch (nc::interrupted_blocking_queue &ex) {
            std::cout << "Time to say goodbye" << std::endl;
            stop = true;
        }
    }
    pthread_join(t, NULL);
    return 0;
}

void *producer (void *arg) {
    nc::blocking_queue<int> *buffer = reinterpret_cast<nc::blocking_queue<int> *>(arg);
    for (int i = 0; i < 10; i++){
        nc::event_info<int> event(i, (double&)i, "int");
        buffer->push(event);
        sleep(1);
    }
    buffer->unblock();
    sleep(5);
    return 0;
}


