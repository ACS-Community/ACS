#include "maciSimpleClient.h"

void algo(char *name){
    maci::SimpleClient* client;




    client = new maci::SimpleClient();

    int argc = 1;
    char* argv[1] = {name};
    printf("\nabout to init %s\n",name);

    if (client->init(argc,argv)==0) {
        printf("\nFail at init %s\n",name);
    }
    printf("\nabout to login %s\n",name);
    if (client->login()==0) {
         printf("\nFail at login %s\n",name);
    }
    sleep(5);
    
    printf("\nabout to ping %s\n",name);
    if(!client->ping()){
        printf("\nping failed on %s\n",name);
    }
    ACS_SHORT_LOG((LM_DEBUG,"trying to log"));
    ACS_SHORT_LOG((LM_DEBUG,"trying to log"));
    ACS_SHORT_LOG((LM_DEBUG,"trying to log"));
    ACS_SHORT_LOG((LM_DEBUG,"trying to log"));
    ACS_SHORT_LOG((LM_DEBUG,"trying to log"));
    ACS_SHORT_LOG((LM_DEBUG,"trying to log"));
    ACS_SHORT_LOG((LM_DEBUG,"trying to log"));
    ACS_SHORT_LOG((LM_DEBUG,"trying to log"));
    ACS_SHORT_LOG((LM_DEBUG,"trying to log"));
    ACS_SHORT_LOG((LM_DEBUG,"trying to log"));
    ACS_SHORT_LOG((LM_DEBUG,"trying to log"));
    sleep(1);
    printf("\nabout to logout %s\n",name);
    if(client->logout()==0){
        
         printf("\nFail at logout %s\n",name);
        }
    printf("\nabout to destroy %s\n",name);
    if(client->destroy()==0){

         printf("\nFail at destroy %s\n",name);
    }
printf("\nending %s\n",name);

}
int main(){
    int *a=NULL;
    delete a;
    try{
    algo("test1");
    }catch(...){
        printf("\nError 1\n");
    }
    sleep(5);
    try{
    algo("test1");
    }catch(...){
        printf("\nError 2\n");
    }
    sleep(5);
    try{
    algo("test3");
    }catch(...){
        printf("\nError 3\n");
    }
    sleep(5);
    try{
    algo("test4");
    }catch(...){
        printf("\nError 4\n");
    }
    sleep(5);
    return 0;
}
