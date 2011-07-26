#include "bulkDataNTCallback.h"
#include <iostream>


int BulkDataCallback::cbStart(unsigned char* param, unsigned  int size)
{
	std::cerr << "Depreciated call BulkDataCallback::cbStart (ACE_Message_Block)" << std::endl;
	ACE_Message_Block mb((char*)param, size); // dirty ????
	mb.length(size);
	this->cbStart(&mb);
}

int BulkDataCallback::cbReceive(unsigned char * frame, unsigned  int size)
{
	std::cerr << "Depreciated call BulkDataCallback::cbReceive (ACE_Message_Block)" << std::endl;
	ACE_Message_Block mb((char*)frame, size); // dirty ????
	mb.length(size);
	this->cbReceive(&mb);
}
