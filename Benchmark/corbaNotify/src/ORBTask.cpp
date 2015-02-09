/*
 * ORBTask.cpp
 *
 *  Created on: Sep 29, 2014
 *      Author: almamgr
 */

#include "ORBTask.h"

ORB_Task::ORB_Task(CORBA::ORB_ptr orb)
	: orb_(CORBA::ORB::_duplicate(orb))
{
}

ORB_Task::~ORB_Task()
{
}

int ORB_Task::svc()
{
	orb_->run();
	return 0;
}


