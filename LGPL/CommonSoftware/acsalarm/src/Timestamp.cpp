/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
#include "Timestamp.h"
#include <sys/time.h>
#include <sstream>
#include <iostream>
#include <stdio.h>

using std::stringstream;
using std::string;
using acsalarm::Timestamp;

/**
 * Default no-args constructor, creates an instance with the time at instantiation.
 */
Timestamp::Timestamp()
{
	// TODO later: portability issues with gettimeofday?
	// i.e. will this work on non-unix platforms? do we care?
	timeval tim;
	gettimeofday(&tim, NULL);
	setSeconds(tim.tv_sec);
	setMicroSeconds(tim.tv_usec);
}

/**
 * Constructor to instantiate and configure a Timestamp with a time.
 */
Timestamp::Timestamp(long secs, long microSecs)
{
	setSeconds(secs);
	setMicroSeconds(microSecs);
}

/*
 * Copy constructor
 */
Timestamp::Timestamp(const Timestamp & ts)
{
	seconds = ts.seconds;
	microSeconds = ts.microSeconds;
}

/**
 * Destructor.
 */
Timestamp::~Timestamp()
{
}

/*
 * Assignment operator.
 */
Timestamp & Timestamp::operator=(const Timestamp & rhs)
{
	setSeconds(rhs.seconds);
	setMicroSeconds(rhs.microSeconds);
	return *this;
}

/*
 * Equality operator.
 */
int Timestamp::operator==(const Timestamp &rhs) const
{
	int retVal = 1;
	if(rhs.seconds != seconds || rhs.microSeconds != microSeconds)
	{
		retVal = 0;
	}
	return retVal;
}

std::string Timestamp::toISOFormat() const {
	struct timeval tval;
	tval.tv_sec=getSeconds();
	tval.tv_usec=getMicroSeconds();

	struct tm* temptm = gmtime(&tval.tv_sec);

	char tmbuf[64], buf[64];
	strftime(tmbuf, sizeof tmbuf, "%FT%H:%M:%S", temptm);
	snprintf(buf, sizeof buf, "%s.%03ld", tmbuf, tval.tv_usec/1000);

	std::string ret=buf;
	return ret;
}

