#ifndef _ACSERROLDTEST__H_
#define _ACSERROLDTEST__H_

/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
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
* "@(#) $Id: acserrOldTest.h,v 1.4 2005/09/22 14:19:59 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* rlemke    30/08/01  created
*/
 
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif


#define ACS_TEST_INIT_LOGGING \
{ \
  LoggingProxy::init (m_logger); \
  ACS_SHORT_LOG((LM_INFO, "Logging proxy successfully created !")); \
}

#define ACS_TEST_INIT_CORBA \
{ \
   try \
    { \
      ACS_DEBUG("ACS_TEST_INIT_CORBA", "Initialising ORB ... "); \
      orb = CORBA::ORB_init (argc, argv, 0); \
      ACS_DEBUG ("ACS_TEST_INIT_CORBA", "ORB initialised !\n"); \
    } \
  catch(CORBA::Exception &ex) \
    { \
      ACE_PRINT_EXCEPTION (ex, "Failed to initalise ORB"); \
      return -1; \
    } \
}



#endif


