#ifndef _ACSERRTEST__H_
#define _ACSERRTEST__H_

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
* "@(#) $Id: acserrTest.h,v 1.38 2005/09/22 14:20:17 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* rlemke    30/08/01  created
*/
 
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#define ACS_TEST_INIT_CORBA \
{ \
   try \
    { \
      ACS_DEBUG("ACS_TEST_INIT_CORBA", "Initialising ORB ... "); \
      orb = CORBA::ORB_init (argc, argv, 0); \
      ACS_DEBUG ("ACS_TEST_INIT_CORBA", "ORB initialised !\n"); \
    } \
  catch( CORBA::Exception &ex ) \
    { \
      ex._tao_print_exception("Failed to initalise ORB");	\
      return -1; \
    } \
}

#endif


