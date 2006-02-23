#ifndef _BACITEST__H_
#define _BACITEST__H_

/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciTest.h,v 1.93 2005/02/09 16:08:25 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* rlemke    30/08/01  created
*/

/** 
 * @file 
 * Header file for BACI Test.
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
   ACE_TRY_EX (orb_block) \
    { \
      ACS_DEBUG("ACS_TEST_INIT_CORBA", "Initialising ORB ... "); \
      orb = CORBA::ORB_init (argc, argv, 0); \
      ACE_TRY_CHECK_EX (orb_block); \
      ACS_DEBUG ("ACS_TEST_INIT_CORBA", "ORB initialised !"); \
    } \
  ACE_CATCHANY \
    { \
      ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION, "Failed to initalise ORB"); \
      return -1; \
    } \
  ACE_ENDTRY; \
}

#define ACS_TEST_RESOLV_NAMING_SERVICE \
{ \
  ACE_TRY_EX (ce_log_block) \
    { \
      ACS_DEBUG ("errorServer", "Resolving  Naming service ... "); \
      CORBA::Object_var naming_obj = \
        orb->resolve_initial_references ("NameService"); \
      ACE_TRY_CHECK_EX (ce_log_block); \
      if (!CORBA::is_nil (naming_obj.in ())) \
        { \
          CosNaming::NamingContext_var naming_context = \
            CosNaming::NamingContext::_narrow (naming_obj.in ()); \
          ACE_TRY_CHECK_EX (ce_log_block); \
          ACS_DEBUG ("errorServer", "Naming Service resolved !"); \
          ACS_DEBUG ("erorServer", "Resolving Logging Service from Naming service .... "); \
          CosNaming::Name name; \
          name.length(1); \
          name[0].id = CORBA::string_dup("Log"); \
          CORBA::Object_var log_obj = naming_context->resolve(name); \
          ACE_TRY_CHECK_EX (ce_log_block); \
          if (!CORBA::is_nil (log_obj.in())) \
            { \
              DsLogAdmin::Log_var logger = DsLogAdmin::Log::_narrow(log_obj.in()); \
              ACE_TRY_CHECK_EX (ce_log_block); \
              ACS_DEBUG ("errorServer", "Logging Service resolved !"); \
              m_logger->setCentralizedLogger(logger.in()); \
            } \
          else \
            { \
              ACS_DEBUG ("errorServer", "Failed to initialise the Logging Service!"); \
            } \
        } \
      else \
        { \
          ACS_DEBUG ("errorServer", "Failed to initialise the NameService!"); \
        } \
    } \
  ACE_CATCHANY \
    { \
      ACE_PRINT_EXCEPTION(ACE_ANY_EXCEPTION, "Failed to get and set the centralized logger"); \
    } \
  ACE_ENDTRY; \
}

#endif


