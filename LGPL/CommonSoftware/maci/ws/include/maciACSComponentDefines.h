#ifndef MACIACSCOMPONENTDEFINES_H
#define MACIACSCOMPONENTDEFINES_H
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
* "@(#) $Id: maciACSComponentDefines.h,v 1.17 2005/04/14 09:27:01 acaproni Exp $"
*
* who       when        what
* --------  --------    ----------------------------------------------
* msekoran  2003-09-17  ConstructCOB renamed to ConstructComponent
* rcirami   2003-08-28  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acserrACSbaseExImpl.h>


/** @file maciACSComponentDefines.h
 *  This file contains macros that are very helpful in creating distributed
 *  objects.
 */

////////////////////////////////////////////////////////////////////////

#include <baciCharacteristicComponentImpl.h>
#include <acserr.h>
#include <maciErrTypeComponent.h>

/** \def MACI_DLL_SUPPORT_FUNCTIONS(Class, ...)
 *  MACI DLL Support Functions Macro
 *  This macro implements the DLL support functions for a specific class.<br><br>
 *  ConstructComponent(...) creates a CORBA object of type "Class".<br>
 *  DLLOpen(...) presently does nothing.<br>
 *  DLLClose() presently does nothing...
 *  @param Class Name of the class
 *  @param args... (variable number of arguements)  WE need it that template class with template list longer than one can be send as a class
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly
 */

// !!!! to be able to handel also template classes with template list longer than one we have to add args.... (variable macro arguemnt)
// in this case the class name that is send to the logging system (ACS_LOG) does not add comma between first and second template argument
#define MACI_DLL_SUPPORT_FUNCTIONS(Class, args...) \
ACS_DLL_UNMANGLED_EXPORT PortableServer::Servant ConstructComponent( \
	                  CORBA::ULong, \
				      const char * name_p, \
				      const char * type_p, \
                      maci::ContainerServices * containerServices) \
{ \
    ACE_UNUSED_ARG(type_p); \
    Class,##args* servant_p =0; \
    try { \
	servant_p = new Class,##args(name_p, containerServices); \
    } catch (ACSErr::ACSbaseExImpl& ex) \
    {\
	std::string  procName="ConstructComponent::ConstructComponent(...,";\
        procName+=name_p;\
	procName+=",";\
	procName+=type_p;\
        procName+=",...)";\
	maciErrTypeComponent::ComponentCreationExImpl compEx(ex,__FILE__,__LINE__,procName.c_str());\
	compEx.addData("Component", name_p);\
	compEx.log();\
	return 0; \
    } \
    catch (...)\
    {\
	ACS_LOG(LM_RUNTIME_CONTEXT, #Class #args"/ exception caught", \
		(LM_ERROR, "Failed to create component - constructor returned 0!")); \
	return 0; \
    }\
    if (servant_p == 0) \
	{ \
		ACS_LOG(LM_RUNTIME_CONTEXT, #Class #args"/DLLOpen", \
			(LM_ERROR, "Failed to create component - constructor returned 0!")); \
		return 0; \
	} \
    else if (servant_p->componentState() != ACS::COMPSTATE_NEW) \
	{ \
		ACS_LOG(LM_RUNTIME_CONTEXT, #Class #args"/DLLOpen", \
			(LM_ERROR, "Failed to create component - initialization error!")); \
		servant_p->_remove_ref(); \
		return 0; \
	} \
    else \
	{ \
		return servant_p; \
	} \
} \
\
ACS_DLL_UNMANGLED_EXPORT bool DLLOpen(int, char**) \
{ \
    return true; \
} \
\
ACS_DLL_UNMANGLED_EXPORT void DLLClose() \
{ \
}


#endif /*MACICOMPONENTDEFINES_H*/


