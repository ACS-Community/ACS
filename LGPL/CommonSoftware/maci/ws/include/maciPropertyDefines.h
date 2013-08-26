#ifndef MACIPROPERTYDEFINES_H
#define MACIPROPERTYDEFINES_H
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
* "@(#) $Id: maciPropertyDefines.h,v 1.2 2003/10/23 08:06:25 acaproni Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* rcirami   2003-08-28  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

/** @file maciPropertyDefines.h
 *  This file contains macros that are very helpful in creating distributed
 *  objects.
 */
////////////////////////////////////////////////////////////////////////
/** @def MACI_PROPERTY(IdlAccessor, CppImplVar)
 *  This macro checks to see if a property object has been created and initialized correctly.  
 *  It should normally be put in the distributed object's constructor after creating each 
 *  new property.
 *
 *  Also adds property information to a vector that is then used to define the descriptor. 
 *  Use this macro for each of the properties.
 *  @param IdlAccessor Name of the property located in the IDL file.  This is also the name 
 *  of the C++ method that must be implemented to acccess the IDL property.
 *  @param CppImplVariable Name of the C++ variable used in the C++ method for each IDL 
 *  property.  CORBA poses no restrictions on the name of this variable although the "C++ 
 *  Coding Standards" document does.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly
 */
#define MACI_PROPERTY(IdlAccessor, CppImplVar) \
    if (!CppImplVar) return; \
    else if (CppImplVar->initialization()) \
        return; \
    { \
     \
    m_desc->properties.length(m_desc->properties.length()+1); \
    m_desc->properties[m_desc->properties.length()-1].property_ref = this->IdlAccessor(); \
    m_desc->properties[m_desc->properties.length()-1].name = CppImplVar->name(); \
    m_desc->properties[m_desc->properties.length()-1].id = CppImplVar->id(); \
    m_desc->properties[m_desc->properties.length()-1].characteristics = CppImplVar->get_all_characteristics(0); \
    }

#endif /*MACIPROPERTYDEFINES_H*/




