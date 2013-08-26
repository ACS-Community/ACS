/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 *
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
*
*
* "@(#) $Id: acscourseMount2ImplDLL.cpp,v 1.2 2005/07/12 11:30:11 gchiozzi Exp $"
*
*/
 
/*
 * ATTENTION:
 * Because the class Mount2ImplLopp will be derived from Mount2Impl, but
 * we also want to be able to activate a PS object on it's own, we must separate the DLL 
 * functions used by Container from the *Impl.cpp file
 * and put it them in the acscourseMount2ImplDLL.cpp file 
 * This file will be explicitly linked with the library building up the
 * component code, while the object code for acscourseMount2Impl.cpp
 * can be linked directly in libraries from derived components or we can
 * create a library that contains only that object file and not the
 * acscourseMount2ImplLoop.o object.
 */

#include "acscourseMount2Impl.h"

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(Mount2Impl)
/* ----------------------------------------------------------------*/


/*___oOo___*/


