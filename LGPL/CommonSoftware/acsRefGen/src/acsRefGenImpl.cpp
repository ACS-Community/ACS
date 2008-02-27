/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2008 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acsRefGenImpl.cpp,v 1.2 2008/02/27 15:55:51 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2008-02-27  created 
*/


#include "vltPort.h"

static char *rcsId="@(#) $Id: acsRefGenImpl.cpp,v 1.2 2008/02/27 15:55:51 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsRefGenImpl.h"

using namespace ACS;

CORBA::Object_ptr acsRefGenImpl::createRef (
        const char * host,
        const char * objectKey,
        const char * RID,
        const char * protocol)
{
	return 0;
}

/*___oOo___*/
