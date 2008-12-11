/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2008 
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
* "@(#) $Id: mockComponentImpl.cpp,v 1.1 2008/12/11 23:31:57 agrimstrup Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* agrimstrup  2008-12-10  created 
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*
*   
*   PARENT CLASS
*
* 
*   DESCRIPTION
*
*
*   PUBLIC METHODS
*
*
*   PUBLIC DATA MEMBERS
*
*
*   PROTECTED METHODS
*
*
*   PROTECTED DATA MEMBERS
*
*
*   PRIVATE METHODS
*
*
*   PRIVATE DATA MEMBERS
*
*
*   FILES
*
*   ENVIRONMENT
*
*   COMMANDS
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id: mockComponentImpl.cpp,v 1.1 2008/12/11 23:31:57 agrimstrup Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <mockComponentImpl.h>

/* ----------------------------------------------------------------*/
MockComponent::MockComponent( 
		       const ACE_CString &name,
		       maci::ContainerServices * containerServices) :
  ACSComponentImpl(name, (maci::ContainerServices *) containerServices)
{
}
/* ----------------------------------------------------------------*/
MockComponent::~MockComponent()
{
}
/*___oOo___*/
