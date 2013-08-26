#ifndef _INMEMORYXMLDATA_H
#define _INMEMORYXMLDATA_H

/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 
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
* "@(#) $Id: InMemoryXmlData.h,v 1.6 2006/11/29 23:01:26 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  1/11/05  created
*/
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/XMLString.hpp>
#include <string>
#include <iostream>

using std::cerr;
using std::endl;
using std::string;
using XERCES_CPP_NAMESPACE_QUALIFIER XMLString;

namespace Parameters 
{
	//------------------------------------------------------------------------------
	//
	//  InMemoryXmlData   One of these objects will be set up for the parameter set file listed
	//             on the command line.  Once set, the data is unchanging.
	//------------------------------------------------------------------------------
	class InMemoryXmlData
	{
		private:
			string  id;
			string  content;

		public :
			// -----------------------------------------------------------------------
			//  Constructors and Destructor
			// -----------------------------------------------------------------------

			/**
			 * Constructor.
			 * @param identifier - string indicating a name/ID for the data
			 * @param contents - string containing the XML
			 */
			InMemoryXmlData(string identifier, string contents)
			{
				content = contents;
				id = identifier;
			}

			~InMemoryXmlData()
			{
			}

			string getId() { return id;}
			string getContent() { return content;}
			const size_t getSize() { return (const size_t) content.length(); }
	};
}
#endif /*!_FILEINFO_H*/

