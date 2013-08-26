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
* "@(#) $Id: loggingXMLElement.cpp,v 1.35 2006/09/01 02:20:55 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/07/08  created
*/

#include <vltPort.h>
#include <loggingXMLElement.h>

 using namespace loggingXMLParser;

XMLElement::XMLElement()
{
  // Open the hash
  m_attributes.open ();
}

XMLElement::~XMLElement()
{
  // Close the hash
  m_attributes.close ();
}

const ACE_TCHAR *
XMLElement::name()
{
  return m_name.c_str();
}
    
const ACE_TCHAR *
XMLElement::value()
{
  return m_value.c_str();
}

void
XMLElement::name(const ACE_CString& name)
{
  m_name = name;
}
    
void
XMLElement::value(const ACE_CString& value)
{
  m_value = value;
}


int
XMLElement::addAttribute (const ACE_CString& name, const ACE_CString& value)
{
  //  bind the id to the data in the hash_map
  if (m_attributes.bind (name, value) != 0)
    return -1; // return code for failure
  else
    return 0;
}

int
XMLElement::getAttribute (const ACE_CString& name, ACE_CString& value)
{
  return m_attributes.find (name, value);
}

XMLElement::ATTRIBUTE_HASH_MAP_ITER
XMLElement::getAttributesIterator (void)
{
  return XMLElement::ATTRIBUTE_HASH_MAP_ITER(m_attributes);
}

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: loggingXMLElement.cpp,v $
// Revision 1.35  2006/09/01 02:20:55  cparedes
// small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
// Revision 1.34  2003/03/14 10:24:49  rgeorgie
// LGPL
//
// Revision 1.33  2002/09/23 12:45:09  vltsccm
// msekoran: loggingXMLParser fixed, memory leak removed and tat test added.
//
// Revision 1.32  2002/04/10 14:41:40  vltsccm
// logging1.32
//
// Revision 1.31  2002/03/27 16:44:26  vltsccm
// logging1.31
//
// Revision 1.30  2002/02/13 12:55:34  vltsccm
// logging1.30
//
// Revision 1.29  2002/02/08 13:40:56  vltsccm
// logging1.29
//
// Revision 1.28  2002/02/05 17:51:56  vltsccm
// logging1.28
//
// Revision 1.27  2002/02/04 08:26:30  vltsccm
// logging1.27
//
// Revision 1.26  2002/01/18 09:43:00  vltsccm
// logging1.26
//
// Revision 1.25  2002/01/16 10:41:30  vltsccm
// logging1.25
//
// Revision 1.24  2002/01/15 12:42:22  vltsccm
// logging1.24
//
// Revision 1.23  2002/01/14 21:10:52  vltsccm
// logging1.23
//
// Revision 1.22  2001/12/27 19:04:11  vltsccm
// logging1.22
//
// Revision 1.21  2001/12/24 13:31:34  vltsccm
// logging1.21
//
// Revision 1.20  2001/11/14 08:44:19  vltsccm
// logging1.20
//
// Revision 1.19  2001/09/13 14:24:34  vltsccm
// logging1.19
//
// Revision 1.18  2001/08/13 07:04:58  vltsccm
// logging1.18
//
// Revision 1.17  2001/07/12 20:23:04  vltsccm
// logging1.17
//
// Revision 1.16  2001/07/12 13:26:00  vltsccm
// logging1.16
//
// Revision 1.15  2001/07/12 12:38:19  vltsccm
// logging1.15
//
// Revision 1.14  2001/07/12 08:56:19  vltsccm
// logging1.14
//
// Revision 1.13  2001/07/12 07:08:31  vltsccm
// logging1.13
//
// Revision 1.12  2001/07/11 09:13:56  vltsccm
// logging1.12
//
// Revision 1.11  2001/07/11 09:13:56  vltsccm
// logging1.11
//
// Revision 1.10  2001/07/11 09:13:56  vltsccm
// logging1.10
//
// Revision 1.9  2001/07/11 09:13:55  vltsccm
// logging1.9
//
// Revision 1.8  2001/07/11 09:13:54  vltsccm
// logging1.8
//
// Revision 1.7  2001/07/11 09:13:54  vltsccm
// logging1.7
//
// Revision 1.6  2001/07/11 09:13:54  vltsccm
// logging1.6
//
// Revision 1.5  2001/07/11 09:13:53  vltsccm
// logging1.5
//
// Revision 1.4  2001/07/11 09:13:53  vltsccm
// logging1.4
//
// Revision 1.3  2001/07/11 09:13:53  vltsccm
// logging1.3
//
// Revision 1.2  2001/07/11 09:13:52  vltsccm
// logging1.2
//
// Revision 1.1  2001/07/11 09:13:52  vltsccm
// logging1.1
//
// Revision 1.0  2001/07/11 09:13:52  vltsccm
// logging1.0
//
//
// ************************************************************************
