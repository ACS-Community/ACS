#ifndef loggingXMLElement_H
#define loggingXMLElement_H

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
* "@(#) $Id: loggingXMLElement.h,v 1.37 2012/01/20 22:07:44 tstaig Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001-07-08  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <loggingExport.h>
#include <ace/SString.h>
#include <ace/Hash_Map_Manager.h>
#include <ace/Thread_Mutex.h>

namespace loggingXMLParser {

class logging_EXPORT XMLElement
{
public:

  /// Constrcutor
  XMLElement();

  /// Destrcutor
  ~XMLElement();


  /// Attributes name-value hashmap typedefs
  typedef ACE_Hash_Map_Manager <ACE_CString, ACE_CString, ACE_Thread_Mutex> ATTRIBUTE_HASH_MAP;
  typedef ACE_Hash_Map_Iterator <ACE_CString, ACE_CString, ACE_Thread_Mutex> ATTRIBUTE_HASH_MAP_ITER;
  typedef ACE_Hash_Map_Entry <ACE_CString, ACE_CString> ATTRIBUTE_HASH_MAP_ENTRY;

  /**
   * Get name
   * @return name
   */
  const ACE_TCHAR * name();
    
  /**
   * Get value
   * @return value
   */
  const ACE_TCHAR * value();

  /**
   * Set name
   */
  void name(const ACE_CString& name);
    
  /**
   * Set value
   */
  void value(const ACE_CString& value);

  /**
   * Add attribute to the element
   * WARNING: Names are case-sensitive!!!
   * @param name
   * @param value 
   * @return 0 on success, -1 on failure
   */
  int addAttribute (const ACE_CString& name, const ACE_CString& value);

  /**
   * Get attribute of the element
   * @param name
   * @param value
   * @return 0 on success, -1 on failure
   */
  int getAttribute (const ACE_CString& name, ACE_CString& value);

  /**
   * Get iterator of all element's attributes
   * @return iterator of all element's attributes
   */
  ATTRIBUTE_HASH_MAP_ITER getAttributesIterator (void);

private:

  /// XML element name
  ACE_CString m_name;

  /// XML element value
  ACE_CString m_value;

  /// XML element attributes
  ATTRIBUTE_HASH_MAP m_attributes;

  /*
    // future releases, ACS1.0 does not need this
    // for now XML parser needs only to parse <Archive ...>value</Archive>
    XMLElement * parentNode;
    XMLElement * childrenNodes[];  // use ACEs DL list
  */

};

 }; 


#endif /*!loggingXMLElement_H*/

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: loggingXMLElement.h,v $
// Revision 1.37  2012/01/20 22:07:44  tstaig
// Backport from branches ACS-9_0_0-windows-B and ACS-9_1_0-windows-B to support
// ACS on Windows under Cygwin. This commit corresponds to the folowing
// CommonSoftware modules:
// jacsutil acsEclipseUtils xmljbind xmlpybind acserridl acsidlcommon acsutil
// acsutilpy acsstartup loggingidl logging acserr acserrTypes acsQoS
// Along with adding dependencies for some libraries in acsdaemon and acstime
// modules so they would be built correctly.
//
// Revision 1.36  2006/09/01 02:20:55  cparedes
// small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
// Revision 1.35  2004/03/17 07:38:33  bjeram
// ported to ACE 5.4 and TAO 1.4
//
// Revision 1.34  2003/03/14 10:24:37  rgeorgie
// LGPL
//
// Revision 1.33  2002/09/23 12:43:05  vltsccm
// msekoran: loggingXMLParser fixed, memory leak removed and tat test added.
//
// Revision 1.32  2002/04/10 14:41:38  vltsccm
// logging1.32
//
// Revision 1.31  2002/03/27 16:44:22  vltsccm
// logging1.31
//
// Revision 1.30  2002/02/13 12:55:30  vltsccm
// logging1.30
//
// Revision 1.29  2002/02/08 13:40:53  vltsccm
// logging1.29
//
// Revision 1.28  2002/02/05 17:51:54  vltsccm
// logging1.28
//
// Revision 1.27  2002/02/04 08:26:27  vltsccm
// logging1.27
//
// Revision 1.26  2002/01/18 09:42:57  vltsccm
// logging1.26
//
// Revision 1.25  2002/01/16 10:41:28  vltsccm
// logging1.25
//
// Revision 1.24  2002/01/15 12:42:19  vltsccm
// logging1.24
//
// Revision 1.23  2002/01/14 21:10:48  vltsccm
// logging1.23
//
// Revision 1.22  2001/12/27 19:04:08  vltsccm
// logging1.22
//
// Revision 1.21  2001/12/24 13:30:58  vltsccm
// logging1.21
//
// Revision 1.20  2001/11/14 08:44:16  vltsccm
// logging1.20
//
// Revision 1.19  2001/09/13 14:24:33  vltsccm
// logging1.19
//
// Revision 1.18  2001/08/13 07:04:56  vltsccm
// logging1.18
//
// Revision 1.17  2001/07/12 20:23:03  vltsccm
// logging1.17
//
// Revision 1.16  2001/07/12 13:25:59  vltsccm
// logging1.16
//
// Revision 1.15  2001/07/12 12:38:17  vltsccm
// logging1.15
//
// Revision 1.14  2001/07/12 08:56:17  vltsccm
// logging1.14
//
// Revision 1.13  2001/07/12 07:08:24  vltsccm
// logging1.13
//
// Revision 1.12  2001/07/11 09:13:40  vltsccm
// logging1.12
//
// Revision 1.11  2001/07/11 09:13:39  vltsccm
// logging1.11
//
// Revision 1.10  2001/07/11 09:13:39  vltsccm
// logging1.10
//
// Revision 1.9  2001/07/11 09:13:39  vltsccm
// logging1.9
//
// Revision 1.8  2001/07/11 09:13:38  vltsccm
// logging1.8
//
// Revision 1.7  2001/07/11 09:13:38  vltsccm
// logging1.7
//
// Revision 1.6  2001/07/11 09:13:37  vltsccm
// logging1.6
//
// Revision 1.5  2001/07/11 09:13:37  vltsccm
// logging1.5
//
// Revision 1.4  2001/07/11 09:13:36  vltsccm
// logging1.4
//
// Revision 1.3  2001/07/11 09:13:36  vltsccm
// logging1.3
//
// Revision 1.2  2001/07/11 09:13:36  vltsccm
// logging1.2
//
// Revision 1.1  2001/07/11 09:13:35  vltsccm
// logging1.1
//
// Revision 1.0  2001/07/11 09:13:35  vltsccm
// logging1.0
//
//
// ************************************************************************
