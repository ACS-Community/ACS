#ifndef loggingXMLParser_H
#define loggingXMLParser_H

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
* "@(#) $Id: loggingXMLParser.h,v 1.35 2006/09/01 02:20:55 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2002-01-17  NONE -> XP_NONE (warnings on LCU)
* msekoran  2001-07-08  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <loggingExport.h>

#include <loggingXMLElement.h>

namespace loggingXMLParser {

#define XP_NONE 0

#define ELEMENT_BEGIN_CHAR 1
#define ELEMENT_DATA 2
#define ELEMENT_NAME 3

#define ELEMENT_END_CHAR 4
#define ELEMENT_END 5


#define ATTRIBUTE_AREA 6
#define ATTRIBUTE_NAME 7
#define ATTRIBUTE_VALUE_BEGIN 8
#define ATTRIBUTE_VALUE 9
#define ATTRIBUTE_VALUE_END 10

#define CDATA 11
#define CDATA_END 12

#define SPECIAL_SECTION 13

/**
 * Simple non-validating XML parser
 * This XML parser is used to parse attributes and a value of <Archive ...>value</Archive>
 * It does not support parsing of sub-elements (only one level).
 * (This is due to lack of time of the programmer, give me 2h and ...)
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: loggingXMLParser.h,v 1.35 2006/09/01 02:20:55 cparedes Exp $"
 */
class logging_EXPORT XMLParser
{

public:

  /**
   * Parse XML string
   * @param xml XML formatted string
   * @param skipValueParsing if true, only element name and its attributes will be parsed
   * @return pointer to XMLEntry, 0 if failed to parse given string
   */
  static XMLElement * parseString(const ACE_TCHAR * xml, bool skipValueParsing=false);

  /**
   * Parse XML element type
   * @param xml XML formatted string
   * @param entryType redtuned element typed if found
   * @return 0 on success
   */
  static int parseElementType(const ACE_TCHAR * xml, ACE_CString &elementType);
};

 }; 

#endif /*!loggingXMLParser_H*/

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: loggingXMLParser.h,v $
// Revision 1.35  2006/09/01 02:20:55  cparedes
// small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
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
// Revision 1.31  2002/03/27 16:44:23  vltsccm
// logging1.31
//
// Revision 1.30  2002/02/13 12:55:31  vltsccm
// logging1.30
//
// Revision 1.29  2002/02/08 13:40:53  vltsccm
// logging1.29
//
// Revision 1.28  2002/02/05 17:51:54  vltsccm
// logging1.28
//
// Revision 1.27  2002/02/04 08:26:28  vltsccm
// logging1.27
//
// Revision 1.26  2002/01/18 09:42:58  vltsccm
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
// Revision 1.20  2001/11/14 08:44:17  vltsccm
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
// Revision 1.15  2001/07/12 12:38:18  vltsccm
// logging1.15
//
// Revision 1.14  2001/07/12 08:56:17  vltsccm
// logging1.14
//
// Revision 1.13  2001/07/12 07:08:24  vltsccm
// logging1.13
//
// Revision 1.12  2001/07/11 09:13:45  vltsccm
// logging1.12
//
// Revision 1.11  2001/07/11 09:13:44  vltsccm
// logging1.11
//
// Revision 1.10  2001/07/11 09:13:44  vltsccm
// logging1.10
//
// Revision 1.9  2001/07/11 09:13:44  vltsccm
// logging1.9
//
// Revision 1.8  2001/07/11 09:13:43  vltsccm
// logging1.8
//
// Revision 1.7  2001/07/11 09:13:43  vltsccm
// logging1.7
//
// Revision 1.6  2001/07/11 09:13:42  vltsccm
// logging1.6
//
// Revision 1.5  2001/07/11 09:13:42  vltsccm
// logging1.5
//
// Revision 1.4  2001/07/11 09:13:42  vltsccm
// logging1.4
//
// Revision 1.3  2001/07/11 09:13:41  vltsccm
// logging1.3
//
// Revision 1.2  2001/07/11 09:13:41  vltsccm
// logging1.2
//
// Revision 1.1  2001/07/11 09:13:40  vltsccm
// logging1.1
//
// Revision 1.0  2001/07/11 09:13:40  vltsccm
// logging1.0
//
//
// ************************************************************************
