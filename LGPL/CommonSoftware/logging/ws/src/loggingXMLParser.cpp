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
* "@(#) $Id: loggingXMLParser.cpp,v 1.39 2007/09/28 09:22:14 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/07/08  created 
*/

#include <vltPort.h>
#include <loggingXMLParser.h>

 using namespace loggingXMLParser;

XMLElement *
XMLParser::parseString(const ACE_TCHAR * xml, bool skipValueParsing)
{

  // initialize mode
  int mode = XP_NONE;

  // current string (preallocate 1024 bytes of memory)
  ACE_CString str(size_t(1024));
  ACE_CString attributeName;

  // current XMLElement
  XMLElement * xmlElement = 0;

  // go and parse char by char
  for (const ACE_TCHAR *pos = xml; *pos; pos++)
    {
      switch (*pos)
	{
	case '<':
	  {
            //
            // start parsing XML
            //
            if (mode == XP_NONE)
	      {
		// set beginning of the element
		mode = ELEMENT_BEGIN_CHAR;

		// and create new XMLElement
		if (!xmlElement)
		  {
		    xmlElement = new XMLElement();
		    //xmlElement->setParent(0);
		  }

	      }

    
            //
            // we are in CDATA block
            //
            else if (mode == CDATA)
              str += *pos;

            //
            // start parsing XML
            //
            else if ((mode == ELEMENT_DATA) || (mode == CDATA_END))
	      {
	      //mode = ELEMENT_END_CHAR;

		// this is CDATA section
		if ((mode != CDATA) && ACE_OS::strncmp(pos, "<![CDATA[", 9) == 0)
		  {
		    mode = CDATA;
		    pos += 9;
		  }

                // is end of element data
                else //if (*(pos+1) == '/')
		  {
		    // skip all whitespaces
		    const ACE_TCHAR * t = pos+1;
		    while (*t && *t==' ' && *t=='\n' && *t=='\t' && *t=='\r')
			t++;
		    
		    // this is really end element char
		    // but is is right one
		    if (*t && *t=='/') 
		      {
		        t++;

		        // skip whitespaces
		        while (*t && *t==' ' && *t=='\n' && *t=='\t' && *t=='\r')
			  t++;

			if (*t)
			  {
			    ACE_CString endStr(size_t(20));
			    while (*t && *t!=' ' && *t!='>' && *t!='\n' && *t!='\t' && *t!='\r')
				{ endStr += *t; t++; }

			    if (endStr.length() && ACE_OS::strcmp(endStr.c_str(),xmlElement->name())==0)
			      {
				xmlElement->value(str.c_str());
				return xmlElement;
				/*
				mode = ELEMENT_END;
				str = "";
				break;
				*/
			      }
			  }
		      }
		  }

                // no, element has subelements
                //else
		//  {
                    // not implemented - read children....
		    str+=*pos;
		    //  }
	      }

            break;
	  }

	case '>':
	  {
            //
            // end of element name
            //
            if (mode == ELEMENT_NAME)
	      {
		xmlElement->name(str.c_str());

	        if (skipValueParsing)
		  return xmlElement;

		mode = ELEMENT_DATA;
		str = "";
	      }

            //
            // end of element's attributes
            //
            else if (mode == ATTRIBUTE_AREA)
	      {
	        if (skipValueParsing)
		  return xmlElement;

		mode = ELEMENT_DATA;
		str = "";
	      }

            //
            // end of element
            //
            else if (mode == ELEMENT_END)
	      {

		// TBD - compare element name matches, <Element>...</Element>
                           
		return xmlElement;
	      }
            
            //
            // we are in CDATA block
            //
            else if (mode == CDATA)
              str += *pos;

	    else if (mode == CDATA_END)
	      mode = ELEMENT_DATA;

	    // not supporting nested nodes
            else if (mode == ELEMENT_DATA)
              str += *pos;

            break;
	  }


	case ' '  :
	case '\n' :
	case '\r' :
	case '\t' :
	  {
	    
            //
            // we are reading data
            //
            if ((mode == ELEMENT_DATA) || (mode == CDATA) || (mode == ATTRIBUTE_VALUE))
              str += *pos;

            //
            // end of element name
            //
            else if (mode == ELEMENT_NAME)
	      {
		xmlElement->name(str.c_str());
		mode = ATTRIBUTE_AREA;
		str = "";
	      }

	    break;
	  }

	  /*
	case '!' :
	case '?' :
	  {
	    mode = SPECIAL_SECTION;
	    break;
	  }
	  */
/*
	case '[' :
	  {
	  //if (mode == SPECIAL_SECTION)
	  //    {
		// this is CDATA section
		if ((mode != CDATA) && (pos-xml)>=2 && ACE_OS::strncmp(pos-2, "<![CDATA[", 9) == 0)
		  {
		    str[str.length()-2] = 32;
		    mode = CDATA;
		    pos += 6;
		  }
		//    }
	    // append
	    else
	      str += *pos;
	    break;
	  }
*/
	case ']' :
	  {
	    // is end of CDATA section
	    if ((mode == CDATA) && (*(pos+1) == ']'))  
	      {
		mode = CDATA_END;
		pos++;
	      }
	    // append
	    else
	      str += *pos;

	    break;
	  }

	case '=' :
	  {
	    // end of attribute name
	    if (mode == ATTRIBUTE_NAME)
	      {
		attributeName = str.c_str();
		mode = ATTRIBUTE_VALUE_BEGIN;
		str = "";
	      }

	    // append
	    else //if (mode != SPECIAL_SECTION)
	      str += *pos;

	    break;
	  }

	case '"' :
	case '\'' :
	  {
	    //
	    // start of attribute value
	    //
	    if (mode == ATTRIBUTE_VALUE_BEGIN)
	      mode = ATTRIBUTE_VALUE;

	    //
	    // end of attribute value
	    //
	    else if (mode == ATTRIBUTE_VALUE)
	      {
		xmlElement->addAttribute(attributeName, str.c_str());
		mode = ATTRIBUTE_AREA;
		str = "";
		attributeName = "";
	      }

	    // append
	    else //if (mode != SPECIAL_SECTION)
	      str += *pos;

	    break;
	  }

	case '/' :
	  {
	    // end of attributes
	    if ((mode == ATTRIBUTE_AREA))
	      mode = ELEMENT_END;

	    // append
	    else if (/*(mode != SPECIAL_SECTION) &&*/ (mode != ELEMENT_NAME) && (mode != ELEMENT_END))
	      str += *pos;

	    break;
	  }

	default:
	  {
            // we are parsing test (values)
            // just append to the <str>
            if (mode == ELEMENT_BEGIN_CHAR)
              mode = ELEMENT_NAME;
            else if (mode == ATTRIBUTE_AREA)
              mode = ATTRIBUTE_NAME;
            
            if (/*(mode != SPECIAL_SECTION) &&*/ (mode != CDATA_END))
              str += *pos;

            break;
	  }


	}

    }

  // invalid (or not supported by this tiny parser) XML string
  // still return what we have
  if (xmlElement && xmlElement->name())
      return xmlElement;

  return 0;
}


int
XMLParser::parseElementType(const ACE_TCHAR * xml, ACE_CString &elementType)
{
  const char * startpos = ACE_OS::strchr(xml, '<');
  if (startpos == 0) 
    return 1;
  startpos++;

  const char * endpos = ACE_OS::strnchr(startpos, ' ', xml-startpos);
  if (endpos == 0 || endpos <= startpos)
    return 2;

  elementType.set(startpos, endpos-startpos, 1);

  return 0;
}



// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: loggingXMLParser.cpp,v $
// Revision 1.39  2007/09/28 09:22:14  cparedes
// Moving the BinToXml from loggingClient to loggingLoggingProxy because of
// the use in loggingLoggingProxy too.
//
// Revision 1.38  2006/09/01 02:20:55  cparedes
// small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
// Revision 1.37  2004/10/14 21:03:48  gchiozzi
// No change. Simply add a commet for the previous change.
// Fixed nasty bug with ACE_CString allocation.
// The wrong costructor was used producing well hidden memory allocation and
// corruption problems:
// -      ACE_CString strCmdLn((const char *)0, 512);
// +      ACE_CString strCmdLn(size_t(512));
//
// Revision 1.36  2004/10/14 20:46:17  gchiozzi
// Cleaned up logging messages:
// - uses now only ACS_* macros and not any more ACE_* macros
// - Removed all new line characters
// - fixed problem with ACS logging that was not putting proper new line
//   characters when logging messages on stdout BEFORE the logging itself
//   was initialised.
//
// Revision 1.35  2003/03/14 10:24:49  rgeorgie
// LGPL
//
// Revision 1.34  2003/03/10 14:29:29  bjeram
// changes according to the changes in TAO x.3
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
// Revision 1.26  2002/01/18 09:43:01  vltsccm
// logging1.26
//
// Revision 1.25  2002/01/16 10:41:31  vltsccm
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
// Revision 1.20  2001/11/14 08:44:20  vltsccm
// logging1.20
//
// Revision 1.19  2001/09/13 14:24:35  vltsccm
// logging1.19
//
// Revision 1.18  2001/08/13 07:04:58  vltsccm
// logging1.18
//
// Revision 1.17  2001/07/12 20:23:05  vltsccm
// logging1.17
//
// Revision 1.16  2001/07/12 13:26:01  vltsccm
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
// Revision 1.12  2001/07/11 09:14:02  vltsccm
// logging1.12
//
// Revision 1.11  2001/07/11 09:14:01  vltsccm
// logging1.11
//
// Revision 1.10  2001/07/11 09:14:00  vltsccm
// logging1.10
//
// Revision 1.9  2001/07/11 09:14:00  vltsccm
// logging1.9
//
// Revision 1.8  2001/07/11 09:14:00  vltsccm
// logging1.8
//
// Revision 1.7  2001/07/11 09:13:59  vltsccm
// logging1.7
//
// Revision 1.6  2001/07/11 09:13:59  vltsccm
// logging1.6
//
// Revision 1.5  2001/07/11 09:13:58  vltsccm
// logging1.5
//
// Revision 1.4  2001/07/11 09:13:58  vltsccm
// logging1.4
//
// Revision 1.3  2001/07/11 09:13:58  vltsccm
// logging1.3
//
// Revision 1.2  2001/07/11 09:13:57  vltsccm
// logging1.2
//
// Revision 1.1  2001/07/11 09:13:57  vltsccm
// logging1.1
//
// Revision 1.0  2001/07/11 09:13:57  vltsccm
// logging1.0
//
//
// ************************************************************************
