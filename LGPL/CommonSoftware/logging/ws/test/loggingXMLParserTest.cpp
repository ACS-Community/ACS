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
* "@(#) $Id: loggingXMLParserTest.cpp,v 1.36 2006/09/01 02:20:55 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2002/09/22  created 
*/

#include <vltPort.h>
#include <logging.h>
#include <loggingXMLParser.h>

 using namespace loggingXMLParser;

const ACE_TCHAR * testXMLs[] = {
    "",
    "error",
    "<error",
    "<entry/>",
    "<entry />",
    "<entry></entry>",
    "< entry   ><  /   entry  >",
    "< entry   ><  /entry     >",
    "<entry>some value here</entry>",
    "<entry>some value here!</entry>",
    "<entry>some value \n\t and also here</entry>",
    "<entry>these < and > should not be here at all !!! CDATA block should be used</entry>",
    "<entry>preData <![CDATA[this is a CDATA section and all < and > are allowed here]]> postData</entry>",
    "<entry>some \"quoted\" 'values' '::'\"\"\";;{{'!!!...32879532&232*422*34*2</entry>",
    "<Info TimeStamp=\"2002-09-22T16:48:52.363\" Routine=\"routineName\" Host=\"csl01\" Process=\"loggingXMLParserTest\" "
    "Thread=\"main\" Context=\"\">This is typical log XML.</Info>",
    "<node>preData1<subnode/>preData2<subnode attribute=\"\"/>innerData<subnode>with value</subnode>postData</node>"
};



void run_test(bool skipValueParsing)
{
    ACS_SHORT_LOG((LM_INFO, "----- Running test with skipValueParsing==%d -----", skipValueParsing));
 
    int len = sizeof(testXMLs)/sizeof(ACE_TCHAR*);
    for (int i=0; i<len; i++)
      {
	XMLElement * element = XMLParser::parseString(testXMLs[i], skipValueParsing);
	if (element)
	    {
	    // succesfully parsed XML

	    ACE_CString atts = ACE_CString("\t\tElement name : '")+element->name()+"'";
	    if (element->value()[0])
		atts += ACE_CString("\n\t\tElement value: '")+element->value()+"'";

	    XMLElement::ATTRIBUTE_HASH_MAP_ENTRY *entry;
	    XMLElement::ATTRIBUTE_HASH_MAP_ITER iter = element->getAttributesIterator();
	    for (; iter.next(entry)!=0; iter.advance ())
	      {
		atts += ACE_CString("\n\t\tAttribute: '")+entry->ext_id_.c_str()+"' = '"+entry->int_id_.c_str()+"'";
	      }

	    ACS_SHORT_LOG((LM_INFO, "Parsed XML test case #%d: '%s'.\n"
			   "%s", 
			   i, testXMLs[i], atts.c_str()));

      	    delete element;
	  }
	else
	  {
	    // failed to parse XML
	    ACS_SHORT_LOG((LM_INFO, "Failed to parse XML test case #%d: '%s'.", i, testXMLs[i]));
	  }
      }
}

int main(int argc, char* argv[])
{
    LoggingProxy * logger = new LoggingProxy(0, 0, 31);
    if (logger)
      {
	LoggingProxy::init(logger);

	// initialize ACE logger instance
	ACE_TCHAR hostname[33];
	ACE_OS::hostname (hostname, sizeof(hostname));
	ACE_Log_Msg::instance()->local_host(hostname);

	LoggingProxy::ProcessName(argv[0]);
	LoggingProxy::ThreadName("main");
      }
    else
      {
	ACS_SHORT_LOG((LM_INFO, "Failed to initialize logging."));
	return 1;
      }

    run_test(true);

    run_test(false);

    LoggingProxy::done();
    if (logger)
	delete logger;

    return 0;
}
