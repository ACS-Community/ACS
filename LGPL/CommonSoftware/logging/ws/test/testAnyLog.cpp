/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) UNSPECIFIED - FILL IN, 2005 
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
* "@(#) $Id: testAnyLog.cpp,v 1.2 2007/12/03 05:25:36 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran  2007-04-12  created
*/

#include "loggingLoggingProxy.h"
#include "logging.h"
#include <time.h>

static char *rcsId="@(#) $Id: testAnyLog.cpp,v 1.2 2007/12/03 05:25:36 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

CORBA::Any bin_record; 
CORBA::Any xml_record; 

double packXmlLogs(){ 
    //
    // format XML
    //
    
    ACE_CString xml((size_t)512);    // create buffer of 512 chars to improove performace (avoid reallocating)
    
    // source info
    xml = "<Info TimeStamp=\"2007-12-12\"";
    xml += " File=\"filename\"";
    xml += " Line=\"line\"";
    xml += " Routine=\"routine\"";

    xml += " Host=\"host\"";
    xml += " Process=\"process\"";
    xml += " Thread=\"thread\"";

    xml += " Context=\"Context\"";
	xml += " SourceObject=\"SourceObject\"";
    xml += " Audience=\"Audience\""; 
    xml += " StackId=\"Stackid\"";
    xml += " StackLevel=\"StackLevel\"";
	xml += " LogId=\"LogId\"";
    
	xml += " Uri=\"Uri\"";
    xml += "Priority=\"Priority\""; 
    xml += ">";
    
    xml += "<Data Name=\"name\"><![CDATA[Data]]></Data>";
    
	xml += "<![CDATA[";
	xml +="I'm a message";
	xml +="]]>";
    
    xml + "</Info>"; 
    clock_t start, end;
    start = clock();
    xml_record <<= xml.c_str();
    end = clock();
    double elapsed = ((double) (end - start)) / CLOCKS_PER_SEC;
    return elapsed;
}

double packBinLogs(){ 
    //
    // format XML
    //
    
	ACSLoggingLog::LogBinaryRecord *s_log = new ACSLoggingLog::LogBinaryRecord();
    ACE_CString xml((size_t)512);    // create buffer of 512 chars to improove performace (avoid reallocating)
    
    // source info
    s_log->type = AcsLogLevels::INFO_VAL;
    s_log->TimeStamp = "2007-12-12";
    s_log->File="filename";
    s_log->Line=21;
    s_log->Routine="routine";

    s_log->Host="host";
    s_log->Process="process";
    s_log->Thread="thread";

    s_log->LogContext="Context";
	s_log->SourceObject="SourceObject";
    s_log->Audience="Audience"; 
    s_log->StackId="Stackid";
    s_log->StackLevel=8;
	s_log->LogId="LogId";
    
	s_log->Uri="Uri";
    s_log->Priority=1;
    
    ACSLoggingLog::NameValue aux [2];
    aux[0].name = "name1";
    aux[0].value = "value1";

    aux[1].name = "name2";
    aux[1].value = "value2";
    int j;
    s_log->log_data = ACSLoggingLog::NameValueSeq(2);
    for (j=0;j<2;j++){
        int length = s_log->log_data.length(); 
        s_log->log_data.length(length+1);
        s_log->log_data[length] = aux[j];
    }
    s_log->MsgData = "I'm a message";


    clock_t start, end;
    start = clock();
    bin_record <<= *s_log;
    delete s_log;   
    end = clock();
    double elapsed = ((double) (end - start)) / CLOCKS_PER_SEC;
    return elapsed;
}

double unpackBinLogs(){

  const ACSLoggingLog::LogBinaryRecord *log;
    clock_t start, end;
    start = clock();
  bin_record >>= log;
    end = clock();
    //delete log;
    double elapsed = ((double) (end - start)) / CLOCKS_PER_SEC;
    return elapsed;
}

double unpackXmlLogs(){
    const char * xmlLog;
    clock_t start, end;
    start = clock();
    xml_record >>=xmlLog;
    end = clock();
    //delete xmlLog;
    double elapsed = ((double) (end - start)) / CLOCKS_PER_SEC; 
    return elapsed;
}

int main(int argc, char *argv[])
{
    int i,max;
    double totalPackTimeBin=0.0, totalPackTimeXml=0.0;
    double totalUnpackTimeBin=0.0, totalUnpackTimeXml=0.0;
    if(argc == 2) max = atoi(argv[1]);
    else{printf("ERROR!!");}
    printf("Running %d times each (xml and bin)\n",max);
    for(i=0;i<max;i++){ 
        totalPackTimeXml+=packXmlLogs();
        totalPackTimeBin+=packBinLogs();
        totalUnpackTimeXml+=unpackXmlLogs();
        totalUnpackTimeBin+=unpackBinLogs();
    }

    printf("Media pack bin=[%f] xml=[%f]\n",totalPackTimeBin,totalPackTimeXml);
    if(totalPackTimeXml>0)
        printf("Ratio pack bin/xml=[%f]\n\n",totalPackTimeBin/totalPackTimeXml);
    
    printf("Media unpack bin=[%f] xml=[%f]\n",totalUnpackTimeBin,totalUnpackTimeXml);
    if(totalUnpackTimeXml>0)
        printf("Ratio unpack bin/xml=[%f]\n\n",totalUnpackTimeBin/totalUnpackTimeXml);
    
    printf("Media total bin=[%f] xml=[%f]\n",totalPackTimeBin+totalUnpackTimeBin,totalPackTimeXml+totalUnpackTimeXml);
    if(totalPackTimeXml+totalUnpackTimeXml>0)
        printf("Ratio total bin/xml=[%f]\n",(totalPackTimeBin+totalUnpackTimeBin)/(totalPackTimeXml+totalUnpackTimeXml));
    
  return 0;
}

