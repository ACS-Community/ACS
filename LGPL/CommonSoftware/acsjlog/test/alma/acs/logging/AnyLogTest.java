/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.logging;
import java.util.Date;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;

import alma.ACSLoggingLog.LogBinaryRecord;
import alma.ACSLoggingLog.LogBinaryRecordHelper;
import alma.ACSLoggingLog.NameValue;
import alma.acs.logging.level.AcsLogLevelDefinition;

class AnyLogTest{

static Any bin_record;
static Any xml_record;
 public static void main(String args[]){

    int i,max=0;
    ORB orb = ORB.init(args, null); 
    bin_record = orb.create_any();
    xml_record = orb.create_any();
    double totalPackTimeBin=0.0, totalPackTimeXml=0.0;
    double totalUnpackTimeBin=0.0, totalUnpackTimeXml=0.0;
    
    if(args.length == 1) max = Integer.parseInt(args[0]);
    else{System.out.println("ERROR!!");}
    System.out.println("Running "+args[0]+" times each (xml and bin)");
    for(i=0;i<max;i++){ 
        totalPackTimeXml+=packXmlLogs();
        totalPackTimeBin+=packBinLogs();
        totalUnpackTimeXml+=unpackXmlLogs();
        totalUnpackTimeBin+=unpackBinLogs();
    }

    System.out.println("Total time pack bin=["+totalPackTimeBin+"] xml=["+totalPackTimeXml+"]");
    if(totalPackTimeXml>0)
        System.out.println("Ratio pack bin/xml=["+totalPackTimeBin/totalPackTimeXml+"]\n");
    
    System.out.println("Total time unpack bin=["+totalUnpackTimeBin+"] xml=["+totalUnpackTimeXml+"]");
    if(totalUnpackTimeXml>0)
        System.out.println("Ratio unpack bin/xml=["+(totalUnpackTimeBin/totalUnpackTimeXml)+"]\n");
    
    System.out.println("Total time total bin=["+(totalPackTimeBin+totalUnpackTimeBin)+"] xml=["+(totalPackTimeXml+totalUnpackTimeXml)+"]");
    if(totalPackTimeXml+totalUnpackTimeXml>0)
        System.out.println("Ratio total bin/xml=["+((totalPackTimeBin+totalUnpackTimeBin)/(totalPackTimeXml+totalUnpackTimeXml))+"]");
    
}
 static double packXmlLogs(){ 
    String xml;
    
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
    
    xml += "</Info>"; 
    
    Date start = new Date();
    xml_record.insert_string(xml);
    Date end = new Date();
    double elapsed = ((double) (end.getTime() - start.getTime()));
    return elapsed;
}

 static double packBinLogs(){ 
    
	LogBinaryRecord s_log = new LogBinaryRecord();
    
    // source info
    s_log.type = (short) AcsLogLevelDefinition.INFO.value;
    s_log.TimeStamp = "2007-12-12";
    s_log.File="filename";
    s_log.Line=21;
    s_log.Routine="routine";

    s_log.Host="host";
    s_log.Process="process";
    s_log.Thread="thread";

    s_log.LogContext="Context";
	s_log.SourceObject="SourceObject";
    s_log.Audience="Audience"; 
    s_log.StackId="Stackid";
    s_log.StackLevel=8;
	s_log.LogId="LogId";
    
	s_log.Uri="Uri";
    s_log.Priority=1;
    
    NameValue[] aux = new NameValue [2];
    aux[0] = new NameValue("name1", "value1");
    aux[1] = new NameValue("name2", "value2");
    int j;
    s_log.log_data = new NameValue[2];
    s_log.attributes = new NameValue[0];
    for (j=0;j<2;j++){
        s_log.log_data[j] = aux[j];
    }

    s_log.MsgData = "I'm a message";
    Date start = new Date();
    LogBinaryRecordHelper.insert(bin_record,s_log);
    Date end = new Date();
    double elapsed = ((double) (end.getTime() - start.getTime()));
    return elapsed;

}

static double unpackBinLogs(){
    Date start = new Date();
    LogBinaryRecord logRecord = LogBinaryRecordHelper.extract(bin_record);
    Date end = new Date();
    double elapsed = ((double) (end.getTime() - start.getTime()));
    return elapsed;
}

static double unpackXmlLogs(){
    Date start = new Date();
    String xmlLog = xml_record.extract_string();
    Date end = new Date();
    double elapsed = ((double) (end.getTime() - start.getTime()));
    return elapsed;
}

}
