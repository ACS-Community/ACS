<!-- ***************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 ******************************************************************************* -->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:loggingts="Alma/ACSLogTS">
        <xsl:output method="text" version="1.0" encoding="ASCII"/>
        <xsl:template match="/loggingts:LogDefinitionType">
<xsl:text>/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) ESO - European Southern Observatory, 2011
* (in the framework of the ALMA collaboration).
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
* "@(#) $Id: LTS2Cpp.xslt,v 1.9 2012/03/06 19:16:56 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* nbarriga  2007-01-30  created
*/

#include "vltPort.h"

#include "</xsl:text><xsl:variable name="typeName"><xsl:value-of select="@name"/></xsl:variable>
<xsl:value-of select="$typeName"/>
<xsl:text>.h"

#include &lt;acsutilTimeStamp.h>
#include &lt;sstream>

using namespace </xsl:text><xsl:value-of select="$typeName"/><xsl:text>;

</xsl:text>
        <xsl:for-each select="loggingts:LogDefinition">
        	<xsl:variable name="logName"><xsl:value-of select="@logName"/></xsl:variable>
		<xsl:value-of select="$logName"/><xsl:text>::</xsl:text><xsl:value-of select="$logName"/><xsl:text>(string file, unsigned long line, string routine){
                        init(file, line, routine);
                }
    void </xsl:text><xsl:value-of select="$logName"/><xsl:text>::init(string file, unsigned long line, string routine){
	this->priority=Logging::ace2acsPriority(ACE_Log_Priority(LM_</xsl:text><xsl:value-of select="@priority"/><xsl:text>));
	this->file=file;
	this->line=line;
	this->routine=routine;
	this->name="</xsl:text>
		<xsl:value-of select="$logName"/><xsl:text>";
	this->audience="</xsl:text>
		<xsl:value-of select="@audience"/><xsl:text>";
	this->shortDescription="</xsl:text><xsl:value-of select="@shortDescription"/><xsl:text>";

}

</xsl:text>
		<xsl:value-of select="$logName"/><xsl:text>::</xsl:text><xsl:value-of select="$logName"/><xsl:text>(string file, unsigned long line, string routine, string array, string antenna){
        init(file, line, routine);
        this->array=array;
        this->antenna=antenna;
}        
</xsl:text>
		<xsl:value-of select="$logName"/><xsl:text>::~</xsl:text><xsl:value-of select="$logName"/><xsl:text>(){

}
void </xsl:text><xsl:value-of select="$logName"/><xsl:text>::setArray(string array){
        this->array=array;
}
void </xsl:text><xsl:value-of select="$logName"/><xsl:text>::setAntenna(string antenna){
        this->antenna=antenna;
}

string </xsl:text><xsl:value-of select="$logName"/><xsl:text>::getArray(){
        return array;
}
string </xsl:text><xsl:value-of select="$logName"/><xsl:text>::getAntenna(){
        return antenna;
}
void </xsl:text><xsl:value-of select="$logName"/><xsl:text>::log(){
        Logging::BaseLog::LogRecord lr;
        lr.priority=this->priority;
        lr.message=this->shortDescription;
        lr.file=this->file;
        lr.line=this->line;
        lr.method=this->routine;
        lr.timeStamp=getTimeStamp();
        LoggingProxy::AddData("logName",this->name.c_str());
        //LoggingProxy::AddData("audience",this->audience.c_str());
        LoggingProxy::audience(this->audience.c_str());
        if(this->array.length()!=0)LoggingProxy::array(this->array.c_str());
        if(this->antenna.length()!=0)LoggingProxy::antenna(this->antenna.c_str());
        for(unsigned int i=0;i&lt;members.length();i++){
                LoggingProxy::AddData(members[i].name.in(),members[i].value.in());
        }
        LoggingProxy::Flags(LM_SOURCE_INFO | LM_RUNTIME_CONTEXT);
        getLogger()->log(lr);
		LoggingProxy::audience(NULL);

}

</xsl:text>
		<xsl:for-each select="loggingts:Member">
		<xsl:text>void </xsl:text><xsl:value-of select="$logName"/><xsl:text>::set</xsl:text><xsl:variable name="memberName"><xsl:value-of select="@name"/></xsl:variable>
		<xsl:value-of select="$memberName"/>
		<xsl:text>(</xsl:text>
		<xsl:choose>
			<xsl:when test='@type="boolean"'>
				<xsl:text>bool</xsl:text>
			</xsl:when>
			<xsl:otherwise>
			<xsl:value-of select="@type"/>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text> value){
	ACSLoggingLog::NameValue nv;
	nv.name=CORBA::string_dup("</xsl:text><xsl:value-of select="$memberName"/><xsl:text>");
</xsl:text>
	<xsl:choose>
		<xsl:when test='@type="string"'>
			<xsl:text>	nv.value=CORBA::string_dup(value.c_str());</xsl:text>
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>	stringstream strstr;
	strstr&lt;&lt;value;
	string value_str=strstr.str();
	nv.value=CORBA::string_dup(value_str.c_str());</xsl:text>
		</xsl:otherwise>
	</xsl:choose>
	<xsl:text>
	members.length(members.length()+1);
	members[members.length()-1]=nv;

}

</xsl:text>
		</xsl:for-each>
	</xsl:for-each>
<xsl:text>

/*___oOo___*/
</xsl:text>
</xsl:template>
</xsl:stylesheet>
