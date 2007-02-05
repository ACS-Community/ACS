<!-- created by Nicolas Barriga-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:loggingts="Alma/ACSLogTS">
        <xsl:output method="text" version="1.0" encoding="ASCII"/>
        <xsl:template match="/loggingts:LogDefinitionType">
<xsl:text>/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2007
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
* "@(#) $Id: LTS2Cpp.xslt,v 1.3 2007/02/05 13:01:54 nbarriga Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* nbarriga  2007-01-30  created
*/

#include "vltPort.h"

#include "</xsl:text><xsl:variable name="typeName"><xsl:value-of select="@name"/></xsl:variable>
<xsl:value-of select="$typeName"/>
<xsl:text>.h"

#include &lt;acstimeTimeUtil.h>

using namespace </xsl:text><xsl:value-of select="$typeName"/><xsl:text>;

</xsl:text>
        <xsl:for-each select="loggingts:LogDefinition">
        	<xsl:variable name="logName"><xsl:value-of select="@logName"/></xsl:variable>
		<xsl:value-of select="$logName"/><xsl:text>::</xsl:text><xsl:value-of select="$logName"/><xsl:text>(string file, unsigned long line, string routine){
	this->priority=Logging::ace2acsPriority(ACE_Log_Priority(LM_</xsl:text><xsl:value-of select="@priority"/><xsl:text>));
	this->file=file;
	this->line=line;
	this->routine=routine;
	this->name="</xsl:text>
		<xsl:value-of select="$logName"/><xsl:text>";
	this->shortDescription="</xsl:text><xsl:value-of select="@shortDescription"/><xsl:text>";

}

</xsl:text>
		<xsl:value-of select="$logName"/><xsl:text>::~</xsl:text><xsl:value-of select="$logName"/><xsl:text>(){

}

void </xsl:text><xsl:value-of select="$logName"/><xsl:text>::log(){
        Logging::BaseLog::LogRecord lr;
        lr.priority=this->priority;
        lr.message=this->shortDescription;
        lr.file=this->file;
        lr.line=this->line;
        lr.method=this->routine;
        lr.timeStamp=baci::getTimeStamp();
        LoggingProxy::AddData("logName",this->name.c_str());
        for(unsigned int i=0;i&lt;members.length();i++){
                LoggingProxy::AddData(members[i].name.in(),members[i].value.in());
        }
        LoggingProxy::Flags(LM_SOURCE_INFO | LM_RUNTIME_CONTEXT);
        getLogger()->log(lr);

}

</xsl:text>
		<xsl:for-each select="loggingts:Member">
		<xsl:text>void </xsl:text><xsl:value-of select="$logName"/><xsl:text>::set</xsl:text><xsl:variable name="memberName"><xsl:value-of select="@name"/></xsl:variable>
		<xsl:value-of select="$memberName"/>
		<xsl:text>(string value){

	ACSLog::NVPair nv;
	nv.name=CORBA::string_dup("</xsl:text><xsl:value-of select="$memberName"/><xsl:text>");
	nv.value=CORBA::string_dup(value.c_str());
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
