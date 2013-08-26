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
<xsl:text>#ifndef _</xsl:text>
                <xsl:value-of select="@name"/>
                <xsl:text>_H_</xsl:text>
                <xsl:text>
#define _</xsl:text>
                <xsl:value-of select="@name"/>
                <xsl:text>_H_
/*******************************************************************************
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
* "@(#) $Id: LTS2H.xslt,v 1.7 2012/03/06 19:16:56 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* nbarriga  2007-01-30  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include &lt;string>
#include &lt;logging_idlS.h>
#include &lt;loggingACSLogger.h>
#include &lt;logging.h>
#include &lt;TypeSafeLog.h>

using namespace std;

namespace </xsl:text><xsl:value-of select="@name"/>
<xsl:text>{

</xsl:text>
        <xsl:for-each select="loggingts:LogDefinition">
		<xsl:text>class </xsl:text>
        	<xsl:variable name="logName"><xsl:value-of select="@logName"/></xsl:variable>
		<xsl:value-of select="$logName"/><xsl:text>:public Logging::TypeSafeLog{
	private:
                Logging::BaseLog::Priority priority;
                string file;
                unsigned long line;
                string routine;
                string name;
                string audience;
                string array;
                string antenna;
                string shortDescription;
                ACSLoggingLog::NameValueSeq members;
	protected:

	public:
		</xsl:text><xsl:value-of select="$logName"/><xsl:text>(string file,
                        unsigned long line,
                        string routine);
		</xsl:text><xsl:value-of select="$logName"/><xsl:text>(string file,
                        unsigned long line,
                        string routine,
                        string array,
                        string antenna);
                void init(string file, unsigned long line, string routine);
                virtual ~</xsl:text><xsl:value-of select="$logName"/><xsl:text>();
		void log();
                void setArray(string array);
                void setAntenna(string antenna);
                string getArray();
                string getAntenna();
</xsl:text>
		<xsl:for-each select="loggingts:Member">
		<xsl:text>		void set</xsl:text><xsl:value-of select="@name"/>
		<xsl:text>(</xsl:text>
		<xsl:choose>
			<xsl:when test='@type="boolean"'>
				<xsl:text>bool</xsl:text>
			</xsl:when>
			<xsl:otherwise>
			<xsl:value-of select="@type"/>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text> value);
</xsl:text> </xsl:for-each>
<xsl:text>
};

</xsl:text>
</xsl:for-each>
<xsl:text>
};

#endif
</xsl:text>
</xsl:template>
</xsl:stylesheet>
