<!-- ***************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2013
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

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:loggingts="Alma/ACSLogTS" xmlns:redirect="http://xml.apache.org/xalan/redirect" extension-element-prefixes="redirect">
        <xsl:output method="text" version="1.0" encoding="ASCII"/>
        <xsl:template match="/loggingts:LogDefinitionType">
	<xsl:variable name="logdefname">
		<xsl:value-of select="@name"/>
	</xsl:variable>
	<xsl:variable name="Prefix">
		<xsl:value-of select="@_prefix"/>
	</xsl:variable>
	<xsl:variable name="LogGroupDescription">
		<xsl:value-of select="@description"/>
	</xsl:variable>
        <xsl:for-each select="loggingts:LogDefinition">
	<xsl:variable name="FileName">
		<xsl:value-of select="$Prefix"/><xsl:text>/</xsl:text><xsl:value-of select="$logdefname"/><xsl:text>/</xsl:text><xsl:value-of select="@logName"/><xsl:text>.java</xsl:text>
	</xsl:variable>
	<redirect:write select="$FileName">
	<xsl:text>/*
* ALMA - Atacama Large Millimiter Array
* Copyright (c) ESO - European Southern Observatory, 2013
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
*/

/**
 * @author  nbarriga
 * @version $Id: LTS2Java.xslt,v 1.9 2012/03/06 19:16:56 tstaig Exp $
 * @since
 */

package </xsl:text><xsl:value-of select="$Prefix"/><xsl:text>.</xsl:text><xsl:value-of select="$logdefname"/>
	<xsl:text>;

import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.LinkedHashMap;
import java.util.Map;

import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.AcsLogRecord;
import alma.acs.logging.ts.TypesafeLogBase;

</xsl:text>
<xsl:text>
/**
 * Generated class that produces a type-safe log message, as configured in </xsl:text><xsl:value-of select="$logdefname"/><xsl:text>.xml.
 * &lt;p&gt;
 * The description for this log is: </xsl:text><xsl:value-of select="@description"/><xsl:text> &lt;br&gt;
 * The description for this group of logs is: </xsl:text><xsl:value-of select="$LogGroupDescription"/><xsl:text>
 */
public class </xsl:text>
        	<xsl:variable name="logName"><xsl:value-of select="@logName"/></xsl:variable>
		<xsl:value-of select="$logName"/><xsl:text> extends TypesafeLogBase {
	
	public static final AcsLogLevel level = AcsLogLevel.</xsl:text><xsl:value-of select="@priority"/><xsl:text>;
	public static final String audience = "</xsl:text><xsl:value-of select="@audience"/><xsl:text>";
	public static final String msg = "</xsl:text><xsl:value-of select="@shortDescription"/><xsl:text>";
	
	public </xsl:text><xsl:value-of select="$logName"/><xsl:text>(Logger logger) {
		this(logger, null, null);
	}

	public </xsl:text><xsl:value-of select="$logName"/><xsl:text>(Logger logger, String array, String antenna) {
		super(logger, "</xsl:text><xsl:value-of select="$logName"/><xsl:text>", level, audience, msg, array, antenna);
	}
	
	/**
	 * Convenience method for compact one-line logs.
	 */
	public static void log(Logger logger</xsl:text>
	<xsl:for-each select="loggingts:Member">
		<xsl:text>, </xsl:text>
			<xsl:choose>
                <xsl:when test='@type="string"'><xsl:text>String</xsl:text></xsl:when>
				<xsl:when test='@type="double"'><xsl:text>double</xsl:text></xsl:when>
                <xsl:when test='@type="long"'><xsl:text>long</xsl:text></xsl:when>
                <xsl:when test='@type="boolean"'><xsl:text>boolean</xsl:text></xsl:when>
			</xsl:choose>
		<xsl:text> </xsl:text><xsl:value-of select="@name"/>
	</xsl:for-each>
	<xsl:text>) {
		if (logger.isLoggable(level)) {	
			</xsl:text><xsl:value-of select="$logName"/><xsl:text> instance = new </xsl:text><xsl:value-of select="$logName"/><xsl:text>(logger);</xsl:text>
			<xsl:for-each select="loggingts:Member"><xsl:text>
			instance.set</xsl:text><xsl:value-of select="@name"/><xsl:text>(</xsl:text><xsl:value-of select="@name"/><xsl:text>);</xsl:text>
			</xsl:for-each><xsl:text>
			instance.log();
		}
	}
	
</xsl:text>
	<xsl:for-each select="loggingts:Member"><xsl:text>
	/**
	 * Sets log parameter </xsl:text><xsl:value-of select="@name"/><xsl:text>.
	 * @param </xsl:text><xsl:value-of select="@name"/><xsl:text>
	 *			</xsl:text><xsl:value-of select="@description"/><xsl:text>
	 */
	public </xsl:text><xsl:value-of select="$logName"/><xsl:text> set</xsl:text><xsl:value-of select="@name"/>
		<xsl:text>(</xsl:text>        
		<xsl:choose>
                <xsl:when test='@type="string"'>
                        <xsl:text>String</xsl:text>
                </xsl:when>
	        <xsl:when test='@type="double"'>
        		<xsl:text>Double</xsl:text>
              	</xsl:when>
                <xsl:when test='@type="long"'>
	        	<xsl:text>Long</xsl:text>
          	</xsl:when>
                <xsl:when test='@type="boolean"'>
	        	<xsl:text>Boolean</xsl:text>
          	</xsl:when>
        </xsl:choose>
	<xsl:text> value) {
		nameValue.put("</xsl:text><xsl:value-of select="@name"/><xsl:text>", value);
		return this;
	}</xsl:text>	
	</xsl:for-each>
<xsl:text>
}

</xsl:text>
</redirect:write>
</xsl:for-each>
</xsl:template>
</xsl:stylesheet>
