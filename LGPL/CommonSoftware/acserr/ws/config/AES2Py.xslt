<!-- edited with XMLSPY v5 rel. 2 U (http://www.xmlspy.com) by Bogdan Jeram (E.S.O.) -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:output method="text" version="1.0" encoding="ASCII"/>
        <xsl:template match="/Type">
<xsl:text>#!/usr/bin/env python
# @(#) $Id: AES2Py.xslt,v 1.11 2006/01/06 23:25:19 dfugate Exp $
#
#    ALMA - Atacama Large Millimiter Array
#    (c) Associated Universities, Inc. Washington DC, USA,  2001
#    (c) European Southern Observatory, 2002
#    Copyright by ESO (in the framework of the ALMA collaboration)
#    and Cosylab 2002, All rights reserved
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
######################################################################
'''
Some form of custom documentation goes here...
'''
######################################################################
from   Acspy.Common.Err import ACSError
import ACSErr
import </xsl:text>
<xsl:value-of select="@name"/>
<xsl:text>
######################################################################
</xsl:text>
<!--  ******************************************** ErrorCode *************************************************************************************************************************** -->
        <xsl:for-each select="ErrorCode[not(@_suppressExceptionGeneration)]">
        <xsl:variable name="ClassName">
                                        <xsl:value-of select="@name"/><xsl:text>ExImpl</xsl:text>
                                </xsl:variable>
                                <xsl:text>class </xsl:text>
                                <xsl:value-of select="$ClassName"/>
                                <xsl:text>(</xsl:text>
                                <xsl:value-of select="../@name"/>
                                <xsl:text>.</xsl:text>
                                <xsl:value-of select="@name"/>
                                <xsl:text>Ex, ACSError):
    '''
    Some form of custom documentation goes here...
    '''
    #-----------------------------------------------------------------
    def __init__(self, 
                 et=ACSErr.</xsl:text>
                 <xsl:value-of select="../@name"/>
                 <xsl:text>, 
                 ec=</xsl:text>
                 <xsl:number value="position()-1"/>
                 <xsl:text>,
                 nvSeq = [],
                 exception = None,
                 create = 1):
        '''
        Some form of custom documentation goes here...
        '''
        self.shortDescription = "</xsl:text>
        <xsl:value-of select="@shortDescription"/>
        <xsl:text>"
        description = self.shortDescription
        ACSError.__init__(self,  
                          et,
                          ec,
                          exception,
                          description,
                          nvSeq,                          
                          create)
        </xsl:text>
        <xsl:value-of select="../@name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="@name"/>
        <xsl:text>Ex.__init__(self, self.errorTrace)
        return
######################################################################
</xsl:text>
</xsl:for-each>

<!--  ******************************************** Completion *********************************** -->
        <xsl:for-each select="Code | ErrorCode">
        <xsl:variable name="ClassName"><xsl:value-of select="@name"/><xsl:text>CompletionImpl</xsl:text></xsl:variable>
                                <xsl:text>class </xsl:text>
                                <xsl:value-of select="$ClassName"/>
                                <xsl:text>(ACSErr.Completion, ACSError):
    '''
    Some form of custom documentation goes here...
    '''
    #-----------------------------------------------------------------
    def __init__(self, 
                 et=ACSErr.</xsl:text>
                 <xsl:value-of select="../@name"/>
                 <xsl:text>, 
                 ec=</xsl:text>
                 <xsl:number value="position()-1"/>
                 <xsl:text>,
                 nvSeq = [],
                 exception = None,
                 create = 1):
        '''
        Some form of custom documentation goes here...
        '''
        self.shortDescription = "</xsl:text>
        <xsl:value-of select="@shortDescription"/>
        <xsl:text>"
        description = self.shortDescription
        ACSError.__init__(self,
                          et,
                          ec,
                          exception,
                          description,
                          nvSeq,
                          create)

        #Create the CORBA object
        ACSErr.Completion.__init__(self,
                                   self.getTimeStamp(),
                                   self.getErrorType(),
                                   self.getErrorCode(),
                                   [self.errorTrace])
        return
######################################################################
</xsl:text>
</xsl:for-each>


<!--  ******************************************** Custom Python Testing ********************************************************************** -->
<xsl:text>if __name__ == "__main__":

</xsl:text>


<xsl:for-each select="ErrorCode[not(@_suppressExceptionGeneration)]">
        <xsl:variable name="ClassName">
                                        <xsl:value-of select="@name"/><xsl:text>ExImpl</xsl:text>
                                </xsl:variable>

<xsl:text>    try:
        raise </xsl:text><xsl:value-of select="$ClassName"/><xsl:text>
</xsl:text>
<xsl:text>    except </xsl:text><xsl:value-of select="../@name"/><xsl:text>.</xsl:text><xsl:value-of select="@name"/><xsl:text>Ex, e:
        print "Caught the correct type of exception:", e
        g = </xsl:text><xsl:value-of select="$ClassName"/><xsl:text>(exception=e)
        g.Print()
    except Exception, e:
        print "Caught the wrong type of exception:", e

</xsl:text>

</xsl:for-each>

<xsl:for-each select="ErrorCode | Code">
        <xsl:variable name="ClassName"><xsl:value-of select="@name"/><xsl:text>CompletionImpl</xsl:text></xsl:variable>

<xsl:text>
    joe = </xsl:text><xsl:value-of select="$ClassName"/><xsl:text>()
    joe.Print()

</xsl:text>

</xsl:for-each>
<xsl:text>
    print
</xsl:text>

</xsl:template>
</xsl:stylesheet>
