<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<!-- define output -->
	<xsl:output method="html" encoding="iso-8859-1"/>
	<!--

  ********************************************************************
  *
  * Template matches /
  *
  * Main template initiating the transform
  *
  *********************************************************************

-->
	<xsl:template match="/">
		<!--xsl:text disable-output-escaping="yes">&lt;!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
	</xsl:text-->
		<xsl:comment>
	Generated from XML using XSLT : DO NOT EDIT
	</xsl:comment>
		<html>
			<xsl:apply-templates/>
		</html>
	</xsl:template>
	<!--

  ********************************************************************
  *
  * Template matches document
  *
  * Layout the main structure of the document
  *
  *********************************************************************

-->
	<xsl:template match="document">
		<xsl:variable name="location">
			<xsl:choose>
				<xsl:when test="@location"><xsl:value-of select="@location"/></xsl:when>
				<xsl:otherwise><xsl:value-of select="'../'"/></xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<xsl:variable name="images" select="concat($location,'images/')"/>
		<head>
			<xsl:apply-templates select="htmlHeader"/>
			<xsl:element name="SCRIPT">
				<xsl:attribute name="Language">JavaScript</xsl:attribute>
				<xsl:attribute name="src"><xsl:value-of select="concat($location,'addon/nav.js')"/></xsl:attribute>
			</xsl:element>
			<xsl:element name="link">
				<xsl:attribute name="rel">stylesheet</xsl:attribute>
				<xsl:attribute name="type">text/css</xsl:attribute>
				<xsl:attribute name="href"><xsl:value-of select="concat($location,'addon/stylesheet.css')"/></xsl:attribute>
				<xsl:attribute name="title">Style</xsl:attribute>
			</xsl:element>
		</head>
		<body bgcolor="#FFFFFF">
			<A NAME="TOP"/>
			<xsl:call-template name="documentHeader">
				<xsl:with-param name="images" select="$images"/>
			</xsl:call-template>
			<xsl:if test="customHeader">
			<xsl:copy-of select="customHeader/node()"/>
		    </xsl:if>
			<br/>
			<table cellpadding="10" width="100%">
			<xsl:if test="not(head/title/@visible = 'false')">
				<tr>
					<td id="PageTitle">
						<xsl:value-of select="head/title"/>
						<IMG src="{concat($images,'graydot.gif')}" HEIGHT="1" WIDTH="100%"/>
						<br/>
					</td>
				</tr>
				</xsl:if>
				<tr>
					<td id="CellLeft">
						<xsl:for-each select="section">
							<xsl:call-template name="section">
								<xsl:with-param name="hTagNumber" select="1"/>
								<xsl:with-param name="images" select="$images"/>
							</xsl:call-template>
						</xsl:for-each>
					</td>
				</tr>
			</table>
			<xsl:comment>
				<xsl:apply-templates select="head"/>
			</xsl:comment>
			<xsl:call-template name="documentFooter">
				<xsl:with-param name="location" select="$location"/>
				<xsl:with-param name="images" select="$images"/>
			</xsl:call-template>
		</body>
	</xsl:template>
	<!--

  ********************************************************************
  *
  * Template matches documentHeader
  *
  * Layout the header of the document
  *
  *********************************************************************

-->
	<xsl:template name="documentHeader">
		<xsl:param name="images"/>
		<TABLE cellSpacing="0" cellPadding="0" width="100%" border="0">
			<TR>
				<TD width="100%" ID="HeaderBar">
					<A href="http://www.cern.ch" target="_top">
						<IMG alt="CERN Homepage" src="{concat($images,'CERN.gif')}" border="0" name="CERN" onMouseOver="msover1('CERN','{concat($images,'CERNON.gif')}');" onMouseOut="msover1('CERN','{concat($images,'CERN.gif')}');"/>
					</A>
					<IMG alt="separator" src="{concat($images,'separator.gif')}" border="0"/>
					<A href="http://cern.ch/ab-div/" target="_top">
						<IMG ALT="AB Div" src="{concat($images,'AB.gif')}" border="0" name="AB" onMouseOver="msover1('AB','{concat($images,'ABON.gif')}');" onMouseOut="msover1('AB','{concat($images,'AB.gif')}');"/>
					</A>
					<A href="http://cern.ch/ab-div-co/" target="_top">
						<IMG ALT="AB-CO" src="{concat($images,'CO.gif')}" border="0" name="ABCO" onMouseOver="msover2('AB','{concat($images,'ABON.gif')}', 'ABCO','{concat($images,'COON.gif')}');" onMouseOut="msover2('AB','{concat($images,'AB.gif')}', 'ABCO','{concat($images,'CO.gif')}');"/>
					</A>
					<A href="http://cern.ch/ab-div-co-ap/" target="_top">
						<IMG ALT="AB/CO/AP" src="{concat($images,'AP.gif')}" border="0" name="ABCOAP" onMouseOver="msover3('AB','{concat($images,'ABON.gif')}','ABCO','{concat($images,'COON.gif')}','ABCOAP','{concat($images,'APON.gif')}');" onMouseOut="msover3('AB','{concat($images,'AB.gif')}','ABCO','{concat($images,'CO.gif')}','ABCOAP','{concat($images,'AP.gif')}');"/>
					</A>
					<IMG ALT="separator" src="{concat($images,'separator.gif')}" border="0"/>
					<A href="http://proj-gp.web.cern.ch/proj-gp/index.html" target="_top">
						<IMG alt="GP" src="{concat($images,'GP.gif')}" border="0" name="GP" onMouseOver="msover1('GP','{concat($images,'GPON.gif')}');" onMouseOut="msover1('GP','{concat($images,'GP.gif')}');"/>
					</A>
					<IMG src="{concat($images,'homepages.gif')}"/>
				</TD>
			</TR>
		</TABLE>
	</xsl:template>
	<!--

  ********************************************************************
  *
  * Template matches documentFooter
  *
  * Layout the footer of the document
  *
  *********************************************************************

-->
	<xsl:template name="documentFooter">
		<xsl:param name="location"/>
		<xsl:param name="images"/>
		<BR/>
		<BR/>
		<A href="#TOP">
			<IMG src="{concat($images,'top.gif')}" alt="Go to top of page" HSPACE="3" border="0"/>
		</A>
		<table border="0" CELLSPACING="0" CELLPADDING="2" WIDTH="100%">
			<tr>
				<td ID="FooterCERN">
	     &#160;<a href="http://www.cern.ch/CERN/Divisions/DSU/Legal/Notices/Copyright.html" style="color:#FFFFFF;">Copyright CERN</a>
				</td>
				<td ID="FooterTimeStamp">
					<script LANGUAGE="Javascript" src="{concat($location,'addon/timestamp.js')}"/>&#160;
	     <a href="{concat('http://consult.cern.ch/xwho/people/',head/author/@ccid)}" style="color:#FFFFFF;">
						<xsl:value-of select="head/author/@firstname"/>&#160;<xsl:value-of select="head/author/@lastname"/>
					</a>&#160;<a href="{concat('mailto:',head/author/@firstname,'.',head/author/@lastname,'@cern.ch')}">
						<img src="{concat($images,'email.gif')}" border="0"/>
					</a>
				</td>
			</tr>
		</table>
	</xsl:template>
	<!--

  ********************************************************************
  *
  * Template matches head
  *
  * Layout the head information of the document
  *
  *********************************************************************

-->
	<xsl:template match="head">
		<table border="0" cellpadding="5">
			<tbody>
				<tr>
					<td colspan="2">
						<hr/>
					</td>
				</tr>
				<tr>
					<td class="headerTitle">Title</td>
					<td class="headerText">
						<xsl:value-of select="title"/>
					</td>
				</tr>
				<tr>
					<td class="headerTitle">Section</td>
					<td class="headerText">
						<xsl:value-of select="section"/>
					</td>
				</tr>
				<tr>
					<td class="headerTitle">Author</td>
					<td class="headerText">
						<a href="{concat('mailto:',author/@firstname,'.',author/@lastname,'@cern.ch')}">
							<xsl:value-of select="concat(author/@firstname,' ',author/@lastname)"/>
						</a>
					</td>
				</tr>
				<tr>
					<td class="headerTitle">Reviewer(s)</td>
					<td class="headerText">
						<xsl:copy-of select="reviewers/node()"/>
					</td>
				</tr>
				<tr>
					<td class="headerTitle">Current Version</td>
					<td class="headerText">
						<xsl:copy-of select="currentVersion/node()"/>
					</td>
				</tr>
				<tr>
					<td class="headerTitle">History</td>
					<td class="headerText">
						<xsl:copy-of select="history/node()"/>
					</td>
				</tr>
			</tbody>
		</table>
	</xsl:template>
	<!--

  ********************************************************************
  *
  * Template matches section
  *
  * The template display the content of a section.
  *
  *********************************************************************

-->
	<xsl:template name="section">
		<xsl:param name="hTagNumber"/>
		<xsl:param name="parentParagrapheNumber"/>
		<xsl:param name="parentNumbering"/>
		<xsl:param name="images"/>
		<xsl:variable name="paragrapheNumber">
			<xsl:choose>
				<xsl:when test="$parentParagrapheNumber">
					<xsl:value-of select="concat($parentParagrapheNumber,position(),'.')"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="concat(position(),'.')"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<xsl:variable name="numbering">
			<xsl:choose>
				<xsl:when test="@numbering">
					<xsl:value-of select="@numbering"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="$parentNumbering"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<!--
    <xsl:if test="@indenting">
    <table border="0" cellpadding="0" cellspacing="0">
    <tr><td colspan="2">&#160;</td></tr>
    <tr>
    <td>&#160;&#160;&#160;</td>
    </xsl:if>
    -->
		<!-- if an anchor is defined... add it -->
		<xsl:if test="anchor">
			<a name="{anchor}"/>
		</xsl:if>
		<!-- if a title is defined... print it ! -->
		<xsl:if test="title">
			<xsl:element name="{concat('h',$hTagNumber)}">
				<a href="#TOP">
					<img src="{concat($images,'top.gif')}" alt="Go to top of page" hspace="3" border="0"/>
				</a>
				<xsl:if test="$numbering and $numbering = 'true'">
					<xsl:value-of select="$paragrapheNumber"/>&#160;
	    </xsl:if>
				<xsl:copy-of select="title/node()"/>
			</xsl:element>
		</xsl:if>
		<!-- output content of this section -->
		<xsl:apply-templates select="description | codeDescription"/>
		<!-- output sub-sections -->
		<xsl:for-each select="section">
			<xsl:call-template name="section">
				<xsl:with-param name="hTagNumber" select="$hTagNumber+1"/>
				<xsl:with-param name="parentParagrapheNumber" select="$paragrapheNumber"/>
				<xsl:with-param name="parentNumbering" select="$numbering"/>
				<xsl:with-param name="images" select="$images"/>
			</xsl:call-template>
		</xsl:for-each>
		<!--
    <xsl:if test="@indenting">
    </td>
    </tr>
    </table>
    </xsl:if>
    -->
	</xsl:template>
	<xsl:template match="description">
		<xsl:copy-of select="child::node()"/>
	</xsl:template>
	<!--

  ********************************************************************
  *
  * Template matches code
  *
  *********************************************************************

-->
	<xsl:template match="codeDescription">
		<table border="0" cellpadding="4" cellspacing="0">
			<xsl:if test="title">
				<tr>
					<td colspan="2" bgcolor="#E4EBFE">
						<b>
							<xsl:copy-of select="title/node()"/>
						</b>
					</td>
				</tr>
			</xsl:if>
			<xsl:if test="comment">
				<tr>
					<td bgcolor="#E4EBFE" valign="top">&#160;&#160;</td>
					<td>
						<xsl:copy-of select="comment/node()"/>
					</td>
				</tr>
			</xsl:if>
			<tr>
				<td bgcolor="#E4EBFE" valign="top">&#160;&#160;</td>
				<td valign="top">
					<pre>
						<xsl:copy-of select="code/node()"/>
					</pre>
				</td>
			</tr>
		</table>
		<br/>
	</xsl:template>
	<!--

  ********************************************************************
  *
  * Template matches header
  *
  * The template output the custom value for the header.
  *
  *********************************************************************

-->
	<xsl:template match="htmlHeader">
		<meta name="Author" content="{head/authors}"/>
		<title>
			<xsl:value-of select="head/title"/>"</title>
	</xsl:template>
</xsl:stylesheet>

