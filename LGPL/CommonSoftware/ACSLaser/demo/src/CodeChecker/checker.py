#*******************************************************************************
# ALMA - Atacama Large Millimeter Array
# Copyright (c) ESO - European Southern Observatory, 2011
# (in the framework of the ALMA collaboration).
# All rights reserved.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#*******************************************************************************
import glob, os.path, filecmp, os

#########################################
### VARIABLE DEFINITIONS
########################################

#The html file generated
html = "<HMTL><HEAD></HEAD><BODY>"
html+='<H1 align="center">Comparison between ACS and Laser Alarm system code</H1><HR>'

#The ACS and Laser folders
ACSDir = "/diska/home/almadev/workspace/ACSLaser"
LaserDir="/diska/home/almadev/workspace/LaserAlarmSystem"

# The total number of modified java files and misalignements (files present in only one system)
GlobalStatistics = { "JFilesChanged": 0, "Misalignements": 0 }

# The command line for diff and grep
diffCmd = '/usr/bin/diff -E -b -w -B -r -q -I ".*System\.out.*" -I ".*System\.err.*" -I Revision -I Date -I Author -I Id '
grepCmd = '/bin/grep -v CVS|/bin/grep -v Makefile|/bin/grep -v cvsignore|/bin/grep -v purify '

# The dictionary with the name of the modules and their path
# in ACS and Laser
Modules = {}

# Get the ACS modules list
ACSModules = glob.glob( ACSDir+"/*" )

# Get the Laser modules list
LaserModules = [LaserDir+"/gp-openide"]
LaserModules = LaserModules + glob.glob( LaserDir+"/laser/*" )

#########################################
### FUNCTIONS
########################################

def fileStatistic( path, pattern ):
    """Calculate some statistics about the files terminating with pattern
       in the path directory
       Returns a dictionary with the following keys: 
        totNumFiles
        totSize
        avgSize (float)
        totLines
        avgLines
        shortestFileName    
        shortestFileSize
        longestFileName
        longestFileSize
        linesInShortestFile
        linesInLongestFile
        minLinesNumber
        minLinesFileName
        maxLinesNumber
        maxLinesFileName"""
    count = 0
    totSize = 0
    avgSize = 0.0
    minSize = 0
    minSizeName = ""
    maxSize = 0
    maxSizeName = ""
    totLines = 0
    minLines = 0
    minLinesName = ""
    maxLines = 0
    maxLinesName = ""
    avgLines = 0.0
    ret = {}
    
    for root, dirs, files in os.walk( path ):
        for f in files:
            if ( f.endswith( pattern ) ):
                count+=1
                totSize+=os.path.getsize( os.path.join( root, f ) )
                fi = open( os.path.join( root, f ) )
                lines = fi.readlines()
                totLines += len( lines )
                fi.close()
                # Get the min lines
                if minLines>len( lines ) or minLines==0:
                    minLines = len( lines )
                    minLinesName = os.path.join( root, f )
                # Get the max lines
                if maxLines<len( lines ):
                    maxLines = len( lines )
                    maxLinesName = os.path.join( root, f )
                # Check the max size
                if os.path.getsize( os.path.join( root, f ) )>maxSize:
                    maxSize=os.path.getsize( os.path.join( root, f ) )
                    maxSizeName=os.path.join( root, f )
                    linesInLongestFile = len( lines )
                # Check the min size
                if os.path.getsize( os.path.join( root, f ) )<minSize or minSize==0:
                    minSize=os.path.getsize( os.path.join( root, f ) )
                    minSizeName=os.path.join( root, f )
                    linesInShortestFile = len( lines )
        if 'CVS' in dirs:
            dirs.remove( 'CVS' )  # don't visit CVS directories
    avgSize = totSize/count
    avgLines = totLines/count
    # Store the key, value pairs
    ret["totNumFiles"]=count
    ret["totSize"]=totSize
    ret["avgSize"]=avgSize
    ret["totLines"] = totLines
    ret["avgLines"]=avgLines
    ret["shortestFileName"]=minSizeName
    ret["shortestFileSize"]=minSize
    ret["longestFileName"]=maxSizeName
    ret["longestFileSize"]=maxSize
    ret["linesInShortestFile"]=linesInShortestFile
    ret["linesInLongestFile"]=linesInLongestFile
    ret["minLinesNumber"] = minLines
    ret["minLinesFileName"] = minLinesName
    ret["maxLinesNumber"] = maxLines
    ret["maxLinesFileName"] = maxLinesName
    return ret

def generateStatisticTable( acsData, laserData ):
    """Generate a table with the dictionaries data from ACS and Laser"""
    table = '<TABLE border="1" align="center">'
    table+= '<CAPTION><EM>Statistics of java files</EM></CAPTION>'
    table+='<TR><TH> <TH>ACS<TH>Laser'
    table+='<TR><TH>Number of files<TD>%d<TD>%d' % ( acsData["totNumFiles"], laserData["totNumFiles"] )
    table+='<TR><TH>Total size in bytes<TD>%d<TD>%d' % ( acsData["totSize"], laserData["totSize"] )
    table+='<TR><TH>Average size in bytes<TD>%.1f<TD>%.1f' % ( acsData["avgSize"], laserData["avgSize"] )
    table+='<TR><TH>Longest file size<TD>%d<BR><FONT size="-1">%s</FONT><TD>%d<BR><FONT size="-1">%s</FONT>' % \
        ( acsData["longestFileSize"], \
        acsData["longestFileName"].replace( ACSDir, "ACSLaser" ), \
        laserData["longestFileSize"], \
        laserData["longestFileName"].replace( LaserDir, "LaserAlarmSystem" ) )
    table+='<TR><TH>Shortest file size<TD>%d<BR><FONT size="-1">%s</FONT><TD>%d<BR><FONT size="-1">%s</FONT>' % \
        ( acsData["shortestFileSize"], \
        acsData["shortestFileName"].replace( ACSDir, "ACSLaser" ), \
        laserData["shortestFileSize"], \
        laserData["shortestFileName"].replace( LaserDir, "LaserAlarmSystem" ) )
    table+='<TR><TH>Tot. num. of lines<TD>%d<TD>%d' % ( acsData["totLines"], laserData["totLines"] )
    table+='<TR><TH>Average num. of lines<TD>%.1f<TD>%.1f' % ( acsData["avgLines"], laserData["avgLines"] )
    table+='<TR><TH>Longest file (lines)<TD>%d<BR><FONT size="-1">%s</FONT><TD>%d<BR><FONT size="-1">%s</FONT>' % \
        ( acsData["maxLinesNumber"], \
        acsData["maxLinesFileName"].replace( ACSDir, "ACSLaser" ), \
        laserData["maxLinesNumber"], \
        laserData["maxLinesFileName"].replace( LaserDir, "LaserAlarmSystem" ) )
    table+='<TR><TH>Shortest file (lines)<TD>%d<BR><FONT size="-1">%s</FONT><TD>%d<BR><FONT size="-1">%s</FONT>' % \
        ( acsData["minLinesNumber"], \
        acsData["minLinesFileName"].replace( ACSDir, "ACSLaser" ), \
        laserData["minLinesNumber"], \
        laserData["minLinesFileName"].replace( LaserDir, "LaserAlarmSystem" ) )
    table+= '</TABLE>'
    return table

def compareSystems(globStats ): 
    """Scans the directory of the two modules looking for differences
    Return the html string describing those differences"""
    totJChanged = 0
    totMis = 0 
    str=""
    for k in Modules.keys():
        str+='<H3>Comparison report for module <EM>'+k+'</EM></H3>'
        currentACSMod = Modules[k][0]
        currentLaserMod=Modules[k][1]
        if currentACSMod=='':
            str+='<P>'+k+' is a Laser module but NOT an ACS module: nothing to compare'
        elif currentLaserMod=='':
            str+='<P>'+k+' is an ACS module but NOT a Laser module: nothing to compare'
        else:
            #cmp = filecmp.dircmp(Modules[k][0],Modules[k][1],['RCS', 'CVS', 'tags'], [os.curdir, os.pardir,'Makefile','.purify','.cvsignore'])
            #cmp.report_full_closure()
            os.system( diffCmd+currentACSMod+" "+currentLaserMod+"|"+grepCmd+" >"+k+".diff" )
            # Read the file to generate a readable html
            fi = open( k+".diff", 'r' )
            lines = fi.readlines()
            fi.close()
            modifiedJavaInThisModule = 0
            misalignedInThisModule = 0
            # I use 4 lists to store the messages from diff (to present them
            # orderd)
            onlyList = []
            onlyJList = []
            differJList = []
            differList = []
            othersList = []
            for line in lines:
                if line.startswith( 'Only' ):
                    if ( line.find( ".java" )!=-1 ):
                        misalignedInThisModule+=1
                        totMis+=1
                        onlyJList.append( line )
                    else:
                        onlyList.append( line )
                elif line.endswith( 'differ\n' ):
                    if ( line.find( ".java " )!=-1 ):
                        modifiedJavaInThisModule+=1
                        totJChanged+=1
                        differJList.append( line )
                    else:
                        differList.append( line )
                else:
                    othersList.append( line )
            # Write the reports on the string to return
            if len( differJList )>0:
                str+='<P><EM>%d</EM> java files modified:' % len( differJList )
                str+='<UL compact>'
                for item in differJList:
                    str+='<LI><FONT color="red">'+beautifyDifferStr(item)+'</FONT>'
                str+='</UL>'
            if len( differList )>0:
                str+='<P><EM>%d</EM> other files modified:' % len( differList )
                str+='<UL compact>'
                for item in differList:
                    str+='<LI>'+beautifyDifferStr(item)
                str+='</UL>'
            if len( onlyJList )>0:
                str+='<P><EM>%d</EM> java files misalignements (java files present in only one system)' % len( onlyJList )
                str+='<UL compact>'
                for item in onlyJList:
                    str+='<LI><FONT color="blue">'+beautifyOnlyStr(item)+'</FONT>'
                str+='</UL>'
            if len(onlyList)>0:
                str+='<P><EM>%d</EM> other misalignements (files present in only one system)' % len( onlyList )
                str+='<UL compact>'
                for item in onlyList:
                    str+='<LI>'+beautifyOnlyStr(item)
                str+='</UL>'
            if len(othersList)>0:
                str+='<P>Other messages:'
                str+='<UL compact>'
                for item in othersList:
                    str+='<LI>'+item
                str+='</UL>'
            # Store some values for statistics
            globStats["JFilesChanged"]=totJChanged
            globStats["Misalignements"]=totMis
    return str

def beautifyDifferStr(difStr):
    """Gives a better look to the string returned by diff"""
    items=difStr.split(' ')
    items[1]=items[1].replace( ACSDir, "<B><I>ACSLaser</I></B>" )
    items[3]=items[3].replace( LaserDir, "<B><I>LaserAlarmSystem</I></B>" )
    return items[1]+' <FONT color="black"><--></FONT> '+items[3]

def beautifyOnlyStr(onlyStr):
    items=onlyStr.split(' ')
    items[2]=items[2].replace( ACSDir, "<B><I>ACSLaser</I></B>" )
    items[2]=items[2].replace( LaserDir, "<B>LaserAlarmSystem</B>" )
    items[3]='<B><I>'+items[3]+'</I></B>'
    print items
    retStr = ''
    for str in items:
        retStr = retStr+str+' '
    return retStr

#########################################
### MAIN
########################################

# Build a dictionary with the name of each module and a list with the ACS and the Laser path
# It scans the ACS directory so we have to add Laser modules
# The key is the name of the module, the value is a couple [acspath,laserpath] i.e.
# { moduleName, [acspath,laserpath] }
for acsmod in ACSModules:
    # split the string to get the name of the module
    if not os.path.isdir( acsmod ):
        continue
    words = acsmod.split( '/' )
    acsmodName = words[len( words )-1]
    if acsmodName=='CVS':
        continue
    # Look for the same module in Laser
    lasermodname=''
    for laserMod in LaserModules:
        if laserMod.endswith( acsmodName ):
            lasermodname=laserMod
            break
    # Store  the value in the dictionary
    # It also add the specific path into each module
    if lasermodname!='':
        lasermodname=lasermodname+"/src/java"
    Modules[acsmodName]=[acsmod+"/src", lasermodname]

# There are also Laser modules that were not imported in ACS?
for lasermod in LaserModules:
    # split the string to get the name of the module
    if ( not os.path.isdir( lasermod ) ) or lasermod.endswith( 'CVS' ):
        continue
    words = lasermod.split( '/' )
    lasermodName = words[len( words )-1]
    if not Modules.has_key( lasermodName ):
        Modules[lasermodName]=['', lasermod]
        
#Write some output for the modules

html+='<HR><H2>Modules comparison</H2>'    
html+='<p>%d modules definition found in both ACS and Laser' % len( Modules )

html+='<P>Common modules'
html+='<UL>'
keys = Modules.keys()
for k in keys:
    if Modules[k][1]!='' and Modules[k][0]!='':
        html+='<LI>'+k
html+='</UL>'

html+='<P>Modules defined only in ACS (i.e. added to the Laser source)'
html+='<UL>'
keys = Modules.keys()
for k in keys:
    if Modules[k][1]=='':
        html+='<LI>'+k
html+='</UL>'

html+='<P>Modules defined only in Laser (i.e. not ported/needed by ACS)'
html+='<UL>'        
keys = Modules.keys()
for k in keys:
    if Modules[k][0]=='':
        html+='<LI>'+k
html+='</UL>'

html+='<HR>'

# Generate a diff file and the report for each module
html+=compareSystems(GlobalStatistics )
        
html+='<HR>'

html+='<H2>Some number</H2>'
html+='<P><EM>%d</EM> total java files modified' % GlobalStatistics["JFilesChanged"]
            
html+='<P><EM>%d</EM> total misalignements (java files present in only one system)' % GlobalStatistics["Misalignements"]
acsStatistics = fileStatistic( ACSDir, ".java" )
laserStatistics = fileStatistic( LaserDir, ".java" )
html+= generateStatisticTable(acsStatistics , laserStatistics)

# Write the html
fo = file( "out.html", 'w' )
fo.write( html )
fo.flush()
fo.close()
        