#! /bin/ksh
#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: acsBUILDInstallReadme.sh,v 1.122 2003/01/28 16:37:10 vltsccm Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2002-12-11 Added ABEANS.LICENCE file
# psivera  2002-10-25 added creation of ACSROOT which was forgotten
# psivera  2002-10-24 both ERRFILE and LOGFILE are written
# psivera  2002-10-23 added case ACSROOT=INSTROOT
# almamgr  2001-05-28 Renamed module acsBUILD.
#

LOGFILE=INSTALL/pkginBuild.log
ERRFILE=INSTALL/pkginBuild.err

echo "Installing README files . . ."

cp acsBUILD/README .
cp acsBUILD/COPYING.LGPL .
cp acsBUILD/ABEANS.LICENSE ABEANS.LICENSE
cp acsBUILD/src/Makefile.acs Makefile
chmod +w README COPYING.LGPL ABEANS.LICENSE Makefile
exit 0
#
#___oOo___
