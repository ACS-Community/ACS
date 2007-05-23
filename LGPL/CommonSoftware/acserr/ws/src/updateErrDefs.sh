#! /bin/sh
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2007
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: updateErrDefs.sh,v 1.1 2007/05/23 08:55:56 nbarriga Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# nbarriga  2007-05-23  created
#

#************************************************************************
#   NAME: updateErrDefs.sh
#
#   SYNOPSIS: Small script to update your error definition XML files.
#
#   DESCRIPTION
#
#   FILES
#
#   ENVIRONMENT
#
#   RETURN VALUES
#
#   CAUTIONS
#
#   EXAMPLES:   updateErrDefs.sh .      #process file only in the current working directory
#               updateErrDefs.sh -r .   #recursively search files
#               updateErrDefs.sh -h     #show help
#
#   SEE ALSO
#
#   BUGS
#
#------------------------------------------------------------------------
#

RECUR=''
ASTERISK='/*'
if [ $1 = -h ]&&[ $2 = -r ]||[ $1 = -r ]&&[ $2 = -h ]||[ $1 = -rh ]||[ $1 = -hr ]; then
    echo 'sorry, only one flag at a time.(-h for help)';
    exit;
elif [ -z $1 ]||[ $1 = -h ]; then
    echo 'usage: updateErrDefs.sh [flag] dir';
    echo 'FLAGS:    -h      show this help';
    echo '          -r      recursively search files';
    exit;
elif [ $1 = -r ]; then
    RECUR=$1;
    DIR=$2;
    ASTERISK='';
else
    DIR=$1;
fi
for file in $(grep -l $RECUR -e "xsi:noNamespaceSchemaLocation=\"ACSError\.xsd\"" $DIR$ASTERISK | grep "xml$");do
    echo "Updating file: $file(backup saved as $file.bak)";
    sed -i.bak "s/xsi:noNamespaceSchemaLocation=\"ACSError\.xsd\"/xmlns=\"Alma\/ACSError\" xsi:schemaLocation=\"Alma\/ACSError ACSError\.xsd\"/g" $file;
done;


#
# ___oOo___
