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
# "@(#) $Id: updateErrDefs.sh,v 1.2 2007/10/22 22:00:09 nbarriga Exp $"
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

if [ -z $1 ]||[ $1 = -h ]; then
    echo 'usage: updateErrDefs.sh [flag] dir';
    echo 'FLAGS:    -h      show this help';
    exit;
fi
DIR=$1

for file in $(find $DIR -name 'Makefile' -exec grep -H 'ACSERRDEF[ \t]*=[ \t]*[A-Za-z_]' {} \; | awk "/src\/Makefile/ {directory = sub(\"src/Makefile:ACSERRDEF\",\"idl/\",\$1); for (i =3; i <= NF; i++)print \$directory\$i\".xml\" }"); do
if [ -a $file ]; then
    echo "Updating file: $file(backup will be saved as $file.bak)";
    if [ $(grep -l -e "xsi:noNamespaceSchemaLocation=\"ACSError\.xsd\"" $file)X = X ]; then
        if [ $(grep -l -e "xsi:schemaLocation=\"Alma\/ACSError ACSError\.xsd\"" $file)X = X ]; then
            sed -i.bak "s/<Type /<Type xmlns=\"Alma\/ACSError\" xsi:schemaLocation=\"Alma\/ACSError ACSError\.xsd\" /g" $file;
            #echo "File $file updated, xsi:noNamespaceSchemaLocation statement not found";
        else
            echo "File $file could not be updated";
        fi
    else
        sed -i.bak "s/xsi:noNamespaceSchemaLocation=\"ACSError\.xsd\"/xmlns=\"Alma\/ACSError\" xsi:schemaLocation=\"Alma\/ACSError ACSError\.xsd\"/g" $file;
        #echo "File $file updated, xsi:noNamespaceSchemaLocation statement found";
    fi
fi
done;
#
# ___oOo___
