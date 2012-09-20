#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2012.
# Copyright by ESO (in the framework of the ALMA collaboration).
# All rights reserved.
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
# "@(#) $Id: acsSearchPath.py,v 1.1 2012/09/20 12:09:55 eallaert Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2012-09-18  created
#

#************************************************************************
#   NAME
#   acsSearchPath.py - create a search path from various components
# 
#   SYNOPSIS
#   acsSearchPath -p PATH -a PATH -v SUBDIR1 SUBDIR2 ....
#
#   whereby the options and arguments are:
#   -p: prepend PATH to the result. Default: empty
#   -a: append PATH to the result. Default: empty
#   -v: verbose output. Default: terse
#   SUBDIRx: sub-directory to append to each individual directory of
#       $INTROOT, $INTLIST and $ACSROOT. Default: empty
#
#   Note that this can also be invoked as a function, with the following
#   synopsis:
#     acsSearchPath(prepend, subdirList, append)
#
#   whereby the arguments are all optional, with defaults as above.
#
#   DESCRIPTION
#   acsSearchPath will print a path to stdout that consists of the
#   following components, in the order as indicated:
#
#   1. Each directory in the prepend-path, SUBDIRx will be appended to
#      it. This is repeated for all SUBDIRx specified, before the
#      next directory in the prepend-path is dealt with.
#   2. ../SUBDIRx, for each SUBDIRx, in the order as they are
#      specified. If the list of SUBDIRs is empty, ".." will be
#      appended to the resulting path.
#      This part is only inserted if the environment variable MODPATH
#      is set to 1.
#   3. $INTROOT/SUBDIRx, for each SUBDIRx, in the order as they are
#      specified. If the list of SUBDIRs is empty, the "pure" $INTROOT
#      will be appended to the resulting path
#   4. Each directory in $INTLIST is dealt with as $INTROOT (i.e.
#      appending each SUBDIRx as specified, if applicable)
#   5. $ACSROOT is also dealt with as $INTROOT
#   6. Each directory in the prepend-path, SUBDIRx will be appended to
#      it. This is repeated for all SUBDIRx specified, before the
#      next directory in the prepend-path is dealt with.
#
#   This path-string construction removes empty path-components ("::"),
#   repeated slashes ("//") within path-components (although these in
#   the end do not matter for Linux) and trailing slashes at the end
#   of path-components - if applicable.
#
#   The pathlist-separator used is system dependent (":" on Linux,
#   ";" on MS-Windows). The path separator, i.e. delimiting directories
#   from subdirectories, is "/".#   
#
#   FILES
#
#   ENVIRONMENT
#   Requires python 2.6 or higher.
#   The environment variables INTROOT, INTLIST and ACSROOT are used as
#   central part for path construction, but this function will not fail
#   if any/all of them do not exist.
#
#   The environment variable MODPATH determines wheter or not the
#   module's root (i.e. "..") will be included before INTROOT.
#
#   RETURN VALUES
#   The function acsSearchPath() returns the constructed path as a string;
#   If this is run as a utility, the path will be printed to stdout.
#
#   CAUTIONS
#   There is no verification on the effective existence of the directories
#   pointed to by INTROOT, INTLIST, ACSROOT and pre-/append paths, nor
#   of the components of the resulting search-patch (with the SUBDIRs
#   included) - they are all basically handled as strings, not directories.
#
#   As a consequence, if $INTROOT or $ACSROOT have (by mistake) a trailing
#   colon, and SUBDIRs are appended, this will lead to confusing and
#   probably undesired results - see the last example here below.
#
#   A similar situation can occur if SUBDIRx includes (again by mistake)
#   leading or trailing whitespace or colons - these arguments are
#   treated as ordinary strings, without any character checking.  
#
#   In any case, this is the same behaviour as e.g. the login scripts.
#
#   Note that the path-separator "/" is used, independent of the
#   operating system. Using backslashes under MS-Windows (within e.g.
#   ACSROOT settings) may lead to unexpected results.
#
#   EXAMPLES
#   me> export INTROOT=/introot/myIntroot
#   me> export INTLIST=/introot/herIntroot:/introot/hisIntroot
#   me> export ACSROOT=/alma/ACS15/ACSSW
#   me> unset MODPATH
#   me>
#   me> acsSearchPath
#   /introot/myIntroot:/introot/herIntroot:/introot/hisIntroot:/alma/ACS15/ACSSW
#   me>
#   me> acsSearchPath lib
#   /introot/myIntroot/lib:/introot/herIntroot/lib:/introot/hisIntroot/lib:/alma/ACS15/ACSSW/lib
#   me>
#   me> acsSearchPath -p /first:/second/ lib1 lib2
#   /first/lib1:/first/lib2:/second/lib1:/second/lib2:/introot/myIntroot/lib1:/introot/myIntroot/lib2:/introot/herIntroot/lib1:/introot/herIntroot/lib2:/introot/hisIntroot/lib1:/introot/hisIntroot/lib2:/alma/ACS15/ACSSW/lib1:/alma/ACS15/ACSSW/lib2
#   me>
#   me> unset INTLIST
#   me> export MODPATH=1
#   me> myPath = `acsSearchPath bin`
#   me> echo $myPath
#   ../bin:/introot/myIntroot/bin:/alma/ACS15/ACSSW/bin
#   me>
#   me> # CAUTION: Appending a ":" to $INTROOT is a mistake!
#   me> set INTROOT=/introot/myIntroot:
#   me> unset MODPATH
#   me> myPath = "/anotherPath"`acsSearchPath bin`
#   me> # Notice the ":" between $INTROOT and /bin
#   me> echo $myPath
#   /anotherPath:/introot/myIntroot:/bin:/alma/ACS15/ACSSW/bin
#   
#   SEE ALSO
#
#   BUGS
#
#------------------------------------------------------------------------
#
##import sys
import os
import re;  # regular expressions

def acsSearchPath(prepend="", subdirs=[], append=""):
    dirList = []

    # os.pathsep is the opsys-dependent component separator used in $PATH etc (":" on unix)
    ps = os.pathsep
    
    # First the prepend
    for dir in prepend.split(ps):
        if (len(dir) > 0):
            ##if (not os.path.exists(dir)):
            ##    print >> sys.stderr, "WARNING: prepend dir \"" + dir + "\" does not exist on this host."
            dirList.append(dir)

    # The module-root dir should only be appended if MODPATH is set to 1
    try:
        if (os.environ['MODPATH'] == "1"):
            # os.pardir is the opsys-dependent parent-dir representation (usually "..")
            dirList.append(os.pardir)
    except KeyError:
        pass

    # Now deal with INTROOT-INTLIST-ACSROOT
    try:
        ##introot = os.environ['INTROOT']
        ##if (not os.path.exists(introot)):
        ##    print >> sys.stderr, "WARNING: INTROOT dir \"" + introot + "\" does not exist on this host."
        dirList.append(os.environ['INTROOT'])
    except KeyError:
        ##print >> sys.stderr, "INTROOT not set"
        pass

    try:
        for dir in os.environ['INTLIST'].split(ps):
            if (len(dir) > 0):
                ##if (not os.path.exists(dir)):
                ##    print >> sys.stderr, "WARNING: INTLIST dir \"" + dir + "\" does not exist on this host."
                dirList.append(dir)
    except KeyError:
        ##print >> sys.stderr, "INTLIST not set"
        pass

    try:
        ##acsroot = os.environ['ACSROOT']
        ##if (not os.path.exists(acsroot)):
        ##    print >> sys.stderr, "WARNING: ACSROOT dir \"" + acsroot + "\" does not exist on this host."
        dirList.append( os.environ['ACSROOT'])
    except KeyError:
        ##print >> sys.stderr, "ACSROOT not set"
        pass

    # Finally the append
    for dir in append.split(ps):
        if (len(dir) > 0):
            ##if (not os.path.exists(dir)):
            ##    print >> sys.stderr, "WARNING: append dir \"" + dir + "\" does not exist on this host."
            dirList.append(dir)

    # Now for this intermediate result, append the subdirs,
    # and put the result into a typical PATH-list string.
    # Don't worry about trailing or double slashes etc - they will be
    # cleaned up at the end.
    path = ""
    for dir in dirList:
        if (len(subdirs) == 0):
            path += dir + ps
            
        else:
            for subdir in subdirs:
                path += dir + "/" + subdir + ps

    if (len(path) > 0):
        # clean-up: remove double colons, double slashes, slash before colon, trailing colon
        path = re.sub (r'//+', '/', path)
        path = re.sub (ps+ps+r'+|/'+ps+'+', ps, path)
        # last char could still be a single path-separator or slash
        if (path[-1] == ps or path[-1] == "/"):
            path = path[:-1]

    return path

if __name__ == "__main__":

    import optparse
    
    parser = optparse.OptionParser(usage="%prog -p PATH -a PATH SUBDIR1 SUBDIR2 ...")
    parser.add_option("-p", "--prepend", dest = "prepend", type = "str", default = "", help = "path to prepend to result.", metavar = "PATH")
    parser.add_option("-a", "--append", dest = "append", type = "str", default = "", help = "path to append to result.", metavar = "PATH")
    parser.add_option("-v", "--verbose", dest = "verbose", action = "store_true", help = "show execution time and run-string")

    (options, subdirs) = parser.parse_args()
    # having an empty list of subdirs is the same as a single empty subdir
    if (len(subdirs) == 1 and len(subdirs[0].strip()) ==0 ):
        subdirs = []
        
    if (options.verbose):
        import datetime
        import sys

        runstring = sys.argv[0]
        for i in range(1,len(sys.argv)) :
            runstring += " " + sys.argv[i]

            print "\nRunstring:\n    " + runstring + "\n\n%s\n\n" % str(datetime.datetime.now())
            print "Search-path: "

            # In case the output is re-directed to a file (e.g. by tat), these prints   
            # may get intermixed with the output from pyunit, due to buffering. To      
            # avoid that, do a flush.                                                   
            sys.stdout.flush()

    print acsSearchPath(options.prepend, subdirs, options.append)
