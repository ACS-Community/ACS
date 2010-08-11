#! /bin/sh
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2009 
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
# "@(#) $Id: testPython.sh,v 1.2 2010/08/11 04:15:04 agrimstrup Exp $"
#
# who         when      what
# --------    --------  ----------------------------------------------
# agrimstrup  2009-10-01  created
#

export TEST_MODE="YES"
. ../src/python

# The test suite
testGetArgsCOpt()
{
  get_args -c "foo bar"
  assertEquals "Bad Flag" "-c " "$PYINTRARGS"
  assertEquals "Bad Command" "foo bar" "$PYCMDSTR"
}

testGetArgsCOptNoCmd()
{
  get_args -c
  assertEquals "Bad Flag" "-h" "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsCOptAfterCommand()
{
  get_args -c "foo bar" -d
  assertEquals "Bad Flag" "-c " "$PYINTRARGS"
  assertEquals "Bad Command" "foo bar" "$PYCMDSTR"
}

testGetArgsCOptOtherOpt()
{
  get_args -d -c "foo bar"
  assertEquals "Bad Flag" "-d -c " "$PYINTRARGS"
  assertEquals "Bad Command" "foo bar" "$PYCMDSTR"
}

testGetArgsMOpt()
{
  get_args -m foo
  assertEquals "Bad Flag" "-m foo " "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsMOptNoCmd()
{
  get_args -m
  assertEquals "Bad Flag" "-h" "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsMOptAfterCommand()
{
  get_args -m foo -d
  assertEquals "Bad Flag" "-m foo " "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsMOptOtherOpt()
{
  get_args -d -m foo
  assertEquals "Bad Flag" "-d -m foo " "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsDOpt()
{
  get_args -d
  assertEquals "Bad Flag" "-d " "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsDOptOAfter()
{
  get_args -d -O
  assertEquals "Bad Flag" "-O -d " "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsDOptOOAfter()
{
  get_args -d -OO
  assertEquals "Bad Flag" "-OO -d " "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsDOptOOBefore()
{
  get_args -OO -d 
  assertEquals "Bad Flag" "-OO -d " "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsvOpt()
{
  get_args -v 
  assertEquals "Bad Flag" "-v " "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsMultivOpt()
{
  get_args -vvvv 
  assertEquals "Bad Flag" "-vvvv " "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsVOptAfter()
{
  get_args -V -d
  assertEquals "Bad Flag" "-V " "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsQOpt()
{
  get_args -Q foo
  assertEquals "Bad Flag" "-Q foo " "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsMultiQOpt()
{
  get_args -Q foo -Q bar
  assertEquals "Bad Flag" "-Q foo -Q bar " "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testGetArgsLongOpt()
{
  get_args --version
  assertEquals "Bad Flag" "--version" "$PYINTRARGS"
  assertEquals "Bad Command" "" "$PYCMDSTR"
}

testSelectIntrNoCASA()
{
  if [ ! -z "$CASA_ROOT" ]
  then
     CASA_SAVE=$CASA_ROOT
     unset CASA_ROOT
  fi

  SAVPYPATH=$PYTHONPATH
  SAVLIBPATH=$LD_LIBRARY_PATH
  select_intr
  assertEquals "Wrong Interpreter" "exec $PYTHON_ROOT/bin/python" "$PYTHONSHELL"
  assertEquals "TCL_LIBRARY set" "" "$TCL_LIBRARY"
  assertEquals "PYTHONPATH modified" "$SAVPYPATH" "$PYTHONPATH"
  assertEquals "LD_LIBRARY_PATH modified" "$SAVLIBPATH" "$LD_LIBRARY_PATH"

  if [ -z "$CASA_ROOT" ]
  then
     CASA_ROOT=$CASA_SAVE
     export CASA_ROOT
  fi
}

testSelectIntrACS()
{
  export USE_ACS_PYTHON="YES"
  SAVPYPATH=$PYTHONPATH
  SAVLIBPATH=$LD_LIBRARY_PATH
  select_intr
  assertEquals "Wrong Interpreter" "exec $PYTHON_ROOT/bin/python" "$PYTHONSHELL"
  assertEquals "TCL_LIBRARY set" "" "$TCL_LIBRARY"
  assertEquals "PYTHONPATH modified" "$SAVPYPATH" "$PYTHONPATH"
  assertEquals "LD_LIBRARY_PATH modified" "$SAVLIBPATH" "$LD_LIBRARY_PATH"
  unset USE_ACS_PYTHON
}

testSelectIntrCASA()
{
  if [ -z "$CASA_ROOT" ]
  then
     CASA_ROOT=/path/to/casa
     export CASA_ROOT
  fi

  SAVPYPATH=$PYTHONPATH
  SAVLIBPATH=$LD_LIBRARY_PATH
  select_intr
  assertEquals "Wrong Interpreter" "exec $CASA_ROOT/lib/casapy/bin/python" "$PYTHONSHELL"
  assertEquals "TCL_LIBRARY not set" "$CASA_ROOT/share/tcl8.4" "$TCL_LIBRARY"
  assertNotEquals "PYTHONPATH modified" "$SAVPYPATH" "$PYTHONPATH"
  assertNotEquals "LD_LIBRARY_PATH modified" "$SAVLIBPATH" "$LD_LIBRARY_PATH"

  unset TCL_LIBRARY
  if [ "$CASA_ROOT" = "/path/to/casa" ]
  then
     unset CASA_ROOT
  fi
}

testStartIntrNoScript()
{
   PYTHONSHELL=echo
   start_intr
}

testStartIntrCmdArgs()
{
   PYTHONSHELL=echo
   PYINTRARGS='-V'
   start_intr
}

testStartIntrCmdStr()
{
   PYTHONSHELL=echo
   PYINTRARGS='-c'
   PYCMDSTR="print 'Hello'"
   start_intr
}

testStartIntrLocalScript()
{
   touch bonzo.py
   PYTHONSHELL=echo
   start_intr bonzo.py
   rm bonzo.py
}

testStartIntrNoLocalScript()
{
   PYTHONSHELL=echo
   start_intr foo.py
}

testStartIntr()
{
   PYTHONSHELL=echo
   start_intr acsutilCASAPathInsert PYTHONPATH
}

tearDown()
{
   unset PYINTRARGS
   unset PYCMDSTR
   OPTIND=1
}

# Run the test engine
. shunit2

unset TEST_MODE
#
# ___oOo___
