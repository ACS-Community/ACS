#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2008
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
# "@(#) $Id: testComponent.py,v 1.4 2011/03/01 23:04:13 javarias Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# arne  2008-06-13  created
#

import os
import signal
import subprocess
import pexpect
from time import sleep

def start_container(cname, ctype):
    """Common container start function"""
    try:
        subprocess.check_call(["acsutilAwaitContainerStart", ctype, cname])
    except Exception, e:
        print e

def stop_container(cname):
    """Common container stop function"""
    try:
        subprocess.check_call(["acsStopContainer", cname])
    except Exception, e:
        print e

def find_worker_pid(cname):
    """Return the pid of the actual container process"""

    #The PID of the top level caller is recorded in the file
    pidfile = "%s/ACS_INSTANCE.0/pids/%s_PID" % (os.environ['ACS_TMP'], cname)
    mpid = subprocess.Popen(['cat', pidfile], stdout=subprocess.PIPE).communicate()[0].strip()

    #Walk down the process tree until you have a process with no children
    try:
        while True:
            wpid = mpid
            mpid = subprocess.Popen(['ps', '--no-heading', '--ppid', mpid], stdout=subprocess.PIPE).communicate()[0].split()[0]
    except IndexError:
        pass

    # The last process is the worker
    return wpid

print "Starting Clients..."
clients = {}
for cmd in ['pyClient', 'cppClient', 'javaClient']:
    clients[cmd] = pexpect.spawn(cmd,searchwindowsize=200)
    clients[cmd].expect("Ready")
    print "%s: %s" % (cmd, clients[cmd].after)
print ""

for c,t in [("javaContainer", "--java"),("cppContainer", "--cpp"),("pyContainer", "--py")]:
    print "Starting %s..." % c
    start_container(c, t)
print ""

print "Acquiring Components..."
for cmd in clients:
    clients[cmd].sendline("Load HELLOWORLD_JAVA HELLOWORLD_CPP HELLOWORLD_PY")
    sleep(5)
    clients[cmd].expect("Load Complete: .*")
    print "%s: %s" % (cmd, clients[cmd].after.strip())
print ""

print "Invoking Component methods..."
for cmd in clients:
    clients[cmd].sendline("Call")
    sleep(1)
    clients[cmd].expect('Call .*')
    print clients[cmd].after
    print "%s: %s" % (cmd, clients[cmd].after.strip())
print ""

for c,t in [("javaContainer", "--java"),("cppContainer", "--cpp"),("pyContainer", "--py")]:
    print "Killing %s..." % c
    os.kill(int(find_worker_pid(c)),signal.SIGKILL)
    print "Restarting %s..." % c
    start_container(c, t)
print ""

sleep(10)
print "Invoking Component methods after kill..."
for cmd in clients:
    clients[cmd].sendline("Call")
    sleep(2)
    clients[cmd].expect(['Call .*',pexpect.EOF])
    print "%s: %s" % (cmd, clients[cmd].after.strip())
print ""

print "Retry invoking Component methods..."
for cmd in clients:
    clients[cmd].sendline("Call")
    sleep(1)
    clients[cmd].expect(['Call .*',pexpect.EOF])
    print "%s: %s" % (cmd, clients[cmd].after.strip())
print ""


for c,t in [("javaContainer", "--java"),("cppContainer", "--cpp"),("pyContainer", "--py")]:
    print "Killing %s..." % c
    os.kill(int(find_worker_pid(c)),signal.SIGKILL)
    print "Restarting %s..." % c
    start_container(c, t)
print ""

sleep(5)

print "Invoking Component methods after kill with 5 second delay..."
for cmd in clients:
    clients[cmd].sendline("Call")
    sleep(1)
    clients[cmd].expect(['Call .*',pexpect.EOF])
    print "%s: %s" % (cmd, clients[cmd].after.strip())
print ""

print "Shutting down clients..."
for cmd in clients:
    clients[cmd].sendline("Done")
    sleep(5)
    clients[cmd].expect(["Bye",pexpect.EOF])
    print "%s: %s" % (cmd, clients[cmd].after)
print ""

sleep(5)

for c,t in [("javaContainer", "--java"),("cppContainer", "--cpp"),("pyContainer", "--py")]:
    print "Stopping %s..." % c
    stop_container(c)

print "End of Processing"

#
# ___oOo___
