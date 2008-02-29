#!/usr/bin/env python
################################################################################################
# @(#) $Id: tcpBlocker.py,v 1.2 2008/02/29 00:24:55 agrimstrup Exp $
#
#    ALMA - Atacama Large Millimiter Array
#    (c) Associated Universities, Inc. Washington DC, USA, 2001
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
###############################################################################
'''

'''  
###############################################################################
import socket
import SocketServer

from time import sleep
from sys  import argv

from AcsutilPy.ACSPorts import getIP

#------------------------------------------------------------------------------
#--GLOBALS
ip_addr = getIP()
tcp_port = None

#------------------------------------------------------------------------------
#--Parse the command-line options.
for i in xrange(0, len(argv)):

    if argv[i]=="-ORBIIOPAddr":
        #inet:$HOST:$IR_PORT
        tcp_port = argv[i+1].split(":")[2]
        break
    
    elif argv[i]=="-ORBEndpoint":
        #iiop://$HOST:$IR_PORT
        tcp_port = argv[i+1].split(":")[2]
        break
    
    elif argv[i]=="-OAport":
        #$CDB_PORT
        tcp_port = argv[i+1]
        break

    elif argv[i]=="-ORBendPoint":
        #"giop:tcp:" + container_host + ':' +  str(newPort)
        tcp_port = argv[i+1].split(":")[3]
        break

    else:
        #
        continue

#-----------------------------------------------------------------------------
#--Sanity checks
if tcp_port==None:
    from sys import exit
    print "No TCP port detected in command-line!"
    exit(1)
else:
    tcp_port = int(tcp_port)

#-----------------------------------------------------------------------------
#--

#s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
#s.settimeout(1)
#returns true when the port is open and false if it's not
#ret_val = s.connect_ex((ip_addr, tcp_port))

server = SocketServer.TCPServer((ip_addr, tcp_port),
                                SocketServer.StreamRequestHandler)


while 1:
    try:
        sleep(1)
    except:
        server.close()
        server = None
        break
    

if server is not None:
    server.close()
