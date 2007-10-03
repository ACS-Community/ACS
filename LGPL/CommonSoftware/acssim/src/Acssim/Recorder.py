#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2005 
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
# "@(#) $Id: Recorder.py,v 1.1 2007/10/03 20:44:03 agrimstrup Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# rhiriart  2006-12-06  created
#

import os
import pickle
import time
import inspect

from xml.dom.minidom import parseString

class Recorder:
    '''
    This class is used by the ACS IDL Simulator to record a simulation session
    into a file. This file can be used afterwards by the Player class to run
    again the session over the "real" component.
    '''

    def __init__(self, comp_name):
        '''
        Constructor. Checks if the environment variable ACSSIM_REC_DIR has
        been set. This should be the directory where the Recorder will output
        invokation information. If this env. variable is not set the Recorder
        class does nothing.

        Parameters:
        comp_name - Component name.
        '''
        self.ACSSIM_REC_DIR = 'ACSSIM_REC_DIR'
        self.rec_file_name = None
        self.rec_file = None
        if self.ACSSIM_REC_DIR in os.environ.keys():
            if not os.path.exists(os.environ[self.ACSSIM_REC_DIR]):
                os.mkdir(os.environ[self.ACSSIM_REC_DIR])
            self.rec_file_name = os.environ[self.ACSSIM_REC_DIR]+'/'+\
                comp_name.replace('/', '_')+".xml"
        self.start_time = -1
        self.comp_name = comp_name

    def begin(self):
        '''
        Begin the recording session.
        '''
        self.start_time = time.time()
        if self.rec_file_name:
            self.rec_file = open(self.rec_file_name, 'w')
            self.rec_file.write('<?xml version="1.0" encoding="UTF-8"?>\n')
            self.rec_file.write('<Simulation-Session start-time="' + str(self.start_time) +
                                '" comp-name="' + self.comp_name + '">\n')
            self.rec_file.flush()

    def end(self):
        '''
        End the recording session.
        '''
        if self.rec_file:
            self.rec_file.write('</Simulation-Session>\n')
            self.rec_file.close()
        
    def record(self, meth_name, args):
        '''
        Record a method invokation.

        Parameters:
        method_name - Method name.
        args - Argument list.
        '''
        t = time.time() - self.start_time
        s = '  <Call meth-name="' + meth_name + '" time="'+ str(t) + '">\n'
        for arg in args:
            sarg = pickle.dumps(arg).replace('<', '&lt;').replace('>', '&gt;')
            s += '    <arg str-rep="' + str(arg).replace('<', '&lt;').replace('>', '&gt;') +\
                 '">' + sarg + '</arg>\n'
        s += '  </Call>\n'
        if self.rec_file:
            self.rec_file.write(s)

class Call:
    '''
    Represents an operation call over a given component.
    Used by the Player class.
    '''

    def __init__(self, meth_name, args):
        '''
        Constructor.
        Parameters:
        meth_name - Method name.
        args - Tuple containing the operation arguments.
        '''
        self.meth_name = meth_name
        self.args = args
        self.return_value = None

    def getMethodName(self):
        '''
        Gets the method's name.
        '''
        return self.meth_name

    def getArgs(self):
        '''
        Gets the method's arguments.
        '''
        return self.args

    def getReturnValue(self):
        '''
        Get the return value. Calling this method only makes sense after
        calling invoke().
        '''
        return self.return_value

    def invoke(self, component):
        '''
        Invoke the call represented by an instance of this class over a
        component. Of course, the component should support the operation.
        Parameters:
        component - Component reference.
        '''
        method = None
        for m in inspect.getmembers(component):
            if m[0] == self.meth_name:
                method = m[1]
        if method:
            self.return_value = apply(method, self.args)

    def setArgs(self, args):
        '''
        Sets the arguments for this call. This is useful for cases where it is needed
        to replace arguments in order for an invokation to work. For example, if
        the arguments are CORBA references or callbacks, which only are valid during
        the recording session.
        Parameters:
        args - Arguments tuple.
        '''
        self.args = args

class Player:
    '''
    Plays a record file generated with the IDL Simulator.
    '''

    def __init__(self, file_name):
        '''
        Constructor.
        Parameters:
        file_name - File generated by the IDL simulator when the ACSSIM_REC_DIR
                    environment variable is set in the process running the simulated
                    components.
                    This file contains a description of the calls to a simulated component,
                    along with its arguments.
        '''
        f = open(file_name)
        xml_doc = f.read()
        root_dom = parseString(xml_doc)

        self.calls = []
        session_dom = root_dom.firstChild
        for node in session_dom.childNodes:
            if node.nodeName == 'Call':
                meth_name = node.getAttribute('meth-name')
                args = []
                for arg_node in node.childNodes:
                    if arg_node.nodeName == 'arg':
                        sarg = str(arg_node.firstChild.nodeValue).replace('&lt;', '<').replace('&gt;', '>')
                        arg = pickle.loads(sarg)
                        args.append(arg)

                self.calls.append(Call(str(meth_name), tuple(args)))

    def __getitem__(self, i):
        '''
        Overrided indexing operation.
        '''
        return self.calls[i]

    def replaceCall(self, i, call):
        '''
        Replaces a call object.
        Parameters:
        i - Call number
        call - Call object
        '''
        self.calls[i] = call
        
#
# __oOo__
        
        
