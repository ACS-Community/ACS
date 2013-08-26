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
# "@(#) $Id: Recorder.py,v 1.2 2010/10/01 17:20:48 javarias Exp $"
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

    def replaceXMLEntities(self, s):
        '''
        Replaces special characters of its corresponding XML entities.

        & -> &amp;
        ' -> &apos;
        < -> &lt;
        > -> &gt;
        " -> &quot;
        '''
        return s.replace('&', '&amp;').replace('\'', '&apos;').replace('<', '&lt;').replace('>', '&gt;').replace('"', '&quot;')

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
            sarg = self.replaceXMLEntities(pickle.dumps(arg))
            s += '    <arg str-rep="' + self.replaceXMLEntities(str(arg)) +\
                 '">' + sarg + '</arg>\n'
        s += '  </Call>\n'
        if self.rec_file:
            self.rec_file.write(s)

class Call:
    '''
    Represents an operation call over a given component.
    Used by the Player class.
    '''

    def __init__(self, meth_name, args, t):
        '''
        Constructor.
        Parameters:
        meth_name - Method name.
        args - Tuple containing the operation arguments.
        '''
        self.meth_name = meth_name
        self.args = args
        self.time = t
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

    def getTime(self):
        return self.time

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
        self.startTime = float(session_dom.getAttribute('start-time'))
        self.compName = session_dom.getAttribute('comp-name')
        for node in session_dom.childNodes:
            if node.nodeName == 'Call':
                meth_name = node.getAttribute('meth-name')
                rel_time = float(node.getAttribute('time'))
                args = []
                for arg_node in node.childNodes:
                    if arg_node.nodeName == 'arg':
                        sarg = self.restoreXMLEntities(str(arg_node.firstChild.nodeValue))
                        arg = pickle.loads(sarg)
                        self.fixCORBAEnums(arg)
                        args.append(arg)

                self.calls.append(Call(str(meth_name), tuple(args), rel_time))

    def __getitem__(self, i):
        '''
        Overrided indexing operation.
        '''
        return self.calls[i]

    def __len__(self):
        return len(self.calls)

    def restoreXMLEntities(self, s):
        '''
        Utilitiy function. Replaces XML entities for its respective characters.

        &amp   -> &
        &apos  -> '
        &lt;   -> <
        &gt;   -> >
        &quot; -> "
        '''
        return s.replace('&quot;', '"').replace('&gt;', '>').replace('&lt;', '<').replace('&apos;', '\'').replace('&amp;', '&')
       
    def fixCORBAEnums(self, obj):
        '''
        Utility function.
        Fixes a problem with CORBA Enums. For some reason that I don't quite understand yet,
        CORBA enumerations are not pickled/unpickled correctly. Using them as they are after
        unpickling will throw a CORBA.BAD_PARAM exception. One way to fix this is simply to
        recreate them, which is what this function does. It goes through each one of obj
        members, find the CORBA enums and recreates them. It works recursively for each one of
        obj members.

        Parameters:
        obj - An arbitrary CORBA object.
        '''
        import omniORB

        # If obj is a list, fix each one of the members recursively.
        if isinstance(obj, list):
            for i in range(len(obj)):
                # self.fixCORBAEnums(o)
                if isinstance(obj[i], omniORB.EnumItem):
                    enum = obj[i]
                    enumName = str(enum)
                    moduleName = enum._parent_id.split(':').pop(1).split('/')[1]
                    module = __import__(moduleName)
                    newEnum = module.__dict__[enumName]
                    obj[i] = newEnum

        # If obj is not a class instance, then there's nothing to do.
        # E.g., primitive types: float, str, etc., which doesn't have '__dict__'.
        try:
            getattr(obj, '__dict__')
        except AttributeError:
            return

        # Fix the case of obj being an enumeration itself.
        # TODO it won't fix, as argument is passed by value.
        # Change this function to return the modified value.
        if isinstance(obj, omniORB.EnumItem):
            enum = obj
            enumName = str(enum)
            moduleName = enum._parent_id.split(':').pop(1).split('/')[1]
            module = __import__(moduleName)
            newEnum = module.__dict__[enumName]
            # print 'Replacing enumeration', enumName
            obj = newEnum

        # Fix IDL structures with nested enumerations.
        # Go through each of of obj members ...
        for k in obj.__dict__.keys():
            # ...verify if it is an enumeration and in this case fix it
            if isinstance(obj.__dict__[k], omniORB.EnumItem):
                enum = obj.__dict__[k]
                enumName = str(enum)
                # (For example, if enum._parent_id is 'IDL:alma/Correlator/eDataProducts:1.0'
                # then in moduleName we get 'Correlator'. Of course, this will work only
                # if _parent_id follows this format. This should be true for all ALMA enums.)
                moduleName = enum._parent_id.split(':').pop(1).split('/')[1]
                module = __import__(moduleName)
                newEnum = module.__dict__[enumName]
                # print 'Replacing enumeration', k
                obj.__dict__[k] = newEnum
        
            # ... or if it is a CORBA structure, in this case call recursively, or ... 
            elif isinstance(obj.__dict__[k], omniORB.StructBase):
                self.fixCORBAEnums(obj.__dict__[k])

            # ... if it is a nested list, in this case case recursively in each one of the
            # members.
            elif isinstance(obj.__dict__[k], list):
                for o in obj.__dict__[k]:
                    self.fixCORBAEnums(o)
 
    def replaceCall(self, i, call):
        '''
        Replaces a call object.
        Parameters:
        i - Call number
        call - Call object
        '''
        self.calls[i] = call
        

    def getStartTime(self):
        return self.startTime

    def getComponentName(self):
        return self.compName

#
# __oOo__
        
        
