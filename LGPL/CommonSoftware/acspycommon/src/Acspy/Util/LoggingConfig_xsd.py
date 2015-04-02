#!/usr/bin/env python

#
# Generated Thu Apr 24 14:01:43 2008 by generateDS.py.
#

import sys
import getopt
from xml.dom import minidom
from xml.dom import Node

#
# If you have installed IPython you can uncomment and use the following.
# IPython is available from http://ipython.scipy.org/.
#

## from IPython.Shell import IPShellEmbed
## args = ''
## ipshell = IPShellEmbed(args,
##     banner = 'Dropping into IPython',
##     exit_msg = 'Leaving Interpreter, back to program.')

# Then use the following line where and when you want to drop into the
# IPython shell:
#    ipshell('<some message> -- Entering ipshell.\nHit Ctrl-D to exit')

#
# Support/utility functions.
#

def showIndent(outfile, level):
    for idx in range(level):
        outfile.write('    ')

def quote_xml(inStr):
    s1 = inStr
    s1 = s1.replace('&', '&amp;')
    s1 = s1.replace('<', '&lt;')
    s1 = s1.replace('"', '&quot;')
    return s1

def quote_python(inStr):
    s1 = inStr
    if s1.find("'") == -1:
        if s1.find('\n') == -1:
            return "'%s'" % s1
        else:
            return "'''%s'''" % s1
    else:
        if s1.find('"') != -1:
            s1 = s1.replace('"', '\\"')
        if s1.find('\n') == -1:
            return '"%s"' % s1
        else:
            return '"""%s"""' % s1


class MixedContainer:
    # Constants for category:
    CategoryNone = 0
    CategoryText = 1
    CategorySimple = 2
    CategoryComplex = 3
    # Constants for content_type:
    TypeNone = 0
    TypeText = 1
    TypeString = 2
    TypeInteger = 3
    TypeFloat = 4
    TypeDecimal = 5
    TypeDouble = 6
    TypeBoolean = 7
    def __init__(self, category, content_type, name, value):
        self.category = category
        self.content_type = content_type
        self.name = name
        self.value = value
    def getCategory(self):
        return self.category
    def getContenttype(self, content_type):
        return self.content_type
    def getValue(self):
        return self.value
    def getName(self):
        return self.name
    def export(self, outfile, level, name):
        if self.category == MixedContainer.CategoryText:
            outfile.write(self.value)
        elif self.category == MixedContainer.CategorySimple:
            self.exportSimple(outfile, level, name)
        else:    # category == MixedContainer.CategoryComplex
            self.value.export(outfile, level, name)
    def exportSimple(self, outfile, level, name):
        if self.content_type == MixedContainer.TypeString:
            outfile.write('<%s>%s</%s>' % (self.name, self.value, self.name))
        elif self.content_type == MixedContainer.TypeInteger or \
                self.content_type == MixedContainer.TypeBoolean:
            outfile.write('<%s>%d</%s>' % (self.name, self.value, self.name))
        elif self.content_type == MixedContainer.TypeFloat or \
                self.content_type == MixedContainer.TypeDecimal:
            outfile.write('<%s>%f</%s>' % (self.name, self.value, self.name))
        elif self.content_type == MixedContainer.TypeDouble:
            outfile.write('<%s>%g</%s>' % (self.name, self.value, self.name))
    def exportLiteral(self, outfile, level, name):
        if self.category == MixedContainer.CategoryText:
            showIndent(outfile, level)
            outfile.write('MixedContainer(%d, %d, "%s", "%s"),\n' % \
                (self.category, self.content_type, self.name, self.value))
        elif self.category == MixedContainer.CategorySimple:
            showIndent(outfile, level)
            outfile.write('MixedContainer(%d, %d, "%s", "%s"),\n' % \
                (self.category, self.content_type, self.name, self.value))
        else:    # category == MixedContainer.CategoryComplex
            showIndent(outfile, level)
            outfile.write('MixedContainer(%d, %d, "%s",\n' % \
                (self.category, self.content_type, self.name,))
            self.value.exportLiteral(outfile, level + 1)
            showIndent(outfile, level)
            outfile.write(')\n')


class _MemberSpec(object):
    def __init__(self, name='', data_type='', container=0):
        self.name = name
        self.data_type = data_type
        self.container = container
    def set_name(self, name): self.name = name
    def get_name(self): return self.name
    def set_data_type(self, data_type): self.data_type = data_type
    def get_data_type(self): return self.data_type
    def set_container(self, container): self.container = container
    def get_container(self): return self.container


#
# Data representation classes.
#

class UnnamedLogger:
    subclass = None
    def __init__(self, minLogLevelLocal=2, minLogLevel=2, valueOf_=''):
        self.minLogLevelLocal = minLogLevelLocal
        self.minLogLevel = minLogLevel
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if UnnamedLogger.subclass:
            return UnnamedLogger.subclass(*args_, **kwargs_)
        else:
            return UnnamedLogger(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_minLogLevelLocal(self): return self.minLogLevelLocal
    def set_minLogLevelLocal(self, minLogLevelLocal): self.minLogLevelLocal = minLogLevelLocal
    def get_minLogLevel(self): return self.minLogLevel
    def set_minLogLevel(self, minLogLevel): self.minLogLevel = minLogLevel
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, name_='UnnamedLogger'):
        showIndent(outfile, level)
        outfile.write('<%s' % (name_, ))
        self.exportAttributes(outfile, level, name_='UnnamedLogger')
        outfile.write('>')
        self.exportChildren(outfile, level + 1, name_)
        outfile.write('</%s>\n' % name_)
    def exportAttributes(self, outfile, level, name_='UnnamedLogger'):
        if self.get_minLogLevelLocal() is not None:
            outfile.write(' minLogLevelLocal="%s"' % (self.get_minLogLevelLocal(), ))
        if self.get_minLogLevel() is not None:
            outfile.write(' minLogLevel="%s"' % (self.get_minLogLevel(), ))
    def exportChildren(self, outfile, level, name_='UnnamedLogger'):
        outfile.write(self.valueOf_)
    def exportLiteral(self, outfile, level, name_='UnnamedLogger'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('minLogLevelLocal = "%s",\n' % (self.get_minLogLevelLocal(),))
        showIndent(outfile, level)
        outfile.write('minLogLevel = "%s",\n' % (self.get_minLogLevel(),))
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        if attrs.get('minLogLevelLocal'):
            try:
                self.minLogLevelLocal = int(attrs.get('minLogLevelLocal').value)
            except ValueError:
                raise ValueError('Bad integer attribute (minLogLevelLocal)')
        if attrs.get('minLogLevel'):
            try:
                self.minLogLevel = int(attrs.get('minLogLevel').value)
            except ValueError:
                raise ValueError('Bad integer attribute (minLogLevel)')
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class UnnamedLogger


class NamedLogger(UnnamedLogger):
    subclass = None
    def __init__(self, Name='', minLogLevelLocal=2, minLogLevel=2, valueOf_=''):
        UnnamedLogger.__init__(self, minLogLevelLocal, minLogLevel)
        self.Name = Name
        self.valueOf_ = valueOf_
    def factory(*args_, **kwargs_):
        if NamedLogger.subclass:
            return NamedLogger.subclass(*args_, **kwargs_)
        else:
            return NamedLogger(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get_Name(self): return self.Name
    def set_Name(self, Name): self.Name = Name
    def getValueOf_(self): return self.valueOf_
    def setValueOf_(self, valueOf_): self.valueOf_ = valueOf_
    def export(self, outfile, level, name_='NamedLogger'):
        showIndent(outfile, level)
        outfile.write('<%s' % (name_, ))
        self.exportAttributes(outfile, level, name_='NamedLogger')
        outfile.write('>')
        self.exportChildren(outfile, level + 1, name_)
        outfile.write('</%s>\n' % name_)
    def exportAttributes(self, outfile, level, name_='NamedLogger'):
        outfile.write(' Name="%s"' % (self.get_Name(), ))
        UnnamedLogger.exportAttributes(self, outfile, level, name_='NamedLogger')
    def exportChildren(self, outfile, level, name_='NamedLogger'):
        UnnamedLogger.exportChildren(self, outfile, level, name_)
        outfile.write(self.valueOf_)
    def exportLiteral(self, outfile, level, name_='NamedLogger'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('Name = "%s",\n' % (self.get_Name(),))
        UnnamedLogger.exportLiteralAttributes(self, outfile, level, name_)
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('valueOf_ = "%s",\n' % (self.valueOf_,))
        UnnamedLogger.exportLiteralChildren(self, outfile, level, name_)
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        self.valueOf_ = ''
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        if attrs.get('Name'):
            self.Name = attrs.get('Name').value
        UnnamedLogger.buildAttributes(self, attrs)
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.TEXT_NODE:
            self.valueOf_ += child_.nodeValue
# end class NamedLogger


class LoggingConfig(UnnamedLogger):
    subclass = None
    def __init__(self, maxLogQueueSize=1000, dispatchPacketSize=10, centralizedLogger="Log", flushPeriodSeconds=10, immediateDispatchLevel=10, _=None, minLogLevelLocal=2, minLogLevel=2):
        UnnamedLogger.__init__(self, minLogLevelLocal, minLogLevel)
        self.maxLogQueueSize = maxLogQueueSize
        self.dispatchPacketSize = dispatchPacketSize
        self.centralizedLogger = centralizedLogger
        self.flushPeriodSeconds = flushPeriodSeconds
        self.immediateDispatchLevel = immediateDispatchLevel
        if _ is None:
            self._ = []
        else:
            self._ = _
    def factory(*args_, **kwargs_):
        if LoggingConfig.subclass:
            return LoggingConfig.subclass(*args_, **kwargs_)
        else:
            return LoggingConfig(*args_, **kwargs_)
    factory = staticmethod(factory)
    def get__(self): return self._
    def set__(self, _): self._ = _
    def add__(self, value): self._.append(value)
    def insert__(self, index, value): self._[index] = value
    def get_maxLogQueueSize(self): return self.maxLogQueueSize
    def set_maxLogQueueSize(self, maxLogQueueSize): self.maxLogQueueSize = maxLogQueueSize
    def get_dispatchPacketSize(self): return self.dispatchPacketSize
    def set_dispatchPacketSize(self, dispatchPacketSize): self.dispatchPacketSize = dispatchPacketSize
    def get_centralizedLogger(self): return self.centralizedLogger
    def set_centralizedLogger(self, centralizedLogger): self.centralizedLogger = centralizedLogger
    def get_flushPeriodSeconds(self): return self.flushPeriodSeconds
    def set_flushPeriodSeconds(self, flushPeriodSeconds): self.flushPeriodSeconds = flushPeriodSeconds
    def get_immediateDispatchLevel(self): return self.immediateDispatchLevel
    def set_immediateDispatchLevel(self, immediateDispatchLevel): self.immediateDispatchLevel = immediateDispatchLevel
    def export(self, outfile, level, name_='LoggingConfig'):
        showIndent(outfile, level)
        outfile.write('<%s' % (name_, ))
        self.exportAttributes(outfile, level, name_='LoggingConfig')
        outfile.write('>\n')
        self.exportChildren(outfile, level + 1, name_)
        showIndent(outfile, level)
        outfile.write('</%s>\n' % name_)
    def exportAttributes(self, outfile, level, name_='LoggingConfig'):
        if self.get_maxLogQueueSize() is not None:
            outfile.write(' maxLogQueueSize="%s"' % (self.get_maxLogQueueSize(), ))
        if self.get_dispatchPacketSize() is not None:
            outfile.write(' dispatchPacketSize="%s"' % (self.get_dispatchPacketSize(), ))
        if self.get_centralizedLogger() is not None:
            outfile.write(' centralizedLogger="%s"' % (self.get_centralizedLogger(), ))
        if self.get_flushPeriodSeconds() is not None:
            outfile.write(' flushPeriodSeconds="%s"' % (self.get_flushPeriodSeconds(), ))
        if self.get_immediateDispatchLevel() is not None:
            outfile.write(' immediateDispatchLevel="%s"' % (self.get_immediateDispatchLevel(), ))
        UnnamedLogger.exportAttributes(self, outfile, level, name_='LoggingConfig')
    def exportChildren(self, outfile, level, name_='LoggingConfig'):
        for __ in self.get__():
            __.export(outfile, level, name_='_')
        UnnamedLogger.exportChildren(self, outfile, level, name_)
    def exportLiteral(self, outfile, level, name_='LoggingConfig'):
        level += 1
        self.exportLiteralAttributes(outfile, level, name_)
        self.exportLiteralChildren(outfile, level, name_)
    def exportLiteralAttributes(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('maxLogQueueSize = "%s",\n' % (self.get_maxLogQueueSize(),))
        showIndent(outfile, level)
        outfile.write('dispatchPacketSize = "%s",\n' % (self.get_dispatchPacketSize(),))
        showIndent(outfile, level)
        outfile.write('centralizedLogger = "%s",\n' % (self.get_centralizedLogger(),))
        showIndent(outfile, level)
        outfile.write('flushPeriodSeconds = "%s",\n' % (self.get_flushPeriodSeconds(),))
        showIndent(outfile, level)
        outfile.write('immediateDispatchLevel = "%s",\n' % (self.get_immediateDispatchLevel(),))
        UnnamedLogger.exportLiteralAttributes(self, outfile, level, name_)
    def exportLiteralChildren(self, outfile, level, name_):
        showIndent(outfile, level)
        outfile.write('_=[\n')
        level += 1
        for _ in self._:
            showIndent(outfile, level)
            outfile.write('NamedLogger(\n')
            _.exportLiteral(outfile, level, name_='_')
            showIndent(outfile, level)
            outfile.write('),\n')
        level -= 1
        showIndent(outfile, level)
        outfile.write('],\n')
        UnnamedLogger.exportLiteralChildren(self, outfile, level, name_)
    def build(self, node_):
        attrs = node_.attributes
        self.buildAttributes(attrs)
        for child_ in node_.childNodes:
            nodeName_ = child_.nodeName.split(':')[-1]
            self.buildChildren(child_, nodeName_)
    def buildAttributes(self, attrs):
        if attrs.get('maxLogQueueSize'):
            try:
                self.maxLogQueueSize = int(attrs.get('maxLogQueueSize').value)
            except ValueError:
                raise ValueError('Bad integer attribute (maxLogQueueSize)')
        if attrs.get('dispatchPacketSize'):
            try:
                self.dispatchPacketSize = int(attrs.get('dispatchPacketSize').value)
            except ValueError:
                raise ValueError('Bad integer attribute (dispatchPacketSize)')
        if attrs.get('centralizedLogger'):
            self.centralizedLogger = attrs.get('centralizedLogger').value
        if attrs.get('flushPeriodSeconds'):
            try:
                self.flushPeriodSeconds = int(attrs.get('flushPeriodSeconds').value)
            except ValueError:
                raise ValueError('Bad integer attribute (flushPeriodSeconds)')
            if self.flushPeriodSeconds < 0:
                raise ValueError('Invalid NonNegativeInteger (flushPeriodSeconds)')
        if attrs.get('immediateDispatchLevel'):
            try:
                self.immediateDispatchLevel = int(attrs.get('immediateDispatchLevel').value)
            except ValueError:
                raise ValueError('Bad integer attribute (immediateDispatchLevel)')
        UnnamedLogger.buildAttributes(self, attrs)
    def buildChildren(self, child_, nodeName_):
        if child_.nodeType == Node.ELEMENT_NODE and \
            nodeName_ == '_':
            obj_ = NamedLogger.factory()
            obj_.build(child_)
            self._.append(obj_)
# end class LoggingConfig


from xml.sax import handler, make_parser

class SaxStackElement:
    def __init__(self, name='', obj=None):
        self.name = name
        self.obj = obj
        self.content = ''

#
# SAX handler
#
class Sax_LoggingConfigHandler(handler.ContentHandler):
    def __init__(self):
        self.stack = []
        self.root = None

    def getRoot(self):
        return self.root

    def setDocumentLocator(self, locator):
        self.locator = locator
    
    def showError(self, msg):
        print '*** (showError):', msg
        sys.exit(-1)

    def startElement(self, name, attrs):
        done = 0
        if name == 'LoggingConfig':
            obj = LoggingConfig.factory()
            stackObj = SaxStackElement('LoggingConfig', obj)
            self.stack.append(stackObj)
            done = 1
        elif name == '_':
            obj = NamedLogger.factory()
            stackObj = SaxStackElement('_', obj)
            self.stack.append(stackObj)
            done = 1
        if not done:
            self.reportError('"%s" element not allowed here.' % name)

    def endElement(self, name):
        done = 0
        if name == 'LoggingConfig':
            if len(self.stack) == 1:
                self.root = self.stack[-1].obj
                self.stack.pop()
                done = 1
        elif name == '_':
            if len(self.stack) >= 2:
                self.stack[-2].obj.add__(self.stack[-1].obj)
                self.stack.pop()
                done = 1
        if not done:
            self.reportError('"%s" element not allowed here.' % name)

    def characters(self, chrs, start, end):
        if len(self.stack) > 0:
            self.stack[-1].content += chrs[start:end]

    def reportError(self, mesg):
        locator = self.locator
        sys.stderr.write('Doc: %s  Line: %d  Column: %d\n' % \
            (locator.getSystemId(), locator.getLineNumber(), 
            locator.getColumnNumber() + 1))
        sys.stderr.write(mesg)
        sys.stderr.write('\n')
        sys.exit(-1)
        #raise RuntimeError

USAGE_TEXT = """
Usage: python <Parser>.py [ -s ] <in_xml_file>
Options:
    -s        Use the SAX parser, not the minidom parser.
"""

def usage():
    print USAGE_TEXT
    sys.exit(-1)


#
# SAX handler used to determine the top level element.
#
class SaxSelectorHandler(handler.ContentHandler):
    def __init__(self):
        self.topElementName = None
    def getTopElementName(self):
        return self.topElementName
    def startElement(self, name, attrs):
        self.topElementName = name
        raise StopIteration


def parseSelect(inFileName):
    infile = file(inFileName, 'r')
    topElementName = None
    parser = make_parser()
    documentHandler = SaxSelectorHandler()
    parser.setContentHandler(documentHandler)
    try:
        try:
            parser.parse(infile)
        except StopIteration:
            topElementName = documentHandler.getTopElementName()
        if topElementName is None:
            raise RuntimeError, 'no top level element'
        topElementName = topElementName.replace('-', '_').replace(':', '_')
        if topElementName not in globals():
            raise RuntimeError, 'no class for top element: %s' % topElementName
        topElement = globals()[topElementName]
        infile.seek(0)
        doc = minidom.parse(infile)
    finally:
        infile.close()
    rootNode = doc.childNodes[0]
    rootObj = topElement.factory()
    rootObj.build(rootNode)
    # Enable Python to collect the space used by the DOM.
    doc = None
    sys.stdout.write('<?xml version="1.0" ?>\n')
    rootObj.export(sys.stdout, 0)
    return rootObj


def saxParse(inFileName):
    parser = make_parser()
    documentHandler = Sax_LoggingConfigHandler()
    parser.setDocumentHandler(documentHandler)
    parser.parse('file:%s' % inFileName)
    root = documentHandler.getRoot()
    sys.stdout.write('<?xml version="1.0" ?>\n')
    root.export(sys.stdout, 0)
    return root


def saxParseString(inString):
    parser = make_parser()
    documentHandler = Sax_LoggingConfigHandler()
    parser.setDocumentHandler(documentHandler)
    parser.feed(inString)
    parser.close()
    rootObj = documentHandler.getRoot()
    #sys.stdout.write('<?xml version="1.0" ?>\n')
    #rootObj.export(sys.stdout, 0)
    return rootObj


def parse(inFileName):
    doc = minidom.parse(inFileName)
    rootNode = doc.documentElement
    rootObj = LoggingConfig.factory()
    rootObj.build(rootNode)
    # Enable Python to collect the space used by the DOM.
    doc = None
    sys.stdout.write('<?xml version="1.0" ?>\n')
    rootObj.export(sys.stdout, 0, name_="LoggingConfig")
    return rootObj


def parseString(inString):
    doc = minidom.parseString(inString)
    rootNode = doc.documentElement
    rootObj = LoggingConfig.factory()
    rootObj.build(rootNode)
    # Enable Python to collect the space used by the DOM.
    doc = None
    sys.stdout.write('<?xml version="1.0" ?>\n')
    rootObj.export(sys.stdout, 0, name_="LoggingConfig")
    return rootObj


def parseLiteral(inFileName):
    doc = minidom.parse(inFileName)
    rootNode = doc.documentElement
    rootObj = LoggingConfig.factory()
    rootObj.build(rootNode)
    # Enable Python to collect the space used by the DOM.
    doc = None
    sys.stdout.write('from LoggingConfig_xsd import *\n\n')
    sys.stdout.write('rootObj = LoggingConfig(\n')
    rootObj.exportLiteral(sys.stdout, 0, name_="LoggingConfig")
    sys.stdout.write(')\n')
    return rootObj


def main():
    args = sys.argv[1:]
    if len(args) == 2 and args[0] == '-s':
        saxParse(args[1])
    elif len(args) == 1:
        parse(args[0])
    else:
        usage()


if __name__ == '__main__':
    main()
    #import pdb
    #pdb.run('main()')

