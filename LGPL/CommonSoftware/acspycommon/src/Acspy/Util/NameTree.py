# @(#) $Id: NameTree.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $

__revision__ = "$Id: NameTree.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

'''
TODO:
- missing lots of inline doc here
- ???
'''

from Acspy.Util.ACSCorba import nameService
from Acspy.Util          import NodeList

import CosNaming

import string
import sys
from   traceback import print_exc

#
# extensions added to the different object types in the name service
#
extensions = { 
  'ROProperty':'rop', 
  'RWProperty':'rwp', 
  'Device':'dev',
  'Service':'srv'
}

#
# getnode - used by many local routines to resolve and narrow
# the node and return a context.  
#
def getnode (node, wd):
  ctx = None
  try:
    obj = wd.resolve(node)
    try:
      ctx = obj._narrow(CosNaming.NamingContext)
    except:
      ctx = None
  except:
    pass
  return ctx

#
# keep a directory tree without any cycles
#
class nameTree:
  def __init__(self, corba_object, nameServerName=''):
    # get reference to name service
    ns_obj = nameService()

    if (ns_obj is None):
      print "name service bad resolve"
      return
    
    self.top = ns_obj._narrow(CosNaming.NamingContext)
    self.path = [('',self.top)]    # keep track of where we are
    self.pathcopy = []
    
  #
  # ls ()
  #
  def ls (self): # pragma: NO COVER
    (n, cwd) = self.path[-1]
    nl = NodeList.nodeList(cwd)
    nl.ls()

  #
  # listdir ()
  #
  def listdir (self): # pragma: NO COVER
    (n, cwd) = self.path[-1]
    nl = NodeList.nodeList(cwd)
    return nl.listdir()

  #
  # mkdir (path)
  #
  def mkdir (self, path):
    #
    # path can be relative or absolute.  This is determined
    # by the leading /
    #
    # break path into parts by the /
    #
    nodes = string.splitfields(path,'/')
    wd = None
    if (nodes[0] == ''):
      wd = self.top
      del nodes[0]
    else:
      (n, wd) = self.path[-1]

    for name in nodes:
      # try to resolve the name, if it fails, jump out and
      # start creating contexts
      node = [CosNaming.NameComponent (name,"")]
      nextwd = getnode (node , wd)
      if (nextwd is None):
        try:
          nextwd = wd.bind_new_context (node) # 
        except:
          print "Error binding new context for ", name, "!!!", sys.exc_info()[0]
          return
      wd = nextwd

  #
  # cd (path)
  #
  def cd (self, path, ignore_error=0):
    nodes = string.splitfields(path,'/')
      
    # check for "../../xxx" addresses
    if (nodes[0] == '..'):
      while (nodes != [] and self.path != [] and nodes[0] == '..'):
        (x,y) =  self.path[len(self.path)-1]
        if(x == '/'): break
        del nodes[0]
        del self.path[len(self.path)-1]
  
    # reached the directory top, fix self.path
    if ((nodes != [] and nodes[0] == '') 
            or len(self.path) == 0):    # absolute, search from the top
      self.path = [('', self.top)]

    (n, wd) = self.path[len(self.path)-1]

    changed = 1
    changes = []

    for name in nodes:
      if (name == ''): continue
      wd = getnode ([CosNaming.NameComponent (name,"")], wd)
      if (wd is not None): 
        changes.append ((name, wd))
        continue
      if (ignore_error == 0):
        changed = 0
        return 0

    if (changed == 1):
      self.path = self.path + changes
    return 1

  #
  # getObject (name, type) - return CORBA::Object()
  #
  def getObject (self, name, type):
    "this does not handle absolute or relative path, just cwd"
    (n, cwd) = self.path[-1]
    leaf = [CosNaming.NameComponent (name, type)]
    try: return cwd.resolve (leaf)
    except CosNaming.NamingContext.NotFound:
      raise
    except:
      print 'name service error resolving ', self.path, name, type
      print sys.exc_info()

  #
  # putObject (name, object) - put CORBA::Object() at this 
  # name.  If name does not exist, make it.  If no object, then
  # make a placeholder
  #
  def putObject (self, name, type, object): # pragma: NO COVER
    "this does not handle absolute or relative path, just cwd"
    # first just try a rebind()  if that fails, then see if
    # path and name.  If it is a path, then check the path
    # and make it if needed.  Then try a bind.

    (n, cwd) = self.path[-1]
    leaf = [CosNaming.NameComponent (name, type)]
    return cwd.rebind (leaf, object)

  #
  # putObject (name, object) - put CORBA::Object() at this
  # name.  If name does not exist, make it.  If no object, then
  # make a placeholder
  #
  def delObject (self, name, type): # pragma: NO COVER
    "this does not handle absolute or relative path, just cwd"
    # Unbind something.

    (n, cwd) = self.path[-1]
    leaf = [CosNaming.NameComponent (name, type)]
    cwd.unbind (leaf)
    return
  
  #
  # pwd()
  #
  def pwd (self):
    wd = ''
    for (x, y) in self.path:
      if x == '': continue
      wd = wd + '/' + x
    return wd

  #
  # rm()
  #
  def rm (self, name, args="noforce"): # pragma: NO COVER
    # pushd this directory, cd to other directory less last name,
    # get the objet for last name, and if it is a context, check
    # force flag.  if it is to go, follow the directory tree and
    # toss all the links
    if (args == "noforce"): return
    return

  #
  # cp()
  #
  def cp (self, name): # pragma: NO COVER
    return

  #
  # pushd()
  #
  def pushd (self, l=""): # pragma: NO COVER
    if (l == "?"):
      i = 0
      for path in self.pathcopy:
        s = ''
        for (n, ctx) in path:
          s = s + '/' + n
        print "%02d: %s" %(i, s)
        i = i + 1
    else:
      self.pathcopy.append(self.path)
    return

  #
  # popd()
  #
  def popd (self, index=-1): # pragma: NO COVER
    try:
      self.path = self.pathcopy.pop(index)
    except:
      print "top of list, no pop"
    return

if (__name__ == "__main__"): # pragma: NO COVER
  print '==> Testing nameTree class'
  testlevel = 0
  nt = nameTree(None)

  print '==> Naming service variable name is: nt'
  print '==> Listing entries in Naming Service:'
  nt.ls ()
  
  # It seems this does not work
  print '==> PWD: ', nt.pwd ()
  if (testlevel > 0):
    nt.pushd ()
    nt.cd ("/")
    nt.pushd ()
    nt.cd ("/cat")
    nt.pushd()
    nt.pushd ("?")

  print '==> cd into MOUNT1.0'

  # Nothing happens here
  nt.cd("MOUNT1.0")

  nt.ls ()


#  nt.cd("Antenna Systems/Services/Cryogenics")
#  obj = nt.getObject("Status","rwp")
#  from acs.baci import ESO
#  status = obj._narrow(ESO.ROlong)
#  print 'status at status'

