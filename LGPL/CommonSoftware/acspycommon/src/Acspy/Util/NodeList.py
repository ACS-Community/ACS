# @(#) $Id: NodeList.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $

__revision__ = "$Id: NodeList.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

'''
TODO:
- most inline doc is missing
'''

#
# NamingContext utility
#
import CosNaming

class nodeList:
  def __init__ (self, context): # pragma: NO COVER
    (self.list, self.it) = context.list(1024) #DWF-changed from 80

  def listdir (self):
    list = []
    for binding in self.list:            # binding - Name, binding type
      name = binding.binding_name   # sequence of name components, 1 deep
      type = binding.binding_type
      if (type == CosNaming.ncontext):  # directory
        list = list + [(name[0].id,1)]
      elif (name[0].kind == ''):  # no .xxx
        list = list + [(name[0].id,0)]
      else:
        list = list + [(name[0].id+"."+name[0].kind,0)]

    return list

  def ls (self): # pragma: NO COVER
    for binding in self.list:            # binding - Name, binding type
      name = binding.binding_name   # sequence of name components, 1 deep
      type = binding.binding_type
      if (type == CosNaming.ncontext):  # directory
        print name[0].id + "/"
      elif (name[0].kind == ''):  # no .xxx
        print name[0].id
      else:
        print name[0].id + "." + name[0].kind

  # return the object found.  should be a naming context
  def find (self, name, kind): # pragma: NO COVER
    for binding in self.list:
      n = binding.binding_name
      if ((n[0].id == name) & (n[0].kind == kind)):
        return binding

    return None

