#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2007 
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
# "@(#) $Id: acsutilpyTestImport.py,v 1.4 2007/09/21 19:45:32 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstr  2007-02-20  created
#

#------------------------------------------------------------------------------
__revision__ = "$Id: acsutilpyTestImport.py,v 1.4 2007/09/21 19:45:32 agrimstrup Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
import sys
import os
import os.path
import string
import unittest
#--ACS IMPORTS____-------------------------------------------------------------
import AcsutilPy.ACSImport
#------------------------------------------------------------------------------

def importTestHelper(modname=None):
    if modname:
        exec "import %s" % str(modname)
    else:
        exec "import"
    return "Ok"
            
def importFromTestHelper(pkgname=None, modname=None):
    if pkgname:
        p = "from %s " % str(pkgname)
    else:
        p = "from "
    if modname:
        m = "import %s" % str(modname)
    else:
        m = "import"
    exec p + m
    return "Ok"
            

class AImportSyntaxCheck(unittest.TestCase):

    def testANoParam(self):
        """Test import statement with no parameter"""
        self.assertRaises(SyntaxError, importTestHelper)
        
    def testBBadInputInt(self):
        """Test import statement with int parameter"""
        self.assertRaises(SyntaxError, importTestHelper, 12)

    def testCBadInputFloat(self):
        """Test import statement with float parameter"""
        self.assertRaises(SyntaxError, importTestHelper, 1.22)

    def testDBadSeparator(self):
        """Test import statement with invalid separator"""
        self.assertRaises(SyntaxError, importTestHelper, 'logging/handlers')

    def testEFromNoParam(self):
        """Test from import statement with no package or module information"""
        self.assertRaises(SyntaxError, importFromTestHelper)
        
    def testFFromBadInputInt(self):
        """Test from import statement with int package and no module information"""
        self.assertRaises(SyntaxError, importFromTestHelper, 12)

    def testGFromBadInputFloat(self):
        """Test from import statement with float package and no module information"""
        self.assertRaises(SyntaxError, importFromTestHelper, 1.22)

    def testHFromBadSeparator(self):
        """Test from import statement with invalid package separator and no module information """
        self.assertRaises(SyntaxError, importFromTestHelper, 'logging/handlers')

    def testIFromBadModule(self):
        """Test from import statement with no module information"""
        self.assertRaises(SyntaxError, importFromTestHelper, 'logging.handlers')

    def testJFromBadModuleInt(self):
        """Test from import statement with int module information"""
        self.assertRaises(SyntaxError, importFromTestHelper, 'logging.handlers',
                          12)

    def testKFromBadModuleFloat(self):
        """Test from import statement with float module information"""
        self.assertRaises(SyntaxError, importFromTestHelper, 'logging.handlers',
                          1.2)


class BSingleImportCheck(unittest.TestCase):

    def testAUnknownModule(self):
        """Import a module with a mis-spelled name"""
        self.assertRaises(ImportError, importTestHelper,'RaNdoM')

    def testBFromUnknownModule(self):
        """Import a module with a mis-spelled name using 'from import' statement"""
        self.assertRaises(ImportError, importFromTestHelper,'RaNdoM', 'gauss')

    def testCModule(self):
        """Import a known module"""
        import random
        self.assertEqual(True, 'random' in sys.modules)
        self.assertEqual(True, 'random' in dir())
        self.assertEqual(random, sys.modules['random'])

    def testDFromModule(self):
        """Import a known module using 'from import' statement"""
        from random import gauss
        self.assertEqual(True, 'random' in sys.modules)
        self.assertEqual(False, 'random' in dir())
        self.assertEqual(True, 'gauss' in dir())
        self.assertEqual(gauss, sys.modules['random'].gauss)
        self.assertEqual(True, 'gauss' in dir(sys.modules['random']))

    def testENestedModule(self):
        """Import a known nested module"""
        import logging.handlers
        self.assertEqual(True, 'logging.handlers' in sys.modules)
        self.assertEqual(True, 'logging' in sys.modules)
        self.assertEqual(True, 'logging' in dir())
        self.assertEqual(True, 'handlers' in dir(logging))
        self.assertEqual(logging, sys.modules['logging'])
        self.assertEqual(logging.handlers, sys.modules['logging.handlers'])

    def testFFromNestedModule(self):
        """Import a known nested module using 'from import' statement"""
        from logging.handlers import RotatingFileHandler
        self.assertEqual(True, 'logging.handlers' in sys.modules)
        self.assertEqual(True, 'logging' in sys.modules)
        self.assertEqual(True, 'RotatingFileHandler' in dir())
        self.assertEqual(False, 'logging' in dir())
        self.assertEqual(False, 'logging.handlers' in dir())
        self.assertEqual(True, 'handlers' in dir(sys.modules['logging']))
        self.assertEqual(RotatingFileHandler,
                         sys.modules['logging.handlers'].RotatingFileHandler)
        self.assertEqual(True, 'RotatingFileHandler' in
                         dir(sys.modules['logging.handlers']))


class CDoubleImportCheck(unittest.TestCase):

    def testASingleName(self):
        """Import a known module twice"""
        import random
        m = sys.modules['random']
        self.assertEqual(m, random)
        import random
        self.assertEqual(m, sys.modules['random'])
        self.assertEqual(m, random)

    def testBFromSingleName(self):
        """Import a known module twice using from import"""
        from random import gauss
        m = sys.modules['random'].gauss
        self.assertEqual(m, gauss)
        from random import gauss
        self.assertEqual(m, sys.modules['random'].gauss)
        self.assertEqual(m, gauss)

    def testCNestedName(self):
        """Import a known nested module twice"""
        import logging.handlers
        p = sys.modules['logging']
        m = sys.modules['logging.handlers']
        self.assertEqual(m, logging.handlers)
        self.assertEqual(p, logging)
        import logging.handlers 
        self.assertEqual(m, sys.modules['logging.handlers'])
        self.assertEqual(p, sys.modules['logging'])
        self.assertEqual(m, logging.handlers)
        self.assertEqual(p, logging)

    def testDFromNestedName(self):
        """Import a known nested module twice using from import"""
        from  logging.handlers import RotatingFileHandler
        m = sys.modules['logging.handlers'].RotatingFileHandler
        self.assertEqual(m, RotatingFileHandler)
        from  logging.handlers import RotatingFileHandler
        self.assertEqual(m, sys.modules['logging.handlers'].RotatingFileHandler)
        self.assertEqual(m, RotatingFileHandler)


class DImportSearchCheck(unittest.TestCase):

    def setUp(self):
        self.__savepath = sys.path
        self.__pathdirs = ['./roota', './rootb', './rootc', './rootd']
        self.__package = 'MyPackage'
        self.__modnames = ['mod1.py', 'mod2.py', 'mod3.py', 'mod4.py']
        self.__pathlist = []
        for d in self.__pathdirs:
            p = os.path.join(d,self.__package)
            os.makedirs(p)
            self.__pathlist.append(p)
        for i in range(3):
            os.system("touch %s" %
                      os.path.join(self.__pathlist[i], self.__modnames[i]))
            if i == 0 or i == 2:
                os.system("touch %s" %
                          os.path.join(self.__pathlist[i], self.__modnames[3]))
        os.system("touch ./rootd/MyPackage/__init__.py")
        sys.path = self.__pathdirs + self.__savepath

    def tearDown(self):
        for d in self.__pathlist:
            dirfiles = os.listdir(d)
            for f in dirfiles:
                os.remove(os.path.join(d,f))
            os.removedirs(d)
        sys.path = self.__savepath


    def testANotFound(self):
        """Import a package module that does not exist"""
        self.assertRaises(ImportError, importTestHelper, 'MyPackage.mod5')

    def testBFirstFound(self):
        """Import a package module that exists in the beginning of the search path """
        import MyPackage.mod1
        self.assertEqual(self.__pathlist[0],
                         os.path.dirname(MyPackage.mod1.__file__))

    def testCMiddleFound(self):
        """Import a package module that exists in the middle of the search path"""
        import MyPackage.mod2
        self.assertEqual(self.__pathlist[1],
                         os.path.dirname(MyPackage.mod2.__file__))

    def testDLastFound(self):
        """Import a package module that exists at the end of the search path"""
        import MyPackage.mod3
        self.assertEqual(self.__pathlist[2],
                         os.path.dirname(MyPackage.mod3.__file__))

    def testEDuplicateFind(self):
        """Import a package module that exists in two places in the search path"""
        import MyPackage.mod4
        self.assertEqual(self.__pathlist[0],
                         os.path.dirname(MyPackage.mod4.__file__))

    def testFDoubleImport(self):
        """Re-import a module that was found by a search"""
        import MyPackage.mod2
        m = sys.modules['MyPackage'].mod2
        self.assertEqual(self.__pathlist[1],
                         os.path.dirname(MyPackage.mod2.__file__))
        import MyPackage.mod2
        self.assertEqual(self.__pathlist[1],
                         os.path.dirname(MyPackage.mod2.__file__))
        self.assertEqual(m, sys.modules['MyPackage'].mod2)
    
    def testGFromNotFound(self):
        """Import a package module that does not exist using 'from import'"""
        self.assertRaises(ImportError, importFromTestHelper,
                          'MyPackage', 'mod5')

    def testHFromFirstFound(self):
        """Import a package module that exists in the beginning of the search path using 'from import'"""
        from MyPackage import mod1
        self.assertEqual(self.__pathlist[0], os.path.dirname(mod1.__file__))

    def testIFromMiddleFound(self):
        """Import a package module that exists in the middle of the search path using 'from import'"""
        from MyPackage import mod2
        self.assertEqual(self.__pathlist[1], os.path.dirname(mod2.__file__))

    def testJFromLastFound(self):
        """Import a package module that exists at the end of the search path using 'from import'"""
        from MyPackage import mod3
        self.assertEqual(self.__pathlist[2], os.path.dirname(mod3.__file__))

    def testKFromDuplicateFind(self):
        """Import a package module that exists in two places in the search path using 'from import'"""
        from MyPackage import mod4
        self.assertEqual(self.__pathlist[0], os.path.dirname(mod4.__file__))

    def testLFromDoubleImport(self):
        """Re-import a module that was found by a search using 'from import'"""
        from  MyPackage import mod2
        m = mod2
        self.assertEqual(self.__pathlist[1], os.path.dirname(mod2.__file__))
        from MyPackage import mod2
        self.assertEqual(self.__pathlist[1], os.path.dirname(mod2.__file__))
        self.assertEqual(m, mod2)
    

class EIDLImportCheck(unittest.TestCase):
    PmwModuleString = 'Pmw.Pmw_1_2.lib.PmwLoader'

    def testAModuleImport(self):
        """Import a single IDL interface module of a package"""
        import baci_idl
        self.assertEqual(False, sys.modules.has_key('ACS'))

    def testBPackageImport(self):
        """Import an IDL generated package"""
        import ACS
        self.assertEqual(hasattr(sys.modules['ACS'],'__file__'), True)

    def testCPackageReload(self):
        """Reload an IDL package on second import"""
        self.testAModuleImport()
        import ACS
        self.assertEqual(hasattr(sys.modules['ACS'],'__file__'), True)

    def testDLoaderImport(self):
        """Import a class loader"""
        import Pmw
        self.assertEqual(Pmw.__module__, self.PmwModuleString)

if __name__ == "__main__":
    unittest.main()


#
# ___oOo___
