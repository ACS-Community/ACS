#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2010 
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
# "@(#) $Id: test_Acspy_Nc_ReconnectionCallback.py,v 1.2 2010/03/20 22:46:40 agrimstrup Exp $"
#
# who         when        what
# --------    --------    ----------------------------------------------
# agrimstrup  2010-02-05  created
#

#------------------------------------------------------------------------------
__revision__ = "$Id: test_Acspy_Nc_ReconnectionCallback.py,v 1.2 2010/03/20 22:46:40 agrimstrup Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
import unittest
import mock
import CORBA
import NotifyExt
import Acspy.Nc.ReconnectionCallback as CB

class TestReconnectionCallback(unittest.TestCase):

    def setUp(self):
        self.callback = CB.ReconnectionCallback(mock.Mock())

    def test_init_no_factory(self):
        self.callback.init(None)
        self.assertEqual(False, self.callback.id_is_valid)

    @mock.patch_object(CB.ReconnectionCallback, '_this')
    def test_init_start_fault(self, poamock):
        def raiser():
            raise AttributeError()

        poamock.side_effect = raiser
        self.callback.init(mock.Mock())
        self.assertEqual(False, self.callback.id_is_valid)
        
    @mock.patch_object(CB.ReconnectionCallback, '_this')
    def test_init_ok(self, poamock):
        mockregistry = mock.Mock(spec=NotifyExt._objref_ReconnectionRegistry)
        mockregistry.register_callback.return_value = 1
        mockfactory = mock.Mock(spec=CORBA.Object)
        mockfactory._narrow.return_value = mockregistry
        self.callback.init(mockfactory)
        self.assertEqual(True, self.callback.id_is_valid)

    def test_disconnect_ok(self):
        mockregistry = mock.Mock(spec=NotifyExt._objref_ReconnectionRegistry)
        mockregistry.register_callback.return_value = 1
        mockfactory = mock.Mock(spec=CORBA.Object)
        mockfactory._narrow.return_value = mockregistry
        self.callback.ecf = mockfactory
        self.callback.id_is_valid = True
        self.callback.disconnect()
        self.assertEqual(False, self.callback.id_is_valid)

if __name__ == "__main__":
    unittest.main()

#
# ___oOo___
