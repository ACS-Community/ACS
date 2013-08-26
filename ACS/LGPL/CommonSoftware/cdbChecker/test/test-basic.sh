#! /bin/bash
#*******************************************************************************
# ALMA - Atacama Large Millimeter Array
# Copyright (c) UTFSM - Universidad Tecnica Federico Santa Maria, 2011
# (in the framework of the ALMA collaboration).
# All rights reserved.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#*******************************************************************************
config_path=$(searchFile config/reqSchemas.xml)
acsStartJava --noDirectory -endorsed -DACS.config_path=$config_path -DACS.cdbpath=$PWD/testdata/basic/good/ cl.utfsm.cdbChecker.CDBChecker -v -n $PWD/testdata/basic/bad/
echo return code is: $?
acsStartJava --noDirectory -endorsed -DACS.config_path=$config_path -DACS.cdbpath=$PWD/testdata/basic/bad/  cl.utfsm.cdbChecker.CDBChecker -v -n $PWD/testdata/basic/good/
echo return code is: $?
acsStartJava --noDirectory -endorsed -DACS.config_path=$config_path -DACS.cdbpath=$PWD/testdata/basic/good/ cl.utfsm.cdbChecker.CDBChecker -v -n $PWD/testdata/basic/good/
echo return code is: $?

