#! /bin/bash
config_path=$(searchFile config/reqSchemas.xml)
acsStartJava --noDirectory -endorsed -DACS.config_path=$config_path -DACS.cdbpath=$PWD/testdata/basic/good/ cl.utfsm.cdbChecker.CDBChecker -v -n $PWD/testdata/basic/bad/
echo return code is: $?
acsStartJava --noDirectory -endorsed -DACS.config_path=$config_path -DACS.cdbpath=$PWD/testdata/basic/bad/  cl.utfsm.cdbChecker.CDBChecker -v -n $PWD/testdata/basic/good/
echo return code is: $?
acsStartJava --noDirectory -endorsed -DACS.config_path=$config_path -DACS.cdbpath=$PWD/testdata/basic/good/ cl.utfsm.cdbChecker.CDBChecker -v -n $PWD/testdata/basic/good/
echo return code is: $?

