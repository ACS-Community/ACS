#! /bin/bash
config_path=$(searchFile config/reqSchemas.xml)
acsStartJava --noDirectory -endorsed -DACS.config_path=$config_path -DACS.cdbpath=$PWD/testdata/good-FE/ cl.utfsm.cdbChecker.CDBChecker -v -n $PWD/testdata/bad-FE/
echo return code is: $?
acsStartJava --noDirectory -endorsed -DACS.config_path=$config_path -DACS.cdbpath=$PWD/testdata/bad-FE/ cl.utfsm.cdbChecker.CDBChecker -v -n $PWD/testdata/good-FE/
echo return code is: $?
acsStartJava --noDirectory -endorsed -DACS.config_path=$config_path -DACS.cdbpath=$PWD/testdata/good-FE/ cl.utfsm.cdbChecker.CDBChecker -v -n $PWD/testdata/good-FE/
echo return code is: $?

