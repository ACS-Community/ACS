#!/bin/bash

config_path=$(searchFile config/reqSchemas.xml)
acsStartJava --noDirectory -endorsed -DACS.config_path=$config_path -DACS.cdbpath=$PWD/testdata/defaultCDB/CDB/schemas cl.utfsm.cdbChecker.CDBChecker -v $PWD/testdata/defaultCDB/
echo return code is: $?
