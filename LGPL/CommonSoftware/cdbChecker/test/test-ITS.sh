#! /bin/bash
config_path=$(searchFile config/reqSchemas.xml)
acsStartJava --noDirectory -endorsed -DACS.config_path=$config_path -DACS.cdbpath=$PWD/testdata/ITS:$PWD/testdata/defaultCDB/CDB/schemas cl.utfsm.cdbChecker.CDBChecker -v --network $PWD/testdata/ITS/ 
echo return code is: $?

