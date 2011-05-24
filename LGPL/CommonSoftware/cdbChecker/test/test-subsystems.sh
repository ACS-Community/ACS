#! /bin/bash
config_path=$(searchFile config/reqSchemas.xml)
acsStartJava --noDirectory -endorsed -DACS.config_path=$config_path -DACS.cdbpath=$PWD/testdata/subsystemCDBs:$PWD/testdata/defaultCDB/CDB/schemas cl.utfsm.cdbChecker.CDBChecker -vn $PWD/testdata/subsystemCDBs 
