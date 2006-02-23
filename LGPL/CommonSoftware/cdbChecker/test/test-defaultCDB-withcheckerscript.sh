#!/bin/bash

# Here we use the cdbChecker script and the standard search path for schema files
export ACS_CDB=$PWD/testdata/defaultCDB
cdbChecker -v $PWD/testdata/defaultCDB/
echo return code is: $?

# _oOo_