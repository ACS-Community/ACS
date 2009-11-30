#!/bin/bash

echo "Checking a CDB that contains a bad idl types"
cdbChecker -n -c $PWD/testdata/bad-IR

sleep 5
echo "Checking a CDB that contains all good idl types"

cdbChecker -n --checkIdlTypes $PWD/testdata/good-IR
sleep 3
