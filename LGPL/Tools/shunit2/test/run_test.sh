#!/bin/bash

pushd ../src/ 1>/dev/null
tar zxf shunit2-2.1.5.tgz 1> /dev/null
cd shunit2-2.1.5/src/shell
./shunit2_test.sh -s /bin/bash
popd 1>/dev/null
