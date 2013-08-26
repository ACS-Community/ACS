#!/bin/bash

pushd ../src/shunit2-2.1.5/src/shell 1>/dev/null
./shunit2_test.sh -s /bin/bash
popd 1>/dev/null
