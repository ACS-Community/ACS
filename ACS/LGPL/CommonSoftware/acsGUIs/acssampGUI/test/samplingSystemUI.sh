#!/bin/bash

echo "=== Running SSG test suite ==="
acsStartJava -endorsed alma.acs.testsupport.tat.TATJUnitRunner cl.SamplingSystemUIJUnitTest
echo "=== Done running SSG test suite ==="
