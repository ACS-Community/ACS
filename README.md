# ACS CB Project

The master branch is used to build a RPM.
The "merges" branch merges the updates from ACS Community master branch and ALMA ACS's master branch, which then
are merged/cherry-picked to this project master branch.

## Git Structure

README.md
   This file

README-new-release
   Instructions to *manually* prepare a new release of ACS

Makefile
   The main Makefile to build, install and test ACS

ACS_VERSION
   The current ACS version

ACS_PATH_LEVEL
   The current ACS patch level

Benchmark
   ACS Benchmarking suite and tests

Documents
   Documentation tree

ExtProd
   External products pre-requisite for ACS build

LGPL
   Core ACS packages available as LGPL licence
