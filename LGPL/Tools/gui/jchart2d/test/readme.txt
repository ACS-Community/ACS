#*******************************************************************************
# E.S.O. - ACS project
#
# Notes for jchart2d test files
#
# who       when      what
# --------  --------  ----------------------------------------------
# bpanta    14-02-2011 created
#

1. General
As of ver. 3.2.0, the test files are not distributed in the in the main zip file 
(jchart2d-eclipse-project-3.2.0), but can be download from the SourceForge CVS by anonymous login:
cvs -d:pserver:anonymous@jchart2d.cvs.sourceforge.net:/cvsroot/jchart2d login 
Please check https://sourceforge.net/scm/?type=cvs&group_id=50440 for more information.



2. Unzipping and installing the test files 
When downloaded from the SourceForge CVS, the test files are already in proper folder 
hierarchy, starting with /info. There are 49 files in total. When uploading to the ALMA CVS, 
I zipped the files, so you need to unzip them. 

Unzip the jchart2DTest.zip under /test.(This is done in makefile- no manual unzip necessary.
The unzipped files will be stored under /info.

Here is how it looks like under gui/jchart2d/test:
/test
    /JChart2DTest.zip
    /JChart2DTestSuite
    /junit-jchart2d
    /Makefile       
    /readme.txt (this file)
    /Testlist.lite

3. Running tests
You run tests as usual using commands like make clean all and make test.

4. Notes 
  4.1 The following file has been removed from the testing battery because I could not make it work.
    /info/monitorenter/gui/chart/demos/TestRangePolicyHighestValues.java
    This file was edited: /info/monitorenter/gui/chart/demos/AllTests.java