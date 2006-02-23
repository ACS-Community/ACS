This directory contains the modular test for the module.

ATTENTION: The tests are not deterministic yet and some work is still
           needed to improve test resilience to timing problems.
           This is partially on purpose to allow to better grasp
           the basic principles behind modular testing with TestDriver
           and tat.

IMPORTANT: before running the test copy TestList_WS.lite to
           TestList.lite if you are testing on Linux or if you want to test
           the WS part on Sun. On Sun testing WS only also make sure
           to undefine WIND_BASE.
           On Sun testing LCU part copy TestList_BOTH.lite to TestList.lite.

The ws/test directory contains 2 type of tests:
- only WS (test clients and test server(s) run on WS
- both WS/LCU (test clients run on WS and server(s) on LCU)

If the environment variable WIND_BASE is defined both WS/LCU tests
will take place otherwise only WS tests.

Reference files:
The reference files are in ref_BOTH and ref_WS, respectively and the
subdirectory ref (e.g. ref_BOTH/ref and ref_WS/ref).
The Make file copies apropriate reference files into test/ and test/ref
depending on finding or not WIND_BASE environment variable. 

NOTE: If you generate new reference files you have to copy them into
appropriate directory (ref_BOTH or ref_WS and ref subdirectory).

TestList files:
There are two TestList.lite files:
 - TestList_BOTH.lite (Specifies two VLT environments: wsTat and lcuTat)
 - TestList_WS.lite (Specifies only one VLT enviroment: wsTat)
Depending on definition of enviroment varibale WIND_BASE the make file copies
one of them to TestList.lite


TestDriver file:
The TestDriver file defines the RTAPENV environment variable in
"WS only" (WIND_BASE is not defined) case it takes value of "wsTat" in
"BOTH" case (WIND_BASE is defined) it takes value of
"lcuTat" from the .testSession file.



HOW TO RUN TESTS

1) TAT IS CONFIGURED ON YOUR MACHINE:
  - To run the test in an automatic way, tat should be configured
    on the machine used for the test.
    See tat documentation for details:
      Tools for Automated Testing User Manual
      VLT-MAN-ESO-17200-0908
    and eccsTestDriver man page.
 
    In this case, just say on the command line:
 
    > tat -nc

  - To just generate the environment for the test:
    > tat -nc makeEnv

  - To just run a test once the environment is running:
    > tat -nc
    or
    > TestDriver

  - To cleanup the environment
    > tat -nc cleanEnv

    When testing LCU part on Sun first run "tat makeEnv" before really
    running the test. This compiles the test and creates the environments.
    After that you need to modify the LCU database. 
    Modify in LCUEnv/dbl the file USER.db, change the name of the WS 
    where the manager is running in the ManagerReference and insert the
    proper name of the LCU in CommandLine.


2) TAT NOT CONFIGURED

  - You must have an environment already running and with the
    correct configuration database.
    The standard environment walma already has the right structure.

  - If not, the USER.db file for the configuration database is in:

    <module>/test/ENVIRONMENTS/wsTat/dbl/USER.db

    Use this file to generate your environment.

  - Build the test software:

    > make -k all

  - Generate by hand the .testSession file in <module>/test.
    It must have the following content:

    QS wsTat walma
   
    Where walma has to be replaced by the name of your environment.

  - To just run a test once the environment is running:
    > TestDriver

3) Tests are listed in TESTLIST.td file (see eccsTestDriver man page).
   Per each test there is:
   - a test number
   - a test name
   - a list of executables, with eventually @SLEEP commands

4) For each test 
   - there is a <test name>.ref reference file
     The output from each executable is preceeded by an identification number.
   - test execution generates a <test name>.rep report file
   - if the test fails, a <test name>.diff difference file is generated
     This file is an ediff style file sorted so that only lines are
     considered and not difference in execution order.
