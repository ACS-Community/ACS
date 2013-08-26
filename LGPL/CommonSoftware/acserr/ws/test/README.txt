This directory contains the modular test for the module.


LCU enviroment variable shoudl be set manually if TestDriver is used



IMPORTANT: before running the test copy TestList_WS.lite to
           TestList.lite if you are testing on Linux or if you want to test
           the WS part on Sun (doe by makefile)
	    On Sun testing WS only also make sure to undefine WIND_BASE.
           On Sun testing LCU part copy TestList_BOTH.lite to
           TestList.lite. (done by makefile)

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



When testing LCU part on Sun first run "tat makeEnv" before really
running the test. This compiles the test and creates the environments.
After that you need to modify the LCU database. 
Modify in LCUEnv/dbl the file USER.db, change the name of the WS 
where the manager is running in the ManagerReference and insert the
proper name of the LCU in CommandLine.


