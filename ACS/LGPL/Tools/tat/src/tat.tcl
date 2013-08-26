#************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tat.tcl,v 1.113 2012/01/17 11:57:11 tstaig Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  07/07/95  created
# fcarbogn  29/10/97  SHLIB_PATH/LD_LIBRARY_PATH setting (SPR970404)
# gfilippi  07/05/98  TestList.NOCCS added (VLTSW980249)
# fcarbogn  23/06/98  Upgrade to New CCS_LITE
# fcarbogn  22/07/98  Passed  WSEnv to tatCleanLCUEnv
# fcarbogn  22/07/98  $LCUROOT check only if LCU Env. is specified on Testlist
# fcarbogn  06/10/98  Set ./tatlog for .out, .diff and TatCreateEnv (SPR960627)
# fcarbogn  02/06/99  Eliminated vcc timeout settings (SPR 990194)
# fcarbogn  10/08/99  bug fixing for make possible RWS from CCSLite Workstation
# psivera   24/05/01  added RQS creation and RHOSTQS variable; fixed bug in tat cleanEnv 
#		      when the first env in TestList is LCU 
# psivera   09/07/02  VLTSW20020160: make all is executed before preparing the TestList
# psivera   09/07/02  added prepareTestList proc to cleanEnv proc
# psivera   05/12/02  fixed bug when cleaning LCU env without WS env
# eallaert 2003-06-11 replaced several exec file-operations by Tcl "file" cmds
# psivera  2003-08-06 SPR 990447 and 20030339: handling of logLCU.config file
# psivera  2003-08-06 SPR 990481: added module include dir to the search path 
# psivera  2003-08-08 SPR 990447 and 20030339 bit moved to tatMakeQsemuEnv.tcl
# psivera  2003-10-22 added module's lib/python/site-packages to PYTHONPATH if exists
# psivera  2003-12-04 tat-eccs merging: restored chmod for scripts under test dir; 
#                     corrected bug to find out if NOCCS is defined. 
# psivera  2004-01-19 added [pwd] to some printings when failure is detected
# psivera  2004-02-13 fixed tcl procheck warnings
# psivera  2004-02-16 corrected bug when parsing the keyword "all" (& C.)
#                     passed from command line
# psivera  2004-03-16 ALMASW2003086 solved: ../bin and ../lib are created if not existing
# psivera  2004-06-22 SPR 20040140: the outputFile is saved in .orig before 
#                     being processed with sed and grep
# eallaert 2005-02-08 SPR 200500442: include sleep while waiting for test termination
# sfeyrin  2005-08-12 SPR 20050196: take into account .TestList.sed in sedFilterx
# sfeyrin  2008-11-06 Copying ENVIRONMENTS directories to tatlog directory (VLTSW20080252)
# sfeyrin  2009-02-04 fixed bug when using -r option
# sfeyrin  2009-04-28 added catch in copyEnv when copying ENVIRONMENTS
# sfeyrin  2010-09-16 - Merge with ALMA tat 1.111:
#                     Added ../idl to IDL_PATH; Inserted INTLIST
#                     egrep exits only if return code is 2
# 		      ALMASW2005120: the source script is sourced before running doCleanEnv
#                     ALMASW2005119 and ALMASW2006007 implemented
#                     ALMASW2004075 implemented: test lines in TestList(.lite) can be 
#                         splitted across multiple lines
#                     ALMASW2005086: epilogue script is always executed after a failure of prologue 
#                     ALMASW2003069: removed the "child process" message from the diff file
#                     ALMASW2006015: implemented option -f NewTestList
#                     - Added check SessionFile empty in copyEnv
# sfeyrin  2010-10-20 time added for each test step
# sfeyrin  2011-01-26 VLTSW20090238: move allocEnv before sourceScript
#
#
#************************************************************************
#   NAME
#   tat - automated test driver
# 
#   SYNOPSIS
#   tat [-v ] [-nc] [-g[enerate]] [-run] [-log] [-noorder] [-order] 
#      [-r num] [-w secs] [-f filename]
#      [all | ALL | makeEnv | cleanEnv | [ <testName> ...] | [<testNum> ...] ]
# 
#   DESCRIPTION
#
#   Looks in the current directory, normally "test", for a file named: 
#     -TestList (the default name)
#     -for the VLT project: 
#       TestList.lite on a CCSlite installation ($RTAPROOT, $NOCCS not defined)
#       TestList.NOCCS on a NOCCS installation ($NOCCS defined)
#     -for the ALMA project, you can indifferently use TestList or TestList.lite
#   The file specifies the scripts to be sourced and/or run to prepare the environment, 
#   QS/RQS/LCU environments to be created (for the VLT project), 
#   the tests to be run, 
#   the scripts to be run to clean-up at the end of the test
#   tat executes the following steps:
#
#   - the environment variable SHLIB_PATH/LD_LIBRARY_PATH is redefined 
#     to module lib directory first
#   - the environment variable PYTHONPATH (for ALMA) is redefined 
#     to module lib/python directory first
#   - the environment variable PATH is redefined to module binaries first
#   - 'make clean all' is executed in the (ws and lcu) test directories
#     to build the test binaries
#   - test environment is prepared: env variables defined, 
#     preparation scripts (prologue) executed
#   - Real time environments (VLT) are allocated and created
#   - tests scripts are run (a test script may actually be any executable 
#     file in PATH).
#   - real time environments (VLT) are deleted and released. 
#   - test environment is cleaned-up (epilogue script is executed)
#   - 'make clean' is executed in ws and lcu test directories
#
#   The standard output and the standard error of test scripts are first 
#   processed by :
#
#   - grep -v -f TestList.grep <output>
#   - sed -f TestList.sed <output>
#
#   and then are redirected into ./tatlogs/run<process_id>/<testName>.out
#   If this file is the  same as ./ref/<testName>.ref, tat prints 
#   "TEST <testName> PASSED" otherwise tat prints "TEST <testName> FAILED". 
#   For the VLT project: note that for each allocated environment, tat 
#   subsitutes the allocated environment name by the corresponding environment 
#   variable in the file
#   ./tatlogs/run<process_id>/<testName>.out and ./ref/<testName>.ref.
#   
#   The output files before the parsing with TestList.grep and TestList.sed 
#   are saved as well in ./tatlogs/run<process_id>/<testName>.out.orig
#
#   Note the matching between the test output name, the reference file name 
#   and the file where the differences are stored
#
#   - output of test script: ./tatlogs/run<process_id>/<testName>.out
#   - differences between reference file and output file: 
#   ./tatlogs/run<process_id>/<testName>.diff
#
#   VLT: In case of failures during RT environments creation, error messages 
#   are saved under ./tatlogs/run<process_id>/<env> along whith the whole 
#   environment for further checking, the RT environment under 
#   $VLTDATA/ENVIRONMENT is always removed 
#
#   TestList SYNTAX
#
#   - blank lines are allowed
#
#   - comment lines start with '#'
#
#   - a test directive can be splitted across multiple lines, provided 
#     the continuation character "\" is used
#
#   - SOURCE <tcl_script>
#  
#   With this directive one has the possibility to source all the 
#   environment variables needed fot the test time frame.
#   Those environment variables must be defined in a tcl script
#   following tcl syntax. The file will be sourced after make all
#   and before doing the environments.
#
#   - PROLOGUE <exe_or_script_name> [ <arg1> <arg2> ... ]
#
#     With this directive, one can specify the name of a program (script 
#     or executable) with optional arguments that has to be run after 
#     'make all' and before the environments creation.
#   
#   - ENVIRONMENT <envName> <type> specifies a environment to be created (VLT only);
#
#   type QS : qsemu local environment (CCS lite only)
#   type WS : local RTAP environment on the local machine
#   type RWS: remote RTAP environment on a remote machine 
#             (RHOST should be set)
#   type RQS: remote QSEMU environment on a remote machine 
#            (RHOSTQS should be set)
#   type LCU: LCU environment
#
#   Two possible syntax for lines containing the actual test programs 
#   are allowed:
#   1. (recommended)
#   - <testNum> [SOURCE <tcl_script>] [PROLOGUE <exe_or_script_name> [ <arg1> <arg2> ... ]] 
#               <testName> <proc 1> [<proc 2>..<proc n>]
#               [EPILOGUE <exe_or_script_name> [ <arg1> <arg2> ... ]]
#                 where:
#                    <testNum>  is the number assigned to the test.
#                          Usually every record has a different number
#                          so that they can executed individually.
#                    <testName> is a symbolic name used to identify the test
#                    <proc 1>...  is a list of programs to be executed
#                          in order to perform the test.
#                          It is required to have at least one program.
#                          All programs in the list are executed in
#                          background, except the last that is executed
#                          in foreground.
#                 To pass parameters to the programs it is enough to
#                 write the complete command line for the program
#                 <proc n> in quotes.
#                 Consider the following sample record:
#
#                     1  MyTest "test -a dfs" test_slave
#
#                 Instead of a program to be executed, it is possible to give
#                 the special directive "@SLEEP n" where n is a number of 
#                 seconds to wait before going on with the parsing of the 
#                 line.
#
#                 This can be used whenever it is necessary to wait for the
#                 proper startup of a process before going on with the 
#                 exeuction of the other processes.
#
#                 Typically this is useful when the first process in the list
#                 performs some initialization (for example writing initial 
#                 values in the database to make deterministic the execution 
#                 of the other components of the test.
#
#                 What follows is a sample record:
#
#                     1  MyTest initDb "@SLEEP 5" "test -a dfs" test_slave
#                 This means that when initDb is executed, a sleep of 5 second 
#                 is simultaneously executed.
#       
#                 Every test case can also be preceded by its own SOURCE script 
#                 or PROLOGUE or EPILOGUE script.
#
# To summarize the usage of the first type of syntax for the test case:
# - All processes you enter in a TestList record are executed in BACKGORUND, 
#   but the LAST ONE, which is executed in FOREGROUND
# - When the last process (the one in foreground) exits, all other processes 
#   in the record are killed (if still alive) to cleanup the test.
# - It is possible to put "@SLEEP xx" directives between any two processes 
#   to give the one on the left some time to initialise cleanly 
#   (in background) before the one on the right is started.
# - @SLEEP should NEVER be the last directive in a TestList record.
# - The last action in a record shall always be a command/script that drives 
#   the test.
# - If you need to wait some seconds at the end of a test you can use the 
#   option -w num (which is valid for every test in the TestList, though).
#
#
#   2. (left for backward compatibility)
#   - <testName> specifies the script to be called to run the test (no
#   parameters allowed). 
#   If the testscript has the ".vw" suffix, tat assumes that it's
#   a VxWorks script and it's executed on the LCU environment
#   listed in TestList. Tat filters automatically the name from the
#   LCU and lines containing "value = " from the VxWorks shell output.
#
#   -<envName> is the name of the environment variable defining the envir (VLT).
#   name: it is NOT the name of the environment. Environments are dynamically
#   allocated: there is no way to use a given RTAP environment or use a given
#   LCU environment. During test execution, RTAPENV is assigned to the first
#   WS environment described in TestList and LCU is assigned to the first LCU
#   described in TestList (the value of LCU is the name of the LCU not the
#   name of the LCU environment). 
#
#   - EPILOGUE <exe_or_script_name> [ <arg1> <arg2> ... ]
#
#     With this directive, one can specify the name of a program (script 
#     or executable) with optional arguments that has to be run after 
#     stopping and deleting the environments and before 'make clean'
#   
#   Commented line begin with the # character.
#
#   ENVIRONMENT CREATION (VLT)
#
#   Environments are built from scratch. The module test subdirectory shall
#   contain a ./ENVIRONMENTS/<envName> subdirectory for every environment
#   listed in TestList. Note that even if such a  directory is empty, it shall
#   still exist; in this case, standard templates are used. Otherwise files 
#   needed to create the environment referenced by <envName> are copied from 
#   ./ENVIRONMENTS/<envName>.
#
#   For a QS/RQS environment, all the files under ./ENVIRONMENTS/<envName> 
#   are taken into account and have priority. 
#   Files under <module>[/ws]/dbl are also taken into account by dbl: 
#   instead of being copied into the target directory, they are searched by 
#   dbl when executing 'make db' using the MODINC environment variable.
#
#   If these files don't exist, files are taken from the following directory:
#   $VLTROOT/templates/forEnvs/QSEMU.
#
#   For a LCU environment, all the files under ./ENVIRONMENTS/<envName>
#   are taken into account and have priority. This includes userScript,
#   logFile, rebootFile, devicesFile, dbl/Makefile, dbl/*.db, etc.
#   The only exception is that bootScript should NOT exist under 
#   ./ENVIRONMENTS/<envName>: if you want to load specific modules, add them 
#   to userScript using lcuboot services. bootScript is always generated 
#   at run time and cannot be directly modified. bootScript executes
#   userScript before starting LCC.
#
#   If these files don't exist, files are taken from the following directory:
#   $VLTROOT/templates/forEnvs/LCU.
#
#   Note that userScript should not contain any reference to the WS/RWS 
#   environment: the statements defining VLTROOT, VLTDATA, HOSTNAME, HOSTENV, 
#   HOSTIPADDR, hostTCPPort, LOCALHOST, LOCALENV, LOCALIPADDR, localTCPPort,
#   etc., are automatically generated by the lcuboot into bootScript. 
#
#   HANDLING OF logLCU.config FILE (VLT)
#   After implementation of SPRs 990447 and 20030339, the user can choose 
#   whether to write itself the logLCU.config file under the 
#   test/ENVIRONMENT/ws_env_name directory or let tat prepare the file.
#   In the second case, tat will use the first WS environment found in 
#   the TestList* files; if no WS environments have to be created, 
#   tat will take the default RTAPENV.
#
#   SCAN SYSTEM CONFIGURATION (VLT)
#
#   If the file ./ENVIRONMENTS/<envName>/dbl/USER.db.scan contains references 
#   to the environment variables for the environments (<envName>), these 
#   references are subsituted with the allocated environment names before 
#   building the DB. Note that :
#   - this works only with USER.db.scan (not with USER.db !)
#   - a reference is just the environment variable name without any prefix 
#   or suffix (no '$' !).
#
#   TEST OF VXWORKS BINARIES (VLT)
#
#   If not already defined, tat defines MODROOT and it is possible to load
#   additional vxWorks modules using lcuboot functions by adding them
#   to userScript. Since lcuboot adds $MODROOT/bin to BINPATH), MODROOT is set
#   to <module>/lcu for a module having both "ws" and "lcu" subdirectories, 
#   otherwise MODROOT is set to <module>. 
#
#   OPTIONS
#
#   makeEnv: run 'make clean all ' in ws and lcu test directories; allocates 
#   all the WS and LCU environments and creates them: RTAP environments have 
#   been created and started, LCU have been allocated and rebooted. 
#   'makeEnv' step fails the target directory is not writable by the current 
#   user. 
#
#   (testNum> (if the first type of syntax is used) or
#   <testName> (if the second type of syntax is used): 
#   run only the script <testNum> or <testName>. This option assumes that
#   'tat makeEnv' has been run before. tat redefines the envir. variables
#   specified in TestList so that the environments created by makeEnv can be
#   used. If 'tat makeEnv' has not been run, the message " cannot open 
#   .testSession" is displayed and tat exits. Several test id. may be
#   specified. If needed, the test script is made executable.
#
#   cleanEnv: frees and cleans all the environments allocated by makeEnv 
#   option. RTAP environments are shut down, corresponding directories are 
#   deleted. LCU boot directories are deleted; run 'make clean'.
#
#   all or ALL: this is the option by default. If no test session is active,
#   it's equivalent to the following options sequence: 
#
#       - 'make clean all'
#       - makeEnv
#       - run all tests
#       - cleanEnv. 
#       - 'make clean'
#
#   If a test session is active, this option executes all test listed
#   in TestList.
#
#   -v: verbose mode. Displays allocate environment and detailed
#   steps of environment creation and deletion.
#
#   -f filename: when one does not want to use the default TestList(.lite)
#   file, one can pass whatever filename (which has to be in the format of 
#   a TestList, with SOURCE, PROLOGUE, testid, EPILOGUE). At the same 
#   time, one can use as well filename.grep and filename.sed to filter 
#   the output files. If filename.grep and filename.sed are not found, 
#   the default ones are used. 
#
#   -g[enerate]: generates the test reference in ./ref/<test>.ref. 
#   A former reference file is overwritten. 
#   The only other valid option with -g are 'all', '-g' and 
#   <testNum> (if the first type of syntax is used) or
#   <testName> (if the second type of syntax is used): 
#   If <testNum> or <testName> is specified, the makeEnv and cleanEnv steps 
#   are not executed. 
#   By default, all tests specified in TestFile are executed.
#
#   -nc: Run 'make all' instead of 'make clean all' before environments
#   allocation and creation; do not run 'make clean' after test execution.
#   
#   -l: allows to loop on a set of test directories by using the
#   the same workstation environment and the same LCU environment.
#   All test directories must be under the current directory and are all
#   taken into account. All test directories MUST have the same environment 
#   variable names for WS/LCU environments (./ENVIRONMENTS/<envVar>) in 
#   TestList. In the first test directory, tat runs 'makeEnv'; in the last 
#   test directory, tat runs 'cleanEnv'; in all test directories, the LCU is 
#   rebooted once and all tests are run. 
#   Verbose option should by used to print current directory under test. 
#   Note that it is not yet possible to specify the first directory to be 
#   tested and that environments may not be properly released in case of 
#   interruption or internal error.
#
#   -log      : takes into account also output of the CCS log system
#               (not implemented for ALMA)
#
#   -noorder  : does not take into account the order in the sequence of
#               output lines (default)
#
#   -order    : takes into account the order in the sequence of
#               output lines
#
#   -run      : execute the test and compare with reference files
#   If the option -run (default) is used the actual tests are performed
#   and the output of the test is compared with the reference output
#   mentioned previously.
#   This option is like all|ALL and has been left for backward compatibility
#
#   -r        : number of times the test must be repeated (def == 1)
#               in case of failure, the test(s) is always repeated r times
#               the output files before and after the parsing with 
#               TestList.grep and TestList.sed and the .diff files are saved 
#               at each iteration in output_n.ext where n runs from 1 to r. 
#
#   -w        : number of seconds to wait at the end of a test before
#               killing the processes still alive.
#               Very usefull when running purecov, since it is necessary
#               to give the processes enough time to produce coverage
#               reports.
#
#   <testNum> : execute only the selected (by number) tests
#
#
#   RETURN VALUES
#
#   0: tat succeeded in executing the test (wether tests have failed
#      or not)
#   1: tat failed to execute one test and managed to release allocated 
#      environments
#   2: tat interrupted
#
#   ENVIRONMENT VARIABLES
#
#   If not defined, tat define MODROOT (see test of VxWorks binaries).
#   NO_ERR_DISPLAY is always set to TRUE.
#
#   LCUROOT is the directory of the LCUs pool.
#
#   RTAP_WAIT specifies the timeout for a RTAP environment starting;
#   default is 60 seconds (for both local and remote environments).
#
#   Remote environments are only supported on one machine defined
#   by the RHOST environment variable. Default value of RHOST is $HOST.
#
#   VLT_VCCBOOTTIME specifies the timeout for a LCU to boot vxWorks or
#   to execute its bootscript. Default value is 300 seconds. Can be overriden
#   only with greater values.
#
#   If defined, TAT_MAKE_ARGS is passed as additional argument to the 
#   'make [clean] all' command.
#
#   TEST_WAIT specifies the timeout for a single test in seconds:
#   if not defined. there is no timeout. If defined, every test in
#   TestList running more than $TEST_WAIT seconds is killed by tat.
#
#   REPEAT: it's an integer number from 1 on. Default is 1.
#   If it is defined, when running tat, all tests in the suite are repeated 
#   $REPEAT number of times. If the option -r is used from command 
#   line, the -r option overrides the env variable $REPEAT. 
#
#   CAUTIONS
#
#   The words "all", "ALL", "makeEnv" and "cleanEnv" are reserved. 
#   You should not have tests called with one of those words. 
#   
#   ------------------------------------------------------------------------
#
#   Each sub directory of ./ENVIRONMENTS should be owner writable.
#
#   ------------------------------------------------------------------------
#
#   Environments allocated by running 'tat makeEnv' are booked until
#   the command 'tat cleanEnv' is run. When developing a test, special
#   care should be taken not to book "forever" environment ressources.
#
#   ------------------------------------------------------------------------
#
#   Do not mix TestList.grep and TestList.sed syntax: it's the best way
#   to get a empty reference file.
#
#   ------------------------------------------------------------------------
#
#   RtapEnvTable is not taken into account: use RtapEnvTable.normal
#   Do not customize bootScript: use userScript.
#
#   ------------------------------------------------------------------------
#
#   Never hard code in your test reference file, any host name or address, 
#   environment name, port number or even user name or file name refering
#   to your test directory. 
#
#   ------------------------------------------------------------------------
#
#   It should be mandatory for the user to re-test once its module tests have
#   been archived, possibly from a different computer using different 
#   environments.
#
#   ------------------------------------------------------------------------
#
#   When writing the TestList.lite (or TestList), first put the WS envir.
#   then the LCU environments. Use blanks, not tabs (at list one blank) 
#   to separate the symbolic name of the environment and the type of env, 
#   for instance: 
#   ENVIRONMENT wsTat  QS
#
#   ------------------------------------------------------------------------
#
#   When writing the logLCU.config file, use blanks, not tabs (at list one
#   blank) to separate LCU and WS environment names. 
#
#   ------------------------------------------------------------------------
#
#   When passing the name of a test (not the number), the name should not 
#   begin with a number!
#
#   ------------------------------------------------------------------------
#
#   The order of execution of the different numbered tests or resp. lettered 
#   is the one written in the TestList[.lite] file
#  
#   ------------------------------------------------------------------------
#
#   The option -x has not been maintained while performing the merge 
#   with eccsTestDriver
#
#   ------------------------------------------------------------------------
#
#   There is not check on the correctness of the env variable REPEAT 
#   which has to be an integer number. 
#  
#   ------------------------------------------------------------------------
#
#   Be careful not to have a continuation line character (\) at the end 
#   of the last test line in the TestList(.lite).
#
#   ------------------------------------------------------------------------
#
#   When a SOURCE script is used for a specific test case, besides the SOURCE 
#   script used for all test cases, for example: 
#    
#     SOURCE generalTclScript
#     1 SOURCE myTclScript myTestName procToRun 
#     2                    mySecondTestName secondProcToRun
#
#   and the tests in the TestList are executed all in one go, then the variables 
#   set in the test case 1 by myTclScript will be valid for the following test cases 
#   as well. 
#   In other words, what has been set in 1 is not un-set before running 2.
#   In the example above, also the test mySecondTestName will benefit of the 
#   same environment variables set during the execution of myTestName. 
#   If you do not want those variables set, you should prepare another SOURCE script 
#   to put in front of mySecondTestName, redefining or unsetting the desired variables.
#   Note that if you run the test cases one by one, using the command: 
#      tat 1 
#      tat 2 
#   in this case, when running tat 2 the environment variables set in 1 are lost.
#   Note also that by design (it was requested in an SPR), the SOURCE script 
#   used at the beginning of the TestList is always re-sourced before any test case, 
#   so that the environment variables are always valid, whether the tests are run 
#   all in one go with one command, or one by one. 
#   So in the example above, the environmnet variables defined in generalTclScript 
#   will be active for test 1 and test 2, whether these tests are run with one command: 
#      tat 
#   or one by one: 
#      tat 1
#      tat 2
#
#   ------------------------------------------------------------------------
#
#   BUGS     
#
#   Files under $MODROOT/dbl are _not_ taken into account by makeEnv step
#   if they already exist under $VLTROOT/dbl or $INTROOT/dbl.
# 
#   If environment variables defined for WS/RWS/LCU environments have
#   similar patterns so that one name is a sub pattern of another name
#   (ex: WSname and RWSname), the scan system configuration fails.
#
#   Tabs instead of blanks in TestList triggers a syntax error.
#
#   An echo statement in a init file (.cshrc for example) makes tat fails
#   because vccEnvCreate fails.
#
#   Tat does not work if executed in background (it seems that RtapUnlockExe
#   hangs in this case).
#
#   Tat must use the sequencer tcl shell (seqSh) because it uses vccLib 
#   procedures: this means that it is not possible to use tat to test the 
#   sequencer with Purify.
#
#   SEE ALSO
# 
#   tatEnvStatus(1), vccEnv(1), envsCreate(1), ENVIRONMENTS_RTAP(5),
#   ENVIRONMENTS_LCU(5), ENVIRONMENTS_QSEMU(5).
#----------------------------------------------------------------------------
#
#

# Files with grep and sed patterns
set gv(grepFile)     "TestList.grep"
set gv(grepFileBis)  ".TestList.grep"
set gv(sedFile)      "TestList.sed"
set gv(sedFileBis)   ".TestList.sed"
set gv(sessionFile)  ".testSession"

##############################################################################
# proc usage
##############################################################################

proc usage {} {
    error "usage: tat \[-v\] \[-t\] \[-nc\] \[-g\] \[-order\] \[-noorder\] \[-run\] \[log\] \[-r num\] \[-w secs\]  \[test nums\] \[makeEnv\] \[cleanEnv\] \[all\] \[ALL\]"
}

##############################################################################
# proc mvFiles: to change access right for cmmCopied files
##############################################################################

proc mvFiles {fromFile toFile } {

    if { [file exists $toFile] && ![file writable $toFile] } {
	    if { [catch {chmod u+w $toFile}] } {
		error "cannot chmod u+w $toFile"
	    }
	} 
    catch {file rename -force -- $fromFile $toFile}   
}

##############################################################################
#
# proc buildLists
#
# parses TestDriver to build the list of the environments to be created
# and the list of the tests to run.
#
##############################################################################

proc buildLists {} {

    global env
    global gv

    set gv(envNameList) {}
    set gv(testList) {}
    set gv(sourceScript) {}
    set gv(prologueScript) {}
    set gv(prologueArgs) {}
    set gv(epilogueScript) {}
    set gv(epilogueArgs) {}
    set nokLineNb 0
    set longLine {}

    # Open the file for read
    set fd [open $gv(testListFileName) r]

    # Loop over the lines in the file.
    # Each line correspond to a test
    while { [gets $fd line] >= 0} {

        # Skips comments and empty lines
	if {[regexp {^[ \t]*#.+|^#$} $line] != 0} {
	    continue
	}
	if {[regexp {^[ \t]*$} $line] != 0} {
	    continue
	}

	# Save in longLine a line with continuation character
	if {[string equal [string index $line end] \\]} {
	    append longLine " [string range $line 0 end-1]"
	    continue
	}

        # convert the current line into a list of words	

	set line [concat $longLine $line]
	set longLine {}

	# remove all blank sequences with more than one blank
	regsub -all {[ ]+} $line " " newline1
	# do it also for the end of the string ...
	set newline [string trimright $newline1]
	set ll [split $newline ]
	set lll [llength $ll]
	set i 0

	# check list of words and update envNameList and testList
	set nokLineNb 0
        set tlfn $gv(testListFileName)
	while  { $i < $lll }  {

	    set lineOK 1
	    set word [lindex $ll 0]  
	    if { $word == "ENVIRONMENT" } {
		if {$lll != 3} {
		    puts "$tlfn: $line: invalid line"
		    set lineOK 0
		    break
		}
		set word2 [lindex $ll 1]
		set word3 [lindex $ll 2]
		if { $word3 != "LCU" && $word3 !="WS" && $word3 != "RWS" && $word3 != "QS" && $word3 != "RQS"} {
		   puts "$tlfn: $line: invalid environment type"
		   set lineOK 0
		   break
		}
		if { $gv(withRtap) == 1} {
		   if { $word3 == "QS" } {
		     puts "$tlfn: $line: invalid QS environment type with RTAP"
		     set lineOK 0
	             break
		   }
	        } elseif { $word3 == "WS" } {
		   puts "$tlfn: $line: invalid (R)WS environment with no RTAP"
	           set lineOK 0
	           break
	        }
		set gv(envNameList) [concat $gv(envNameList) $word2 $word3]
		break
	    } elseif { $word == "SOURCE" } {
		if {$lll < 2} {
                    puts "$tlfn: $line: invalid line"
                    set lineOK 0
                    break
                } else {
                    set gv(sourceScript) [lindex $ll 1]
		}
	    } elseif { $word == "PROLOGUE" } {
		if {$lll < 2} {
                    puts "$tlfn: $line: invalid line"
                    set lineOK 0
                    break
		} else {
		    set gv(prologueScript) [lindex $ll 1]
		    set gv(prologueArgs) [lrange $ll 2 end]
                }
	    } elseif { $word == "EPILOGUE" } {
		if {$lll < 2} {
                    puts "$tlfn: $line: invalid line"
                    set lineOK 0
                    break
		} else {
		    set gv(epilogueScript) [lindex $ll 1]
		    set gv(epilogueArgs) [lrange $ll 2 end]
                }
	    } else { 
		set testLine [string trim $line]
		#set testLine [lrange $ll 0 end]
		lappend gv(testList) $testLine
		#incr i
		break
	    }
	    incr i
	}

	if { $lineOK == 0 } {
	    incr nokLineNb
	}

    }

    close $fd
    if { $nokLineNb == 0 } {
	return 0
    } else {
	return 1
    }

}

#*******************************************************************
# proc printLog {str}         prints on standard output the given string
#                           properly formatted
# proc printLogVerbose {str}  like printLog, but prints only if verbose == 1
#
#*******************************************************************
proc printLog {str} {
    puts "$str"
}

proc printLogVerbose {str} {
    global verbose
    if {$verbose == 1} {
        puts "$str"
    }
}


###############################################################################
# checkLists
#
# checks TestList consistency with directory status
###############################################################################

proc checkList {} {

    global gv

    set i 0
    set le [llength $gv(envNameList)]
    while { $i < $le } {
	set envName [lindex $gv(envNameList) $i]
	incr i
	set envType [lindex $gv(envNameList) $i]
	incr i

	set envDir ./ENVIRONMENTS/$envName
	if { ![file isdirectory $envDir] } {
	    error "$envDir does not exist"
	}

	if { ![file writable $envDir] } {
	    error "$envDir is not writable"
	}
    }
}

###############################################################################
# proc allocEnv
#
#  allocates environments of envNameList: book only names.
#  updates TestList.sed (generates if does not exist) to
#  have a test reference independant of allocated environment names
###############################################################################

proc allocEnv {} {

    global env
    global gv

    set gv(envCreatedNow) 1

    file delete -force -- $gv(sessionFile)
    close [open $gv(sessionFile) w];	# equivalent to Unix "touch"

    set SFileName [pwd]/$gv(sessionFile)
    set i 0
    set le [llength $gv(envNameList)]
    while { $i < $le } {
	set envName [lindex $gv(envNameList) $i]
	incr i
	set envType [lindex $gv(envNameList) $i]
	incr i
	if { $envType == "WS" || $envType == "RWS" } {

	    if { $envType == "RWS" } {
		set envHOST $gv(RHOST)
	    } else {
		set envHOST $gv(HOST)
	    }

	    set envId [ tatGetRTAPEnv $envName $SFileName $envHOST $gv(VLTDATA)]
	    set env($envName) $envId
	    exec echo $envType $envName $envId >>$gv(sessionFile)
	    if { $envType == "WS" } {
		set env(RTAPENV) $envId
	    }

	} elseif { $envType == "LCU"} {
	    # Control on $LCUROOT only if an LCU Env. is specified on Testlist
            if {[catch {set gv(LCUROOT) $env(LCUROOT)}]} {
                error "LCUROOT undefined"
            }
	    set H $gv(HOST)
	    set envId [ tatGetLCUEnv $envName $SFileName $gv(LCUROOT) $gv(VLTDATA) $H]
	    set env($envName) $envId
	    exec echo LCU $envName $envId >>$gv(sessionFile)
	    # convention: env(LCU) is the name of the corresponding LCU
	    if {[ catch { vccInit } err ]} {
		error "$err"
	    }
	    if {[ catch { set lcu [vccInfo GetByEnv $envId hostName] } out ]} {
		error "$out"
	    }
	    set env(LCU) $lcu

	} elseif { $envType == "QS" || $envType == "RQS" } {
	
            if { $envType == "RQS" } {
                set envHOST $gv(RHOSTQS)
            } else {
                set envHOST $gv(HOST)
            }

            set envId [ tatGetQsemuEnv $envName $SFileName $envHOST $gv(VLTDATA)]
            set env($envName) $envId
	    exec echo $envType $envName $envId >>$gv(sessionFile)

	} 
    }

    #
    # Create its own "sedFile" to replace allocated environment
    # names by the corresponding variables

    set gv(sedFileBis) ".TestList.sed"
    set fd [open $gv(sedFileBis) w+]

    set isEnvName 1
    foreach envName $gv(envNameList) {
	if { $isEnvName } {
	    puts $fd s/$env($envName)/$envName/g
	    set isEnvName 0
	} else {
	    set isEnvName 1
	}
     }

     close $fd

}
###############################################################################
# proc setEnv
#
# set the environments variables for the RTAP & LCU environments, according to
# sessionFile.
###############################################################################

proc setEnv {} {


    global env
    global gv

  # set the env. variables according to sessionFile
  if { ![file exists $gv(sessionFile)] } {
      error "cannot open $gv(sessionFile)"
  }

   set rtapenvDone 0
   set lcuDone 0
   # Open the file for read
   set fd [open $gv(sessionFile) r]

   # Loop over the lines in the file.
   # Each line correspond to a anvironment
   while { [gets $fd line] >= 0} {


       # convert the current line into a list of words	
	set ll [split $line " "]
	set lll [llength $ll]

        set word [lindex $ll 0]  
	set word2 [lindex $ll 1]
	set word3 [lindex $ll 2]

	if { $word != "LCU" && $word!="WS" && $word != "RWS" && $word != "QS" && $word != "RQS"} {
	    error "$sessionFile: $line: setEnv invalid environment type"
	}

	if { ($word == "WS" || $word == "QS") && $rtapenvDone == 0 } {
	    set rtapenvDone 1
	    set env(RTAPENV) $word3
	}

	if { $word == "LCU" && $lcuDone == 0 } {
	    set lcuDone 1
	    set env(LCU) [string range $word3 1 end]
	}
	
	# define the environment variable and assign to it the right value
	set env($word2) $word3

    }
    close $fd

}


###############################################################################
# proc replaceScanEnv
#
# Replace every occurrence of <envName> in ./ENVIRONMENTS/<envName>dbl/USER.db 
# files with the allocated environments name for the scan system configuration
###############################################################################

proc replaceScanEnv {} {

    

    global env
    global gv

    set sedFile sed.scan
    file delete -force -- $sedFile
    set fd [open $sedFile w]

    set isEnvName 1
    foreach envName $gv(envNameList) {
	if { $isEnvName } {
	puts $fd s/$envName/$env($envName)/
	    set isEnvName 0
	} else {
	    set isEnvName 1
	}
    }
    close $fd

    set isEnvName 1
    foreach envName $gv(envNameList) {
	if { $isEnvName } {
	    # inputFile must be different from realFile otherwise
	    # original file will be wrongly modified !
	    set inputFile ./ENVIRONMENTS/$envName/dbl/USER.db.scan
            set realFile ./ENVIRONMENTS/$envName/dbl/USER.db
	    if { [file exists $inputFile] } {
		if { [catch {exec sed -f $sedFile $inputFile > $realFile.tmp}] } {
		    error "sed processing of the $inputFile file failed. Check syntax in the $sedFile file" 
		}
		mvFiles $realFile.tmp $realFile  
		}
		set isEnvName 0
	    } else {
		set isEnvName 1 
	    }
	    
	}

}
###############################################################################
# proc execMakeAll
#
# run 'make [clean] all' taking into account command line options 
# in all test directories 
###############################################################################

proc execMakeAll { noClean TAT_MAKE_ARGS } {

  set cwd [pwd]
  cd ..
  set ppwd [pwd]

  if {[ regexp "/ws$" $ppwd ]} {
	    # module has "ws" _and_ "lcu" part:
	    cd ..
            # lcu
            if {[ file isdirectory lcu/test ]} {
		cd lcu/test
		execMakeAllDir $noClean $TAT_MAKE_ARGS
		cd ../..
	    }
	}
  # ws part (or only part)
  cd $cwd
  execMakeAllDir $noClean $TAT_MAKE_ARGS 

  prepareTestList

  }

###############################################################################
# proc execMakeAllDir
#
# run 'make [clean] all' taking into account command line options 
# in the the current directory
###############################################################################

proc execMakeAllDir { noClean TAT_MAKE_ARGS  } {

    global env

    # no Makefile => no "make ..."
    if { ![file exists ./Makefile] } {
	return
    }

    set mlog /tmp/make.log.[pid].$env(USER)
    file delete -force -- $mlog

    # N.B.: 
    # - exec $cmd fails with "$cmd: No such file or directory"
    # - 'make all' SHALL always be redirected because it writes on
    # stderr "... object.xxx.ds: No such file or directory"
    # (even if return code of 'make' itself is 0, this makes exec fails)

    if { $noClean == 0 } {
	set cmd "exec make clean all $TAT_MAKE_ARGS MAKE_FROMTAT=defined >>& $mlog" 
	tatPuts "Executing $cmd in [pwd]"
	if {[catch $cmd ]} {
	    set out [read_file $mlog]
	    error "$cmd has failed: $out."
	}
    } else {
	set cmd "exec make all $TAT_MAKE_ARGS >>& $mlog"
	tatPuts "Executing $cmd in [pwd]"
	if {[ catch $cmd ]} {
	     set out [read_file $mlog]
	     error "$cmd has failed: $out."
	}
    }

}

###############################################################################
# proc execMakeClean
#
# run 'make clean' taking into account command line options 
# in all test diretories
###############################################################################

proc execMakeClean { noClean  } {

  set cwd [pwd]
  cd ..
  set ppwd [pwd]
  if {[ regexp "/ws$" $ppwd ]} {
	    # module has "ws" _and_ "lcu" part:
	    cd ..
            # lcu
            if {[ file isdirectory lcu/test ]} {
		cd lcu/test
		execMakeCleanDir $noClean 
		cd ../..
	    }
	}
  # ws part (or only part)
  cd $cwd
  execMakeCleanDir $noClean

  }

###############################################################################
# proc execMakeCleanDir
#
# run 'make clean' taking into account command line options 
# into the current directory
###############################################################################

proc execMakeCleanDir { noClean } {

    # no Makefile: no "make ..."
    if { ![file exists ./Makefile] } {
	return
    }

    # N.B.: exec $cmd fails with "$cmd: No such file or directory"

    if { $noClean == 0 } {
	set cmd "exec make clean MAKE_FROMTAT=defined " 
	tatPuts "Executing $cmd in [pwd]"
	if {[catch $cmd out ]} {
	    error "$cmd has failed: $out."
	}
    } else {
	return
    }

}

###############################################################################
# proc makeEnv
#
#  creates all the environments of envNameList
###############################################################################

proc makeEnv {} {

    global gv
    global env
    global PID

    set i 0
    set le [llength $gv(envNameList)]
    set wsEnvList {}
#  
#   sets MODROOT environment variable if not defined (to be used by lcuboot).
#   (assuming we are in <mod>/[ws]/test)
#
    if {[ catch { set modPath $env(MODROOT) } ]} {
	set cwd [pwd]
	cd ..
        set ppwd [pwd]
        if {[ regexp "/ws$" $ppwd]} {
	    # module has "ws" _and_ "lcu" part:
            # MODROOT/bin should be <mod>/lcu/bin
	    cd ..
            cd lcu
            set env(MODROOT) [pwd]
	} else {
	    # module is workstation by default: 
            # MODROOT/bin should be <mod>/bin
            set env(MODROOT) [pwd]
	}
        cd $cwd
    }

#   add <module>/[ws/]dbl to USER_INC to avoid duplication of dbl files
#   SPR 990481: add <module>/[ws/]include also to USER_INC

    set cwd [pwd]
    cd ..
    set modPath [pwd]
    cd $cwd
    tatPuts "adding $modPath/dbl and $modPath/include to USER_INC"	

     set oldUSER_INC ""
    catch { set oldUSER_INC $env(USER_INC) }
    set env(USER_INC) "$oldUSER_INC -I $modPath/dbl -I $modPath/include"
    
#   
#   a LCU environment needs to know the booting environment (WS or QS)
#   it is the first appearing in TestList. If none, it is the
#   standard environment.
#   

    catch { set wS RTAPENV }
    while { $i < $le } {
	set envName [lindex $gv(envNameList) $i]
	set envType [lindex $gv(envNameList)  [expr {$i + 1}]  ]

	if { $envType == "WS" || $envType == "QS" } {
	    set wS $envName
	    set i [expr {$le + 1}]
	}

	incr i
	incr i
    }
    
#
#   create *all* the environments
#

    set i 0
    set le [llength $gv(envNameList)]

    while { $i < $le } {

	set envName [lindex $gv(envNameList) $i]
	set envType [lindex $gv(envNameList) [expr {$i + 1}]]

	if { $envType == "LCU"  } {
	   set ok [catch { tatMakeLCUEnv $envName $wS $gv(HOST) $gv(VLTDATA) } m ]
	    if { $ok != 0 } {
    		file copy $gv(VLTDATA)/ENVIRONMENTS/$env($envName) ./tatlogs/run$PID/$env($envName)
		set tatlog [ open ./tatlogs/run$PID/$env($envName)/tatMakeLCUEnv.log w]
		puts $tatlog $m
		close $tatlog
		error "tatMakeLCUEnv FAILED: verify tat log  [pwd]/tatlogs/run$PID/$env($envName)/tatMakeLCUEnv.log"
	   }
	} elseif { $envType == "WS" || $envType == "RWS" } {
	    set ok [catch { tatMakeRTAPEnv $envName $gv(HOST) $gv(VLTDATA) } m ]
	    if { $ok != 0 }  {
		if { $envType == "WS" } {
    			file copy $gv(VLTDATA)/ENVIRONMENTS/$env($envName) ./tatlogs/run$PID/$env($envName) 
			set tatlog [ open ./tatlogs/run$PID/$env($envName)/tatMakeRTAPEnv.log w]
			puts $tatlog $m
			close $tatlog
			error "tatMakeRTAPEnv FAILED: verify tat log  [pwd]/tatlogs/run$PID/$env($envName)/tatMakeRTAPEnv.log"
		} else {
			error "tatMakeRTAPEnv FAILED: $m"
		}	
	    }
	}  elseif { $envType == "QS" || $envType == "RQS" } {
	    if { $envType == "QS" } {
	    	set ok [ catch { tatMakeQsemuEnv $envName $gv(LOGNAME) $gv(HOST) } m]
	    } else {
		set ok [ catch { tatMakeQsemuEnv $envName $gv(LOGNAME) $gv(RHOSTQS) } m]
	    }
	    if { $ok != 0 } {
		if { $envType == "QS" } {
    			file copy $gv(VLTDATA)/ENVIRONMENTS/$env($envName) ./tatlogs/run$PID/$env($envName)
			set tatlog [ open ./tatlogs/run$PID/$env($envName)/tatMakeQsemuEnv.log w]
			puts $tatlog $m
			close $tatlog
			error "tatMakeQsemuEnv FAILED: verify tat log  [pwd]/tatlogs/run$PID/$env($envName)/tatMakeQsemuEnv.log"
		} else {
                        error "tatMakeQsemuEnv FAILED: $m"
                }
	    }
	}
	incr i
	incr i
    }

}

###############################################################################
# proc copyEnv
#
#  save the environments into the tatlog/run$PID directory
###############################################################################

proc copyEnv {} {

    global gv
    global PID

    # Copying the ENVIRONMENT directories
    if { $gv(generate) == 0 } {
       # search the allocated name in sessionFile when not empty       
       if {[file exists $gv(sessionFile)] && [file size $gv(sessionFile)] != 0} {
	  tatPuts "Copying ENVIRONMENTS directories to tatlog directory"
          set fd [open $gv(sessionFile) r]
    	  while { [gets $fd line] >= 0 } {
    	     set sline [split $line " "]
    	     set envId [lindex $sline 2]
    	     catch {file copy $gv(VLTDATA)/ENVIRONMENTS/$envId ./tatlogs/run$PID/$envId}
    	  }
    	  close $fd	 
       }
    }

}
###############################################################################
# proc cleanEnv
#
# deletes the environments created by makeEnv
#
###############################################################################

proc cleanEnv {} {


    global gv
    global env

    set i 0

    prepareTestList

    set le [llength $gv(envNameList)]

#
#   It is mandatory to specify the -w <wsEnv> switch to vccEnvDelete
#   and this parameter has to passed to tatCleanLCUEnv.
#   wsEnv it's the first appearing in TestList. If none, it is the
#   standard environment.
#

    catch { set wS RTAPENV }
    while { $i < $le } {
        set envName [lindex $gv(envNameList) $i]
        set envType [lindex $gv(envNameList) [expr {$i + 1}]]

        if { $envType == "WS" || $envType == "QS" } {
            set wS $envName
            set i [expr {$le + 1}]
        }

        incr i
        incr i
    }

    set i 0

#   for each environment

    while { $i < $le } {
	set envName [lindex $gv(envNameList) $i]
	set envType [lindex $gv(envNameList) [expr {$i + 1}]]

	# If corresponding environment variables are not defined,
	# define them for tatClean(...) .
        if {[catch {set envId $env($envName)}]} {
	    set envId undefined
	}

	# if $sessionFile does not exist, nothing has to be cleaned
	if { ![file exists $gv(sessionFile)]} {
	    return
	}

	# search the allocated name
	set fd [open $gv(sessionFile) r]
	while { [gets $fd line] >= 0 } {
	    if { [regexp $envName $line] != 0 } {
		set sline [split $line " "]
		set envId [lindex $sline 2]
		break
	    }
	}
	close $fd

	if { $envId != "undefined" } {
	    set env($envName) $envId
	} else {
	    error "Environment variable $envName not defined"
	}

	# if one cleanEnv fails do not call error but go on
	# to clean the other ones.

	if { $envType == "LCU" } {
	    # Control on $LCUROOT only if an LCU Env. is specified on Testlist
            if {[catch {set gv(LCUROOT) $env(LCUROOT)}]} {
                error "LCUROOT undefined"
            }

	    # the tat -v cleanEnv doesn't work properly if the firs env in TestList 
	    # is a LCU env. To fix this bug, I use the following piece of code to 
	    # get again the workstation env (newwS) to pass to tatCleanLCUEnv
	    set newwS ""
            set fd [open $gv(sessionFile) r]
            while { [gets $fd line] >= 0 } {
                if { [regexp $wS $line] != 0 } {
                    set sline [split $line " "]
                    set newwS [lindex $sline 2]
                    break
                }
            }
            close $fd
	    if { $newwS == "" } {
		set newwS $env($wS)
	    }

	    set ok [catch { tatCleanLCUEnv $envName $gv(LCUROOT) $newwS }  msg ]
	    if { $ok != 0 } {
		puts "tatCleanLCUEnv failed: $msg"
	    }
	} elseif { $envType  == "WS" || $envType == "RWS" } {
	    set ok [catch { tatCleanRTAPEnv $envName $gv(HOST) $gv(VLTDATA) } msg]
	    if { $ok != 0 } {
		puts "tatCleanRTAPEnv failed: $msg"
	    }
	} elseif { $envType == "QS" || $envType == "RQS" } {
	    if { $envType == "QS" } {
	   	 set ok [ catch { tatCleanQsemuEnv $envName $gv(LOGNAME) $gv(VLTDATA) $gv(HOST) } msg ]	    
	    } else {
		 set ok [ catch { tatCleanQsemuEnv $envName $gv(LOGNAME) $gv(VLTDATA) $gv(RHOSTQS) } msg ] 
	    }
	    if { $ok != 0 } {
		puts "tatCleanQsemuEnv failed: $msg"
	    }
	}

	incr i
	incr i
    } 

    file delete -force -- $gv(sedFileBis)
    file delete -force -- $gv(grepFileBis)

    # the test session has ended

    file delete -force -- $gv(sessionFile)

}

###############################################################################
# runTest (testList, generate, occurrence)
#
#  runs the test programs as listed in in testList
#
#  if generate is 0, the test are run and compared with a reference output.
#  If generate is 1, a reference output file is generated
#
###############################################################################

proc runTest { testList generate occurrence} {

    global gv
    global env
    global PID
    global noorder waitAtEnd verbose
    global repeat

    set retFlag   0
    set FLAG_TESTDRIVER 1

    set sourceTestId {}
    set prologueTestId {}
    set epilogueTestId {}
    set numEl 0
        
    set extension ""
    if { $repeat > 1 } {
       set extension _[expr {$repeat - $occurrence +1}]    
    }

    # Loop over the tests
    # Each line correspond to a test
    while { ![lempty $testList]} {

	# testid will contain the number which identifies the test; in case the old tat syntax is used, the testid will simply be the test itself
	set testid [lvarpop testList]
	set startingTime [clock seconds] 

	# Parse the test command line
	if { [llength $testid ] > 1 } {
	    set thisTest [lindex $testid 0]
	    lvarpop testid
	    set FLAG_TESTDRIVER 1
            if { [lsearch -exact $testid SOURCE] != -1 } {
		if { [lindex $testid 0] != "SOURCE" } {
		    error "SOURCE directive is not in the right place; it should be immediately after the test id number"
		}
		lvarpop testid
		set sourceTestId [lvarpop testid]
		tatPuts "Sourcing script $sourceTestId"
		source $sourceTestId
	    }
	    if { [lsearch -exact $testid PROLOGUE] != -1 } {
		if { [lindex $testid 0] != "PROLOGUE" } {
		    error "PROLOGUE directive is not in the right place"
		}
		lvarpop testid
		set prologueTestId [lvarpop testid]
		set prologueProg [lindex $prologueTestId 0]
		set prologueArgs [lrange $prologueTestId 1 end]
	        tatPuts "Executing prologue script $prologueTestId"
	        if {[ catch {eval exec $prologueProg $prologueArgs} out ]} {
	    	    tatPuts "The prologue script $prologueTestId fails: $out"
	        }
	        tatPuts "End of prologue script"
	    }
	    if { [lsearch -exact $testid EPILOGUE] != -1 } {
		set numEl [llength $testid]
		set lastElIndex [expr {$numEl -1}]
		set epiDirIndex [expr {$lastElIndex -1}]
		if { [lindex $testid $epiDirIndex] != "EPILOGUE" } {
		    error "EPILOGUE directive is not in the right place"
		}
		set epilogueTestId [lindex $testid $lastElIndex]
		set epilogueProg [lindex $epilogueTestId 0]
		set epilogueArgs [lrange $epilogueTestId 1 end]
		set testid [lreplace $testid $epiDirIndex $lastElIndex] 
	    }
	    #set thisTest [lindex $testid 0]
	    set testName [lindex $testid 0]
	    set testid [concat $thisTest $testid]
	    set outputFile   ./tatlogs/run$PID/$testName$extension.out
	    set refFile      ./ref/$testName.ref
#	    The following line is a debug printing, not necessarily needed
#	    puts "thisTest=$thisTest"
	    if { $gv(testAll) == 0 && [lsearch $gv(toTest) $thisTest] == -1 } {
		continue
	    }
	    if { [ regexp {[0-9]+} $thisTest ] } {
                set nProc [expr {[llength $testid] - 2}]
                set fProc 2
                set testProc [lindex $testid 1]

            } else {
                set nProc [expr {[llength $testid]} - 1]
                set fProc 1
                set testProc [lindex [lindex $testid $nProc] 0]
            }

            printLogVerbose "TEST NAME: $testProc ($nProc processes to run)"
	    # make test script executable for cmmCopied dir:
	    # only test scripts should be in the current dir.
	    # not the binary generated under bin.
	    if { [file exists $testid] && ![file executable $testid] } {
	        if { [catch {chmod u+x $testid}] } {
		    error "cannot chmod u+x $testid"
	        }
	    }

	} else {
	    set FLAG_TESTDRIVER 0
#	    The following line is a debug printing, not necessarily needed
#	    puts "testid = $testid"

	    set outputFile   ./tatlogs/run$PID/$testid$extension.out
	    set refFile      ./ref/$testid.ref
	    catch {file delete -force -- $outputFile}
	    # make test script executable for cmmCopied dir:
	    # only test scripts should be in the current dir.
	    # not the binary generated under bin.
	    if { [file exists $testid] && ![file executable $testid] } {
	        if { [catch {chmod u+x $testid}] } {
		    error "cannot chmod u+x $testid"
	        }
	    }
	    
	}

        set userId [id user]
        set file /tmp/${userId}_test[pid]

        set fileList {}

	# execute the test


	if { $FLAG_TESTDRIVER == 0 } {

	# if VxWorks script, command is different from test script
	set vxcmd 0
	if { [regexp ".*.vw$" $testid] } {
	    set testcmd "tatRemExec $testid"
	    set vxcmd 1
	} else {
	    set testcmd $testid
	}

	if {[ catch { set time_out $env(TEST_WAIT) } ]} {
	    set time_out 0
	}

	if { $time_out != 0 } {
	    # timeout specified

	    tatPuts "Executing $testcmd (timeout: $time_out s.)"	
	    if { $vxcmd == 0 } {
		catch { set testPid [exec $testid >& $outputFile &] }
	    } else {
		catch { set testPid [exec tatRemExec $testid >& outputFile &]}
	    }

	    set testHasEnded 0
	    set startTime [getclock]
	    set waitTime 0

	    # Test if the "wait" command is available under this opsys.
	    # On HP-UX 11.00, Solaris 2.8 and Linux it should be.
            set have_waitpid [infox have_waitpid] 
	    while {[expr {$waitTime < $time_out}]} {
		if {$have_waitpid} {
		    set testExitStatus [wait -nohang $testPid]
		} else {
		    # use 'exec ps' to create a similar return value as 'wait'does.
		    if {[catch {exec ps -p $testPid}]} {
			# ps -p <pid> returns 1 if the process <pid> does not exist
			set testExitStatus "$testPid EXIT 0"
		    } else {
			set testExitStatus ""
		    }
		}
		if {[llength $testExitStatus] == 3} {
		    set testHasEnded 1
		    # We can actually see how this test has ended, as the wait cmd
		    # returns 3 list elements:
		    # 1. the pid
		    # 2. 'EXIT' if exited normally or 'SIG' if ended by signal
		    # 3. exit code (2 = 'EXIT') or signal name (2 = 'SIG')
		    break
		} else {
		    # $testPid still in the process list
		    set currentTime [getclock]
		    set waitTime [expr {$currentTime - $startTime}]
		    after 1000;	# sleep one second before trying again
		}
	    }
	    if { $testHasEnded == 0 } {
		# test is still running after $time_out minutes:
		# kill it
		kill SIGKILL $testPid
		puts "TEST $testcmd KILLED."
	    }

	} else {
	    # no time out specified
	    tatPuts "Executing $testcmd"	
	    if { $vxcmd == 0 } {
		catch {exec $testid >& $outputFile}
	    } else {
		# no way to have the output with '>&' even in background !!!
		set log /tmp/tatRemExec.[pid]
		file delete -force -- $log
		catch { exec tatRemExec $testid $log } out
		# retrieve tat/exec possible errors
		if { $out != ""  } {
		    exec echo "$out" > $outputFile
		}
		exec cat $log >> $outputFile
	    }
	}


	# Filter the files for lines that can change and time stamps

	# for VxWorks tests
	if { $vxcmd == 1 } {
	     # remove:
	     # - <LCU>-> prompts
	     # - VxWorks shell return value

	     set fd [open $gv(grepFileBis) w+]
	     set lcu $env(LCU)
	     puts $fd "l$lcu->"
	     puts $fd "value = "
	     close $fd
	}

# SPR 20040140: the outputFile is saved in .orig before being processed with sed and grep
	catch {file copy -force $outputFile $outputFile.orig}
	
	egrepFilter $outputFile $testid $refFile $generate

        sedFilter $outputFile $testid

	# Cleanup the files not used any more
	catch {eval file delete -force -- $file }

        # if the option was run
	if {$generate == 0} {

	    set runningTime [clock format [expr {[clock seconds] - $startingTime}] -format %T]

	    set err [catch {exec diff $outputFile $refFile} diffErr]
	    set diffFile ./tatlogs/run$PID/$testid$extension.diff
	    if {$err > 0} {
		# SPR ALMASW2003069: remove the "child process" message below from the diff file
		catch { regsub "child process exited abnormally$" $diffErr "" newDiffErr }
		set dfd [open $diffFile w]
		puts $dfd $newDiffErr
		close $dfd
		puts "Differences found in [pwd]/$diffFile"
		puts "TEST $testid FAILED.\t$runningTime"
		set retFlag 1
	    } else {
		puts "TEST $testid PASSED.\t$runningTime"

	    }

	}
      
        # if the option was "generate"
	if {$generate == 1} {
	    puts "Reference file $testid.ref generated"
	    mvFiles $outputFile $refFile
	}

	# Case of extension to TESTDRIVER=1
        } else {
	
        # Looks for all entries in sigle record
        # This can be real processes or special directives (@SLEEP)
        # then builds the list of real processes and stores the number
        # in $actualProc
        set actualProc 0
        for {set i 0} { $i < $nProc } { incr i +1 } {

            set cmdLine [lindex $testid [expr {$i + $fProc}]]
           
            if {[lindex $cmdLine 0] == "@SLEEP" } {
                set sleepTime [lindex $cmdLine 1]
                printLogVerbose "waiting $sleepTime secs."
                sleep $sleepTime
            } else {
                set fileArray($actualProc) /tmp/${userId}_test[pid].$actualProc
                lappend fileList $fileArray($actualProc)

                set bpid($actualProc) 0


                set outFile $fileArray($actualProc)

                if { $i != [expr {$nProc -1}] } {
                    printLogVerbose "Executing bg: $cmdLine (out in $outFile)"
                    set bpid($actualProc) [eval exec tatTestSpawner [runProg $cmdLine] >&$outFile &]
                } else {
                    printLogVerbose "Executing fg: $cmdLine (out in $outFile)"
                    sleep 2
                    catch {eval exec [runProg $cmdLine] >&$outFile }
                }
                incr actualProc +1
            }
        }

        if { $waitAtEnd != 0 } {
                printLogVerbose "Sleeping $waitAtEnd secs"
                sleep $waitAtEnd
            }

	if { $epilogueTestId != "" } {
	    tatPuts "Executing epilogue script $epilogueTestId"
	    if {[ catch {eval exec $epilogueProg $epilogueArgs} out ]} {
	        tatPuts "The epilogue script $epilogueTestId fails: $out"
	    }
	    tatPuts "End of epilogue script"
	    set epilogueTestId {}
	}
        # Add a time stamp to all the lines of the output files
        # that do not have it.
        # Add also a mark to identify the process (master or slave)
        # Finally kill all the processes in the group
        # identified by the backgrund task
        for {set i 0} { $i < $actualProc } { incr i +1 } {
            addTimeStamp $fileArray($i) "[expr {$i + 1}] -"
            if { $noorder == 1 } {
                # Then sort all the lines according to the time stamp.
                eval exec cat $fileList | sort > $file
            } else {
                eval exec cat $fileList > $file
            }
            # Now I can safely kill all the processes in the group
            # identified by the backgrund task
            if {$bpid($i) > 0} { catch { kill -pgroup SIGTERM $bpid($i) } }

        }

        # Now the time stamp can be removed, since all lines are sorted
        rmTimeStamp $file

        # SPR 2006007: the outputFile is saved in .orig before being processed with sed and grep
        catch {file copy -force $file ./tatlogs/run$PID/$testProc.out.orig}

        # Filter the files for lines that can change
        egrepFilterx $file $testProc $refFile $generate
        sedFilterx $file $testProc

        # Copy the file in the definitive place
        catch {file copy -force $file ./tatlogs/run$PID/$testProc.out}

        # Cleanup the files not used any more
        catch {eval file delete -force -- $fileList $file }

        # If Run, compare the output and the reference files,
        # writes a report and prints the test result
        if {$generate == 0} {

            # Check if the reference file exist.
            # If not, no comparison can be done and exits
            if { ![file exists ./ref/$testProc.ref] } {
                printLog "Reference file ./ref/$testProc.ref does not exist"
                return 1
            }

            # Checks if the files must be compared taking into account
            if { $noorder == 1 } {

                # line position or not.
                set file1 /tmp/${userId}_test[pid].ref
                set file2 /tmp/${userId}_test[pid].out

                # make sure the files does not exist already
                catch {file delete -force -- $file1 $file2}

                # sort the two files and compare them
                exec sort ./ref/$testProc.ref > $file1
                exec sort ./tatlogs/run$PID/$testProc.out > $file2
                set err [catch {exec diff $file1 $file2} diffErr]

                # Delete the temporary .ref and .out files after usage
                catch {file delete -force -- $file1 $file2}

            } else {

                set err [catch {exec diff ./ref/$testProc.ref ./tatlogs/run$PID/$testProc.out} diffErr]

            }
	    # rename output files
	    if { $repeat > 1 } {
	    	catch {file rename -force -- ./tatlogs/run$PID/$testProc.out ./tatlogs/run$PID/$testProc$extension.out}
	    	catch {file rename -force -- ./tatlogs/run$PID/$testProc.out.orig ./tatlogs/run$PID/$testProc$extension.out.orig}
	    }
            # Check if differences have been found and log proper messages
            if {$err > 0} {
		# SPR ALMASW2003069: remove the "child process" message below from the diff file
		catch { regsub "child process exited abnormally$" $diffErr "" newDiffErr }
                set dfd [open ./tatlogs/run$PID/$testProc.diff w]
                puts $dfd $newDiffErr
                close $dfd

                puts "Differences found in [pwd]/tatlogs/run$PID/$testProc$extension.diff"
                printLog "TEST$thisTest $testProc FAILED."
                set retFlag 1
	        # rename diff file
	        if { $repeat > 1 } {
	    	   catch {file rename -force -- ./tatlogs/run$PID/$testProc.diff ./tatlogs/run$PID/$testProc$extension.diff}
	        }
            } else {
                printLog "TEST$thisTest $testProc PASSED."
            }

        }
        # end if run

        # if the option was "generate"
        if {$generate == 1} {
            printLogVerbose "Reference file ./ref/$testProc.ref generated"
            file rename -force -- ./tatlogs/run$PID/$testProc.out ./ref/$testProc.ref
        }

	# End of case TESTDRIVER yes or no
	}


    }

    # If Run, compare the output and the reference files,
    # writes a report and prints the test result

    # Set the output file name depending on the mode
    if { $generate == 0 } { 
	if {$retFlag == 1} {
	    puts "FAILED."
	    return 1
	} else {
	    puts "PASSED."
	    # if test PASSED and no -nc option cancel logs Files
	    # and if no diff files are already in tatlog (for case -r option)
	    set df [exec find ./tatlogs/run$PID -type f -name "*.diff" -print]
	    if { $occurrence == 1 } {
               if { $gv(noClean) == 0 && $df == "" } {
		   tatPuts "Removing current log directory ./tatlogs/run$PID"
	    	   file delete -force -- ./tatlogs/run$PID
	       } else {
		   tatPuts "current log directory ./tatlogs/run$PID not removed"
	       }
	    }
	    return 0
	}
    } else {
	puts "Reference file generated."
	return 0
    }
}

###############################################################################
# procedure egrepFilter (fileName testName refName generate) 
# Strips off from the file all the lines maching the reg. exps. in file
# $grepFile (if it exist)
############################################################################## 

proc egrepFilter {fileName testName refName generate} {

    global gv
    set userId [id user]
    set tmpGrep /tmp/${userId}_test[pid]

    # process tat .TestList.grep (for VxWorks tests only)

    if {[file exists $gv(grepFileBis)] && [file size $gv(grepFileBis)] != 0} {
	# if the reference file for the test is supposed to be empty, we skip the egrep processing, if not egrep fails
	if { [file size $refName] == 0 && $generate == 0 } {
            printLogVerbose "No grep filter applied: $refName is empty"
        } else {
	    set res  [catch {exec egrep -v -f $gv(grepFileBis) $fileName > $fileName.tmp}]
	    if { $res == 2 } {
		# changing the algorithm to check for rc = 2, the following check should not be needed any longer
		# we want that tat exits with an error only if there is a syntax error in the grep file (if this is ever possible)
		# we do not want an error while processing the output file with egrep, if the output file is empty because of a test failure
		if { [file size $fileName] != 0 } {
	            error "grep processing of $fileName failed. Check the  $gv(grepFileBis) file, please"
		}
	    }
	    mvFiles $fileName.tmp $fileName
	}
    } else {
        printLogVerbose "VxWorks (ignore this message if you do not use VxWorks): no filter applied: $gv(grepFileBis) does not exist"


    } 

    # process user TestList.grep or testName.grep
    if { ($gv(commandLineTestLists) == 1) && ([file exists $gv(testListFiles).grep]) } {
	printLogVerbose "Cleaning with $gv(testListFiles).grep: $fileName"
	catch {file copy -force  $gv(testListFiles).grep $tmpGrep$gv(grepFile).tmp }
    } elseif {[file exists $testName.grep] && [file exists $gv(grepFile)]} {
        printLogVerbose "Cleaning with $gv(grepFile) and $testName.grep: $fileName"
        catch {exec cat $gv(grepFile) $testName.grep > $tmpGrep$gv(grepFile).tmp }
    } elseif {[file exists $testName.grep]} {
        printLogVerbose "Cleaning with $testName.grep: $fileName"
        catch {file copy -force $testName.grep $tmpGrep$gv(grepFile).tmp }
    } elseif {[file exists $gv(grepFile)]} {
        printLogVerbose "Cleaning with $gv(grepFile): $fileName"
        catch {file copy -force $gv(grepFile) $tmpGrep$gv(grepFile).tmp}
    }


    # first we start the circus only if a grep file exists
    if {[file exists $tmpGrep$gv(grepFile).tmp]} {
      # then we have to distinguish whether we are in the case of creating the reference file or not: if we want to create the reference files, generate is 1
      if { $generate == 1 } {
	# then we should consider the case whether the ref file already exists or not:
	if { [file exists $refName] } {
	  # if the reference file for the test is supposed to be empty, we skip the egrep processing, if not egrep fails
	  if { [file size $refName] == 0 } {
              printLogVerbose "No grep filter applied: $refName is empty"
          } else {
	      set res  [catch {exec egrep -v -f $tmpGrep$gv(grepFile).tmp $fileName > $fileName.tmp}]
	      if { $res == 2 } {	
		  # changing the algorithm to check for rc = 2, the following check should not be needed any longer
		  # we want that tat exits with an error only if there is a syntax error in the grep file (if this is ever possible)
		  # we do not want an error while processing the output file with egrep, if the output file is empty because of a test failure
		  if { [file size $fileName] != 0 } {
	              error "grep processing of $fileName failed. Check the  $tmpGrep$gv(grepFile).tmp file, please"
		  }
	      }
	      mvFiles $fileName.tmp $fileName
              catch {file delete -force -- $tmpGrep$gv(grepFile).tmp}
	  }
	# the ref file does not exist because I'm generating it for the first time
        } else {
	  set res  [catch {exec egrep -v -f $tmpGrep$gv(grepFile).tmp $fileName > $fileName.tmp}]
	  if { $res == 2 } {
	      # changing the algorithm to check for rc = 2, the following check should not be needed any longer
	      # we want that tat exits with an error only if there is a syntax error in the grep file (if this is ever possible)
	      # we do not want an error while processing the output file with egrep, if the output file is empty because of a test failure
	      if { [file size $fileName] != 0 } {
		  error "grep processing of $fileName failed. Check the  $tmpGrep$gv(grepFile).tmp file, please"
	      }
	    }
	    mvFiles $fileName.tmp $fileName
	    catch {file delete -force -- $tmpGrep$gv(grepFile).tmp} 
        }
      # here we process the case where we do not want to generate the reference files
      } elseif { $generate == 0 } {	
	# then we should consider the case whether the ref file already exists or not:
	if { [file exists $refName] } {
	  # if the reference file for the test is supposed to be empty, we skip the egrep processing, if not egrep fails
	  if { [file size $refName] == 0 } {
              printLogVerbose "No grep filter applied: $refName is empty"
          } else {
	      set res  [catch {exec egrep -v -f $tmpGrep$gv(grepFile).tmp $fileName > $fileName.tmp}]
	      if { $res == 2 } {
	          # changing the algorithm to check for rc = 2, the following check should not be needed any longer
		  # we want that tat exits with an error only if there is a syntax error in the grep file (if this is ever possible)
		  # we do not want an error while processing the output file with egrep, if the output file is empty because of a test failure
		  if { [file size $fileName] != 0 } {
	              error "grep processing of $fileName failed. Check the  $tmpGrep$gv(grepFile).tmp file, please"
		  }
	      }
	      mvFiles $fileName.tmp $fileName
              catch {file delete -force -- $tmpGrep$gv(grepFile).tmp}
	  }
	# the ref file does not exist because I'm generating it for the first time
        } else {
	    error "you are executing a test without having a reference file; this is not allowed"
        }

      }
    # the grep file does not exist, so we do not do anything
    } else {
        printLogVerbose "No filter applied: grepFile does not exist"
    } 

    return 0
}

###############################################################################
#
# procedure egrepFilterx (fileName testName refName generate)
# Strips off from the file all the lines maching the reg. exps. in file
# $grepFile (if it exist)
##############################################################################

proc egrepFilterx {fileName testName refName generate} {

    global gv
    set userId [id user]
    set tmpGrep /tmp/${userId}_test[pid]

    if { ($gv(commandLineTestLists) == 1) && ([file exists $gv(testListFiles).grep]) } {
        printLogVerbose "Cleaning with $gv(testListFiles).grep: $fileName"
        catch {file copy -force  $gv(testListFiles).grep $tmpGrep$gv(grepFile).tmp }
    } elseif {[file exists $testName.grep] && [file exists $gv(grepFile)]} {
        printLogVerbose "Cleaning with $gv(grepFile) and $testName.grep: $fileName"
        catch {exec cat $gv(grepFile) $testName.grep > $tmpGrep$gv(grepFile).tmp }
    } elseif {[file exists $testName.grep]} {
        printLogVerbose "Cleaning with $testName.grep: $fileName"
        catch {file copy -force $testName.grep $tmpGrep$gv(grepFile).tmp}
    } elseif {[file exists $gv(grepFile)]} {
        printLogVerbose "Cleaning with $gv(grepFile): $fileName"
        catch {file copy -force $gv(grepFile) $tmpGrep$gv(grepFile).tmp}
    }

    # Clean the output file.
    # first we start the circus only if a grep file exists
    if {[file exists $tmpGrep$gv(grepFile).tmp]} {
      # then we have to distinguish whether we are in the case of creating the reference file or not: if we want to create the reference files, generate is 1
      if { $generate == 1 } {
	# then we should consider the case whether the ref file already exists or not:
        if { [file exists $refName] } {
	  # if the reference file for the test is supposed to be empty, we skip the egrep processing, if not egrep fails
	  if { [file size $refName] == 0 } {
	      printLogVerbose "No grep filter applied: $refName is empty"
	  } else {
	      set res  [catch {exec egrep -v -f $tmpGrep$gv(grepFile).tmp $fileName > $fileName.tmp}]
	      if { $res == 2 } {
	          # changing the algorithm to check for rc = 2, the following check should not be needed any longer
	  	  # we want that tat exits with an error only if there is a syntax error in the grep file (if this is ever possible)
		  # we do not want an error while processing the output file with egrep, if the output file is empty because of a test failure
		  if { [file size $fileName] != 0 } {
	              error "grep processing of $fileName failed. Check the file  $tmpGrep$gv(grepFile).tmp, please"
		  }
	      }
              catch {file rename -force -- $fileName.tmp $fileName}
              catch {file delete -force -- $tmpGrep$gv(grepFile).tmp}
	  }
        # the ref file does not exist because I'm generating it for the first time
        } else {
	  set res  [catch {exec egrep -v -f $tmpGrep$gv(grepFile).tmp $fileName > $fileName.tmp}]
	  if { $res == 2 } {
	      # changing the algorithm to check for rc = 2, the following check should not be needed any longer
              # we want that tat exits with an error only if there is a syntax error in the grep file (if this is ever possible)
              # we do not want an error while processing the output file with egrep, if the output file is empty because of a test failure
              if { [file size $fileName] != 0 } {
                  error "grep processing of $fileName failed. Check the  $tmpGrep$gv(grepFile).tmp file, please"
	      } else {
		  printLogVerbose "INFO: while applying the grep filter, I noticed that the generated reference file will be empty"
              }
            }
            catch {file rename -force -- $fileName.tmp $fileName}
            catch {file delete -force -- $tmpGrep$gv(grepFile).tmp}
        }
      # here we process the case where we do not want to generate the reference files
      } elseif { $generate == 0 } {
	# again we consider the case whether the ref file exists or not:
        if { [file exists $refName] } {
          # if the reference file for the test is supposed to be empty, we skip the egrep processing, if not egrep fails
          if { [file size $refName] == 0 } {
              printLogVerbose "No grep filter applied: $refName is empty"
          } else {
	    set res  [catch {exec egrep -v -f $tmpGrep$gv(grepFile).tmp $fileName > $fileName.tmp}]
	    if { $res == 2 } {
	      # changing the algorithm to check for rc = 2, the following check should not be needed any longer
	      # we want that tat exits with an error only if there is a syntax error in the grep file (if this is ever possible)
   	      # we do not want an error while processing the output file with egrep, if the output file is empty because of a test failure
   	      if { [file size $fileName] != 0 } {
   		error "grep processing of $fileName failed. Check the  $tmpGrep$gv(grepFile).tmp file, please"
   	      }
   	    }
   	    catch {file rename -force -- $fileName.tmp $fileName}
   	    catch {file delete -force -- $tmpGrep$gv(grepFile).tmp}
	 }
        } else {
            error "you are executing a test without having a reference file; this is not allowed"
        }

      }  
    } else {
	printLogVerbose "No filter applied: grepFile does not exist"
    }

    return 0
}


###############################################################################
# procedure sedFilter (fileName testName) 
#
# Executes the sed commands stored in the file $sedFileBis
# Executes the sed commands stored in the file $sedFile (if it exist)
#
############################################################################## 

proc sedFilter {fileName testName} {

    global gv
    set userId [id user]
    set tmpSed /tmp/${userId}_test[pid]

#   process tat allocated environment names

    if {[file exists $gv(sedFileBis)] && [file size $gv(sedFileBis) ] != 0} {
	if { [catch {exec sed -f $gv(sedFileBis) $fileName > $fileName.tmp}] } {
	     error "sed processing of the $fileName file failed. Check syntax in the $gv(sedFileBis) file "
	}
	mvFiles $fileName.tmp $fileName
    }

    # process user TestList.sed or testName.sed

    if { ($gv(commandLineTestLists) == 1) && ([file exists $gv(testListFiles).sed]) } {
        printLogVerbose "Cleaning with $gv(testListFiles).sed: $fileName"
        catch {file copy -force  $gv(testListFiles).sed $tmpSed$gv(sedFile).tmp }
    } elseif {[file exists $testName.sed] && [file exists $gv(sedFile)]} {
        printLogVerbose "Cleaning with $gv(sedFile) and $testName.sed: $fileName"
        catch {exec cat $gv(sedFile) $testName.sed > $tmpSed$gv(sedFile).tmp }
    } elseif {[file exists $testName.sed]} {
        printLogVerbose "Cleaning with $testName.sed: $fileName"
        catch {file copy -force $testName.sed $tmpSed$gv(sedFile).tmp }
    } elseif {[file exists $gv(sedFile)]} {
        printLogVerbose "Cleaning with $gv(sedFile): $fileName"
        catch {file copy -force $gv(sedFile) $tmpSed$gv(sedFile).tmp}
    }


    if {[file exists $tmpSed$gv(sedFile).tmp]} {
	if { [catch {exec sed -f  $tmpSed$gv(sedFile).tmp $fileName > $fileName.tmp}] } {
	    error "sed processing of the $fileName file failed. Check syntax in the TestList.sed or <testName>.sed file"
	}
	mvFiles $fileName.tmp $fileName
        catch {file delete -force -- $tmpSed$gv(sedFile).tmp}
    } else {
        printLogVerbose "No filter applied: sedFile does not exist"
    } 

    return 0
}

###############################################################################
#
# procedure sedFilterx (fileName testName)
# Executes the sed commands stored in the file $sedFile (if it exist)
# SPR VLTSW20050196:Executes the sed commands stored in the file $sedFileBis too
##############################################################################

proc sedFilterx {fileName testName} {

    global gv
    set userId [id user]
    set tmpSed /tmp/${userId}_test[pid]

    if { ($gv(commandLineTestLists) == 1) && ([file exists $gv(testListFiles).sed]) } {
        append sedScript [read_file $gv(testListFiles).sed]
        append msg "$gv(testListFiles).sed + "
    } else {
        foreach scriptFile [list $gv(sedFileBis) $gv(sedFile) $testName.sed] {
            if {[file exists $scriptFile]} {
                append sedScript [read_file $scriptFile]
                append msg "$scriptFile + "
            }
        }
    }
    
    if {[info exists sedScript]} {
        printLogVerbose "Cleaning with [string range $msg 0 end-3]: $fileName"
        write_file $tmpSed$gv(sedFile).tmp $sedScript
        # Clean the output file.
        if { [catch {exec sed -f $tmpSed$gv(sedFile).tmp $fileName > $fileName.tmp} ] } {
	    error "sed processing of the $fileName file failed. Check syntax in the $tmpSed$gv(sedFile).tmp file"
	}
        catch {file rename -force -- $fileName.tmp $fileName}
        catch {file delete -force -- $tmpSed$gv(sedFile).tmp}
    } else {
        printLogVerbose "No filter applied: sedFile does not exist"
    }

    return 0
}

###############################################################################
#
# procedure addTimeStamp (fileName)
#  strips off the time stamp (first) from the fileName
##############################################################################

proc addTimeStamp {fileName pnum} {

    set stamp "0000-00-00"
    set time  "00:00:00."
    set msec  "000000"
    set count 0

    printLogVerbose "Adding time stamps to lines of file: $fileName"

    set fd [open $fileName r]
    set rfd [open $fileName.tmp w]
    while { [gets $fd line] >= 0} {
        if {[regexp {^[0-9]{4}-[0-9]{2}-[0-9]{2}} $line] != 0} {
            regexp {^[0-9]{4}-[0-9]{2}-[0-9]{2}} $line stamp
            # Probably "." should be replaced by "\." and "*" by "+", but
            # I'm not sure what the precise intention is - EAL 2005-02-08
            # buggy: regexp {[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{0,6} *} $line fulltime
	    # better (see ITS/FunctionalTests/obsp/test): 
	    regexp {([0-9]{2}:[0-9]{2}:[0-9]{2})+(\.[0-9]{0,6})*} $line fulltime
            set  time  [string range $fulltime 0 8]
	    if { ![regexp {(\.)+} $time dot]} {
                set time [concat $time\.]
            }
            set  msec  [string range $fulltime 9 end]
            set  count 0
            scan $msec "%d" msec
            if {$msec < 0 || $msec > 999999} { set msec 0 }
            set  nmsec [format "%6.6d" $msec]
            regsub {^[0-9]{4}-[0-9]{2}-[0-9]{2} *[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{6} *} $line "" newLine
            puts $rfd "$stamp  $time$nmsec $pnum $newLine"
        } else {
            incr count
            set  nmsec [format "%6.6d" [expr {$msec + $count}]]
            puts $rfd "$stamp  $time$nmsec $pnum $line"
        }
    }
    close $fd
    close $rfd

    # Copy back the new file over the original one
    catch {file rename -force -- $fileName.tmp $fileName}
}

###############################################################################
#
# procedure rmTimeStamp (fileName)
#  strips off the time stamp from the fileName
##############################################################################
proc rmTimeStamp {fileName} {

    printLogVerbose "Removing time stamps from file: $fileName"

    set fd [open $fileName r]
    set rfd [open $fileName.tmp w]
    while { [gets $fd line] >= 0} {
        regsub {^[0-9]{4}-[0-9]{2}-[0-9]{2} *[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{6} *} $line "" newLine
        puts $rfd $newLine
    }
    close $fd
    close $rfd

    # Copy back the new file over the original one
    catch {file rename -force -- $fileName.tmp $fileName}
}

###############################################################################
#
# procedure runProg (name)
#
##############################################################################
proc runProg {name} {
    global env

    if {[file exists [lindex $name 0]]} {
        return $name
    }
    if {[file exists ../bin/[lindex $name 0]]} {
        return "../bin/$name"
    }
    if {[info exists env(INTROOT)] &&
        [file exists $env(INTROOT)/bin/[lindex $name 0]]} {
        return "$env(INTROOT)/bin/$name"
    }
    if {[info exists env(INTLIST)]} {
        set myIntlist "$env(INTLIST)"
        set intlist_list [split $myIntlist :]
        foreach tmpdir $intlist_list {
           if {[file exists $tmpdir/bin/[lindex $name 0]]} {
               return "$tmpdir/bin/$name"
               break
           }
        }
    }
    if {[info exists env(ACSROOT)] &&
        [file exists $env(ACSROOT)/bin/[lindex $name 0]]} {
        return "$env(ACSROOT)/bin/$name"
    }
    if {[info exists env(VLTROOT)] &&
        [file exists $env(VLTROOT)/bin/[lindex $name 0]]} {
        return "$env(VLTROOT)/bin/$name"
    }
    printLog "Could not find program $name"
    exit 1
}

###############################################################################
#
# procedure getRefFile (line)
#
##############################################################################
proc getRefFile {process} {
    set newLine  [split $process /]
    return "[lindex $newLine [expr {[llength $newLine] - 1}]].ref"
}

############################################################################
#
# doMakeEnv
#
############################################################################
proc doMakeEnv {} {

    global gv
    global env

    if { [file exists $gv(sessionFile)] } {
	puts "Test session already active"
    } else {
	execMakeAll $gv(noClean) $gv(TAT_MAKE_ARGS)
        # VLTSW20090238 (JIRA COMP-3949): allocEnv is executed before sourcing the source script
	allocEnv
	# source script handling
	if { $gv(sourceScript) != "" } {
	    tatPuts "Sourcing script..."
	    source $gv(sourceScript) 
	}
	# prologue script handling
	if { $gv(prologueScript) != "" } {
	    tatPuts "Executing prologue script..."
	    set gv(execEpilogue) "ON"
	    if {[ catch {eval exec $gv(prologueScript) $gv(prologueArgs)} out ]} {
		error "The prologue script fails: $out"
	    }
	    tatPuts "End of prologue script"
	}
	replaceScanEnv
	makeEnv
    }

}

############################################################################
#
# doCleanEnv
#
############################################################################

proc doCleanEnv {} {

    global gv
    global env

    if { ![file exists $gv(sessionFile)] } {
	puts "No test session active"
    } else {
	cleanEnv
	# source script handling
	if { $gv(sourceScript) != "" } {
	    tatPuts "Sourcing script..."
	    source $gv(sourceScript) 
	}
	# epilogue script handling
        if { $gv(epilogueScript) != "" } {
	    tatPuts "Executing epilogue script..."
	    if {[ catch {eval exec $gv(epilogueScript) $gv(epilogueArgs)} out ]} {
		error "The epilogue script fails: $out"
	    }
	    set gv(execEpilogue) "OFF"
	    tatPuts "End of epilogue script"
        }
	# SPR 960020 (EAL request)
	execMakeClean $gv(noClean)
    }

}

############################################################################
#
# doRunTest
#
############################################################################

proc doRunTest  {} {

     global env
     global gv
     global repeat

    execMakeAll $gv(noClean) $gv(TAT_MAKE_ARGS)

    if { $gv(testAll) == 1 } {
       #if { [llength $gv(userTestList) ] == 0 || $gv(testAll) == 1 } 

       #  no test. id on the command line: 
       #  if no test session, option "all" by default
       #  if test session active, run all the tests

       if { ![file exists $gv(sessionFile)] } {

           # VLTSW20090238 (JIRA COMP-3949): allocEnv is executed before sourcing the source script
	   allocEnv
	   # source script handling
	   if { $gv(sourceScript) != "" } {
	       tatPuts "Sourcing script..."
	       source $gv(sourceScript) 
  	   }
	   # prologue script handling
	   if { $gv(prologueScript) != "" } {
		tatPuts "Executing prologue script..."
	       set gv(execEpilogue) "ON"
	       if {[ catch {eval exec $gv(prologueScript) $gv(prologueArgs)} out ]} {
	    	    error "The prologue script fails: $out"
	        }
	        tatPuts "End of prologue script"
	   }

	   replaceScanEnv
	   makeEnv
	   # save the environment into tatlog directory
	   copyEnv	   	   
	   # cannot be done any more in allocEnv (vcc conflict)
           setEnv
	   for { set i $repeat } { $i > 0 } {incr i -1 } {
	       runTest $gv(testListToRun) $gv(generate) $i
	   }
	   cleanEnv

	   # epilogue script handling
	   if { $gv(epilogueScript) != "" } {
	        tatPuts "Executing epilogue script..."
	        if {[ catch {eval exec $gv(epilogueScript) $gv(epilogueArgs)} out ]} {
	    	    error "The epilogue script fails: $out"
	        }
	        set gv(execEpilogue) "OFF"
	        tatPuts "End of epilogue script"
	   }

	   # SPR 960020 (EAL request)
	   execMakeClean $gv(noClean)
       } else {

	   # save the environment into tatlog directory
	   copyEnv	   	   
	   # source script handling
	   if { $gv(sourceScript) != "" } {
	       tatPuts "Sourcing script..."
	       source $gv(sourceScript) 
  	   }
           # and environment variables need to be set	
	   setEnv
	   for { set i $repeat } { $i > 0 } {incr i -1 } {
	     runTest $gv(testListToRun) $gv(generate) $i
	   }
	}

    } else {

	   # save the environment into tatlog directory
	   copyEnv	
	      	   
    #  if testid is specified, run only that test

# To BE INVESTIGATED: why is buildLists here repeated?
	#buildLists

	# it has been explictely asked not to stop in case the
	# specififed test id. is wrong (SPR 960586).
	# check that the specified testid are defined in TestList ...
	if { [llength $gv(userTestList) ] != 0 } {
	    set concList {}
	    foreach item $gv(testList) {
	        set concList [concat $concList $item]
	    }
	    foreach userTest $gv(userTestList) {
		if {[lsearch $concList $userTest] == -1 } {
		    puts "$userTest not found in $gv(testListFileName)."
	        }
	    }
	}
	
	if { ![llength $gv(envNameList)] } {
	    # TestList has no LCU/WS env.
	    # source script handling
	    if { $gv(sourceScript) != "" } {
	       tatPuts "Sourcing script..."
	       source $gv(sourceScript) 
  	    }
	    for { set i $repeat } { $i > 0 } {incr i -1 } {
	      runTest $gv(testListToRun) $gv(generate) $i
	    }
	} else {
	    # TestList has LCU/WS env.
	    if { ![file exists $gv(sessionFile)] } {
		error "No test session active"
	    } else {
	        # source script handling
	        if { $gv(sourceScript) != "" } {
	           tatPuts "Sourcing script..."
	           source $gv(sourceScript) 
  	        }
		setEnv
	    }
	    for { set i $repeat } { $i > 0 } {incr i -1 } {
	      runTest $gv(testListToRun) $gv(generate) $i
	    }
	}
    }
}

############################################################################
#
# doRebootLCU (adapted from doMakeEnv + tatMakeLCUEnv) for doLoop only !
#
############################################################################

proc doRebootLCU {} {

    global gv
    global env


    #   WS booting environment

    set i 0
    set le [llength $gv(envNameList)]
    set wsEnvList {}

    catch { set wS RTAPENV }
    while { $i < $le } {
	set envName [lindex $gv(envNameList) $i]
	set envType [lindex $gv(envNameList) [expr {$i + 1}]]

	if { $envType == "WS" || $envType == "QS" } {
	    set wS $envName
	    set i [expr {$le + 1}]
	}

	incr i
	incr i
    }

    # reboot LCU
    
    set i 0
    set le [llength $gv(envNameList)]
    while { $i < $le } {

	set envName [lindex $gv(envNameList) $i]
	set envType [lindex $gv(envNameList) [expr {$i + 1}]]

	if { $envType == "LCU"  } {
	    
	    if {[catch {set LCUEnvName $env($envName)}]} {set LCUEnvName "?"}
	    if {[catch {set WSEnv $env($wS)}]} {
		error "tatMakeLCUEnv: $wS environment variable not defined."
	    }

	    # new userScript

	    file copy -force -- ./ENVIRONMENTS/$envName/userScript \
		    $gv(VLTDATA)/ENVIRONMENTS/$LCUEnvName

	    # execute the bootChange sequence

	    tatPuts "$envName rebooting vxWorks."

	    if {[ catch { exec vccEnvInit -e $LCUEnvName -w $WSEnv} out ]} {
		error "tatMakeLCUEnv: vccEnvInit -e $LCUEnvName: $out"
	    }

	    # reboot with vxWorks and execute bootscript.

	    tatPuts "Executing bootScript of $envName."

	    if {[ catch { exec vccEnvStart -v -e $LCUEnvName -w $WSEnv} out ]} {
	       set oldOut $out
	       set newOut \
		[read_file $gv(VLTDATA)/ENVIRONMENTS/$LCUEnvName/.reboot.log]
	       set out "$oldOut $newOut"
	       error "tatMakeLCUEnv failed vccEnvStart -v -e $LCUEnvName: $out"
	    }

	    tatPuts "$envName environment successfully rebooted."
	    break
	}

	incr i

    }


}

#############################################################################
#
# doLoop: run tat on a module group
#
#############################################################################

proc doLoop {} {

    global gv

    set moduleTestList ""

    set ltd ""
    set ldir [readdir .]

    foreach dir $ldir {
	if {[file isdirectory $dir]} {
	    if {[file isfile "$dir/test/TestList"]} {
		set ltd "$ltd $dir/test"
	    } elseif {[file isfile "$dir/ws/test/TestList"]} {
		set ltd "$ltd $dir/ws/test"
	    } elseif {[file isfile "$dir/lcu/test/TestList"]} {
		set ltd "$ltd $dir/lcu/test"
	    }
	}
    }

    tatPuts "looping over:"
    foreach dir $ltd {
	tatPuts "- $dir"
    }
    tatPuts ""

    set moduleTestList $ltd
    set cwd [pwd]
    set firstDir ""

    foreach dir $moduleTestList {
	tatPuts "in $dir:"
	# first dir: run also makeEnv
	# others   : run all tests
	# last dir : run also cleanEnv
	cd $dir
	if { $firstDir == "" } {
	    set firstDir [pwd]
	    set gv(makeEnvOn) 1
	    doCurrent
	    doRunTest
	    set gv(makeEnvOn) 0
	} else {
	    # to reuse environments, link $testSession
	    # to filter environments, link $sedFileBis
	    # ($grepFileBis on generated in each test directory)
	    # => environments variables should be the same !!!
	    # 
	    # if error/interruption: 'cleanEnv' OK but .testSession not removed
	    #
	    file delete -force -- $gv(sessionFile)
	    link -sym $firstDir/$gv(sessionFile) $gv(sessionFile)
	    link -sym $firstDir/$gv(sedFileBis) $gv(sedFileBis)
	    # reboot the LCU with new userScript
	    doRebootLCU
	    doCurrent
	    file delete -force -- $gv(sessionFile)
	    file delete -force -- $gv(sedFileBis)
	}
	cd $cwd
    }
    # last dir
    cd $firstDir
    set gv(cleanEnvOn) 1
    doCleanEnv
    set gv(cleanEnvOn) 0

}

#############################################################################
#
# prepareTestList: prepare TestList name and do some checks
#
#############################################################################

proc prepareTestList {} {

global env
global gv
global PID

#
# Select TestList file:
#
#    if NOCCS     TestList.NOCCS
#    if CCSlite   TestList.lite
#    if full CCS or if TestList.NOCCS or TestList.lite is missing: TestList
#    TESTLIST is maintained, after the merge with eccsTestDriver
#

if { $gv(commandLineTestLists) == 1 } {
    if {![file exists "$gv(testListFiles)"]} {
	puts "$gv(testListFiles) does not exist!"
	exit 1
    }
    set gv(testListFileName) $gv(testListFiles)
} else {
   if {[ catch { set gv(NOCCS) $env(NOCCS) } ] || ("$env(VLTSW_CCSTYPE)" != "no" && "$env(VLTSW_CCSTYPE)" != "noccs") } {
       set gv(NOCCS) 0
       if {[ catch { set gv(withRtap) $env(RTAPROOT) } ]} {
           set gv(withRtap) 0
           if {[file exists "TestList.lite"]} {
               set gv(testListFileName) "TestList.lite"
      	   } elseif {[file exists "TestList"]} {
	       set gv(testListFileName) "TestList"
           } elseif {[file exists "TESTLIST"]} {
               # Here for backward compatibility
               puts "WARNING!: Using old config file TESTLIST. Rename it TestList"
               set gv(testListFileName) "TESTLIST"
	   } else {
	       puts "TestList or TestList.lite does not exist!"
	       exit 1
	   }
       } else {
            set gv(withRtap) 1
	    if {[file exists "TestList"]} {
                set gv(testListFileName) "TestList"
	    } elseif {[file exists "TESTLIST"]} {
	        # Here for backward compatibility
                puts "WARNING!: Using old config file TESTLIST. Rename it TestList"
                set gv(testListFileName) "TESTLIST"
            } else {
                puts "TestList or TESTLIST does not exist!"
                exit 1
            }
        }
   } else {
       set gv(NOCCS) 1
       set gv(withRtap) 0
       if {[file exists "TestList.NOCCS"]} {
           set gv(testListFileName) "TestList.NOCCS"
       } elseif {[file exists "TestList"]} {
           set gv(testListFileName) "TestList"
       } elseif {[file exists "TESTLIST"]} {
           # Here for backward compatibility
           puts "WARNING!: Using old config file TESTLIST. Rename it TestList"
           set gv(testListFileName) "TESTLIST"
       } else {
           puts "TestList or TestList.NOCCS does not exist!"
           exit 1
       }
   }
}

#
# build env. lists and tests lists
#

if { [buildLists] == 1 } {
     error "cannot analyse TestList"
}

#
# check env.lists and tests lists
#

if { [catch checkList errmsg] == 1 } {
    puts "$errmsg"
    error "cannot process TestList"
}

if { [llength $gv(testList)] == 0 } {
    error "No test specified in TestList"
}

# Note: on Solaris, vccEnvCreate fails (rcp: (...) Permission denied) if user
# directory contains read-only sudirectories because rcp keep this read only
# mode for the target directory (true whether tat user is the owner
# or not of the directories).
set dl ""
if { [file isdirectory ./ENVIRONMENTS] } {
    # check only for tests with environments
    set dl [exec find ./ENVIRONMENTS -type d -a ! -perm -0200 -print]
    if { $dl != "" } {
        error "all ./ENVIRONMENTS sub directories  should be writable"
    }
}

#
# create ./ref and ./out if they don't exist
# to avoid 'tat' failures.

if { ![file isdirectory ./tatlogs] } {
    file mkdir ./tatlogs
    tatPuts "Created log directory ./tatlogs"
}

if { ![file isdirectory ./tatlogs/run$PID] } {
    file mkdir ./tatlogs/run$PID
    tatPuts "Created current log directory ./tatlogs/run$PID"
}

if { ![file isdirectory ./ref] } {
    file mkdir ./ref
    tatPuts "Created ./ref"
}


# test id. given on the command line overrides TestList
if { $gv(testAll) == 0 } {
  if { [llength $gv(userTestList)] != 0 } {
    foreach test $gv(userTestList) {
      lappend gv(testListToRun) $test
    }
  } 
  if { [llength $gv(toTest)] != 0 } {
    foreach number $gv(toTest) {
	set element_index [ lsearch $gv(testList) $number* ]
	lappend gv(testListToRun) [ lindex $gv(testList) $element_index ]
    }
  }
} else {
    set gv(testListToRun) $gv(testList)
}

}

#############################################################################
#
# proc preparePath
#
#############################################################################

proc preparePath {} {

global env
global gv

# let's redefine SHLIB_PATH/LD_LIBRARY_PATH to allow
# the module under test to include it's shared libraries

    if { [file isdirectory ../lib] } {
        cd ../lib
        set libPath [pwd]
        cd ../test
        if {![catch {set env(SHLIB_PATH) $libPath:$env(SHLIB_PATH)}]} {
            tatPuts "Redefined SHLIB_PATH to: $env(SHLIB_PATH)"
        }
        if {![catch {set env(LD_LIBRARY_PATH) $libPath:$env(LD_LIBRARY_PATH)}]} {
            tatPuts "Redefined LD_LIBRARY_PATH to: $env(LD_LIBRARY_PATH)"
        }
        if { $env(OSYSTEM) == $env(CYGWIN_VER) } {
            if {![catch {set env(PATH) $libPath:$env(PATH)}]} {
                tatPuts "Redefined PATH to: $env(PATH)"
            }
        }
    } elseif {[file exists ../lib]} {
        error "Non-directory ../lib already exists."
    } else {
        if { [catch {file mkdir ../lib} msg] } {
    	    error "Cannot create directory ../lib"
        } else {
	    cd ../lib
	    set libPath [pwd]
	    cd ../test
	    if {![catch {set env(SHLIB_PATH) $libPath:$env(SHLIB_PATH)}]} {
	        tatPuts "Redefined SHLIB_PATH to: $env(SHLIB_PATH)"
	    }
	    if {![catch {set env(LD_LIBRARY_PATH) $libPath:$env(LD_LIBRARY_PATH)}]} {
	        tatPuts "Redefined LD_LIBRARY_PATH to: $env(LD_LIBRARY_PATH)"
	    }
       if { $env(OSYSTEM) == $env(CYGWIN_VER) } {
           if {![catch {set env(PATH) $libPath:$env(PATH)}]} {
               tatPuts "Redefined PATH to: $env(PATH)"
           }
       }
        }
    }
    if {[info exists env(PYTHONPATH)]} {
        if {![catch {set env(PYTHONPATH) $libPath/python/site-packages:$env(PYTHONPATH)}]} {
            tatPuts "Redefined PYTHONPATH to: $env(PYTHONPATH)"
        }
    }
    if {[info exists env(IDL_PATH)]} {
        if {![catch {set env(IDL_PATH) "-I ../idl $env(IDL_PATH)"}]} {
            tatPuts "Redefined IDL_PATH to: $env(IDL_PATH)"
        }
    }
#
# let's redefine PATH to allow RtapScheduler to find also module
# binaries (if invoked via msgScheduleProcess): search path is based on
# the value of PATH at environment starting.
#                                  ^^^^^^^^

    if { [file isdirectory ../bin] } {
        cd ../bin
        set binPath [pwd]
        cd ../test
        set env(PATH) $binPath:$env(PATH)
    } elseif {[file exists ../bin]} {
        error "Non-directory ../bin already exists."
    } else {
        if { [catch {file mkdir ../bin} msg] } {
            error "Cannot create directory ../bin"
        } else {
    	    cd ../bin
	    set binPath [pwd]
	    cd ../test
	    set env(PATH) $binPath:$env(PATH)
        }
    }
}

#############################################################################
#
# doCurrent: run tat on the current test directory
#
#############################################################################

proc doCurrent {} {

global env
global gv
global PID

preparePath

# execute user's request at module level

if { $gv(makeEnvOn) == 1 } {

    doMakeEnv

  } elseif { $gv(cleanEnvOn) == 1 } {

    doCleanEnv

  } else {

    doRunTest

  }

}

#############################################################################
#
# main body of tat
#
#############################################################################

proc tatBody {} {

global gv
global env
global verbose noorder repeat waitAtEnd log 
global argc argv0 argv
global PID

set PID [ pid ]
set UnknownOption 0
set loop 0
set multipleTestLists 0
set gv(commandLineTestLists) 0
set verbose 0 
set trace 0
set gv(generate) 0
set gv(makeEnvOn) 0
set gv(cleanEnvOn) 0
set gv(noClean) 0
set gv(testAll) 1
set iterations 0
set gv(userTestList) {}
set gv(envCreatedNow) 0
set gv(sourceScript) {}
set gv(prologueScript) {}
set gv(prologueArgs) {}
set gv(epilogueScript) {}
set gv(epilogueArgs) {}
set gv(toTest) {}
set gv(execEpilogue) "OFF"


# Used to log also output to logManager
set log          0

# Used to define if message order must be taken into account or ignored
set noorder      1

# Used to define the number of times the test is repeated
if {[catch {set repeat $env(REPEAT)}]} {
    set repeat      1
}

# Used to define the number of seconds to wait before killing
# processes at end of test
set waitAtEnd   0


if {[catch {set gv(TAT_MAKE_ARGS) $env(TAT_MAKE_ARGS)}]} {
    set gv(TAT_MAKE_ARGS) ""
} 

if {[catch {set gv(RHOST) $env(RHOST)}]} {
    set env(RHOST) $env(HOST)
    set gv(RHOST) $env(HOST)
}
 
if {[catch {set gv(RHOSTQS) $env(RHOSTQS)}]} {
    set env(RHOSTQS) $env(HOST)
    set gv(RHOSTQS) $env(HOST)
}

set env(NO_ERR_DISPLAY) "TRUE"

#
# test environment variables
#

if {[catch {set gv(HOST) $env(HOST)}]} {
    error "HOST undefined" 
}


if {[catch {set gv(VLTDATA) $env(VLTDATA)}]} {
    error "VLTDATA undefined"
}

if {[catch {set gv(LOGNAME) $env(LOGNAME)}]} {
    error "LOGNAME undefined"
}

set gv(testList) {}
set gv(testListFiles) {}
set ALL_FLAG 0

#
# Parse command line options
#

while {! [lempty $argv]} {
    set arg [lvarpop argv]
    switch -regexp -- $arg {
     	        -l         { set loop 1; 
	                     # module list parameter is optional
	                     set ml [lvarpop argv] 
		             if {[cindex $ml 0] == "-"}  {
				 set argv [linsert $argv 0 $ml]
                                 set ml ""
			     } else {
				 set gv(moduleList) $ml
			     }
                           }
		-f 	   { set gv(commandLineTestLists) 1;
			     set gv(testListFiles) [lvarpop argv] 
			     if {[llength $gv(testListFiles)] > 1} {
				set multipleTestLists 1
			      }
			    }
	        -v         { set verbose 1 }
	        -t         { set trace 1 }
                -run        { set gv(generate) 0 }
		-g         { set gv(generate) 1 }
		-nc        { set gv(noClean) 1 }
		-s         { set UnknownOption 1 }
                -log        { set log 1 }
                -noorder    { set noorder 1 }
                -order      { set noorder 0 }
                -r[0-9]+    { set repeat [string range $arg 2 end]}
                -r          { set repeat [lvarpop argv]}
                -w[0-9]+    { set waitAtEnd [string range $arg 2 end]}
                -w          { set waitAtEnd [lvarpop argv]}
		"^all$"      { set gv(testAll) 1 ; set ALL_FLAG 1 }
		"^ALL$"      { set gv(testAll) 1 ; set ALL_FLAG 1 }
		"^makeEnv$"  { set gv(makeEnvOn) 1 }
		"^cleanEnv$" { set gv(cleanEnvOn) 1 }
                "^[0-9]+"     { set gv(testAll) 0; lappend gv(toTest) $arg }
		default	   { set gv(userTestList) [concat $gv(userTestList) $arg] ; 
			     set gv(testAll) 0
			   }
	    }
	}

if { $trace == 1 } {
    cmdtrace on
}

#
# check command line
#

# -s option was foreseen but never implemented.
if { $UnknownOption == 1 } {
    puts "-s: invalid option."
    usage
}

set userTest 0
if { [llength $gv(userTestList)] != 0 } {
    set userTest 1
}


if { $gv(generate) == 1 }  {
    if { $gv(makeEnvOn) == 1 || $gv(cleanEnvOn) == 1 } {
	error "tat $argv: too many arguments for -g option"
    }
}


if {($gv(makeEnvOn) == 1) && (($gv(cleanEnvOn) == 1) || ($ALL_FLAG == 1)|| \
    ($gv(generate) == 1) || ($userTest == 1) || ($loop == 1)) } {
	error "tat $argv: too many arguments: makeEnv options exclude other options"   
}


if {($gv(cleanEnvOn) == 1) && (($gv(makeEnvOn) == 1) || \
    ($ALL_FLAG == 1) || ($gv(generate) == 1) || ($userTest == 1) || ($loop == 1))} {
	error "tat $argv: too many arguments: cleanEnv options exclude other options"   
}

#
# use environment variable for global verbose mode
#
if { $verbose == 1 } {
    set env(TAT_VERBOSE) 1
}

# Activate log on stdout of CCS log messages, if the option is selected
if { $log } {
    set env(LOG_ON_STDOUT) 1
}

#
# tat on a module group or the current module ?
#
if { $loop == 1 } {
    doLoop
} else {
    doCurrent
}


}

#############################################################################
# tat entry point
#############################################################################

signal trap { SIGINT SIGQUIT SIGABRT }  { 
    # do not release environments if they were started in a different
    # tat process (i.e. "tat makeEnv" has been run before "tat").
    if { $gv(envCreatedNow) == 1 } {
	puts "Interrupted. Releasing environments. Please wait ..."
	catch cleanEnv fatalError 
    } else {
	puts "Interrupted."
    }
    if { $gv(execEpilogue) == "ON" } {
	if { $gv(epilogueScript) != "" } {
	    tatPuts "Executing epilogue script..."
	    if {[ catch {eval exec $gv(epilogueScript) $gv(epilogueArgs)} out ]} {
		error "The epilogue script fails: $out"
	    }
	    tatPuts "End of epilogue script"
	}
    }
    exit 2
}

if { [catch { tatBody } result] } {

    puts "Error: $result"
    #   release the allocated environments if done by this process
    if { $gv(envCreatedNow) == 1 } {
	puts "Releasing environments. Please wait ..."
	catch cleanEnv fatalError
	file delete -force -- $gv(sessionFile)
    } elseif { [file exists $gv(sessionFile)] && 
               [file size $gv(sessionFile)] == 0 } {
	# always remove session file if no allocated environment
	file delete -force -- $gv(sessionFile)
    }
    #   execute the epilogue script (if any) to clean up processes
    if { $gv(execEpilogue) == "ON" } {
	if { $gv(epilogueScript) != "" } {
	    tatPuts "Executing epilogue script..."
	    if {[ catch {eval exec $gv(epilogueScript) $gv(epilogueArgs)} out ]} {
		error "The epilogue script fails: $out"
	    }
	    tatPuts "End of epilogue script"
	}
    }
   #   remove logs directory if empty
   if { [file isdirectory ./tatlogs/run$PID] } {
       if { [readdir ./tatlogs/run$PID] == "" } {
    	   file delete -force -- ./tatlogs/run$PID
	   tatPuts "Removing empty log directory ./tatlogs/run$PID"
       }
   }
   #   result contains an error message
   exit 1

} else {

   #   remove logs directory if empty
   if { [file isdirectory ./tatlogs/run$PID] } {
       if { [readdir ./tatlogs/run$PID] == "" } {
    	   file delete -force -- ./tatlogs/run$PID
	   tatPuts "Removing empty log directory ./tatlogs/run$PID"
       }
   }
   # result contains the value returned by tatBody (i.e. nothing)
    exit 0
}

#
# ___oOo___

