@echo off
REM param1 root output directory for ALMA IDL compiler (usually some temporary directory)
REM param2 IDL include directory1, e.g. project's /idl directory
REM param3 IDL include directory2, e.g. INTROOT/idl
REM param4 IDL include directory3, e.g. ACSROOT/idl
REM param5 the idl file that should be compiled
REM param6 flag for generation of component helper classes ("true" will do, otherwise not)
REM param7 root output directory for component helper generator (usually some /src directory)

REM example call from XmlIdl/test
REM runXmlIdl.bat ..\src_generated ..\idl Z:\introot\idl Y:\alma\ACS-2.0\ACSSW\idl ..\idl\xmltest.idl true ..\src_generated

@echo off

rem will be removed later, as the Makefile is supposed to provide us with the all-powerfull classpath
rem
set CLASSPATH=../lib/xmlidl.jar;../lib/openorb13Debug.jar;../lib/logkit.jar;../lib/avalon-framework.jar;Z:\introot\lib\comphelpgen.jar
set CLASSPATH=%CLASSPATH%;../lib/castor.jar
set CLASSPATH=%CLASSPATH%;Y:/alma/ACS-2.0/ACSSW/lib/endorsed/xercesImpl.jar;


set AlmaIDLOutRootDirDef=-d %1
set AlmaIDLIncludeDef=-I %2 -I %3 -I %4
set AlmaIDLMainClass=alma.tools.idlgen.XmlIdlCompiler

set CompHelpDoGenerateDef=-Dalma.acs.tools.comphelpergen.doGenerate=%6
set CompHelpOutRootDirDef=-Dalma.acs.tools.comphelpergen.outRootDir=%7

rem feel free to toggle this for debug output
rem set verboseDef=-verbose
set verboseDef=

@echo on

java %CompHelpDoGenerateDef% %CompHelpOutRootDirDef% -cp %CLASSPATH% %AlmaIDLMainClass% %AlmaIDLOutRootDirDef% %AlmaIDLIncludeDef% -notie %verboseDef% %5



