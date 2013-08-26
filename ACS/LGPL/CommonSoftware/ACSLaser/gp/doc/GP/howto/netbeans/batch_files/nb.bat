@echo off
set NBHOME=.

rem -----------------------------------------------------------------
rem   scan system dirs
rem -----------------------------------------------------------------

CALL scanDir.bat %NBHOME%\lib
set LIB_LIST=%FILE_LIST%

CALL scanDir.bat %NBHOME%\lib\patches
set PATCH_LIST=%FILE_LIST%

CALL scanDir.bat %NBHOME%\lib\ext
set EXT_LIST=%FILE_LIST%

set CLASSPATH=%PATCH_LIST%%LIB_LIST%%EXT_LIST%

rem -----------------------------------------------------------------
rem    scan user dirs
rem -----------------------------------------------------------------

set NBUSER=%NBHOME%\user

CALL scanDir.bat %NBUSER%\lib
set LIB_LIST=%FILE_LIST%

CALL scanDir.bat %NBUSER%\lib\patches
set PATCH_LIST=%FILE_LIST%

CALL scanDir.bat %NBUSER%\lib\ext
set EXT_LIST=%FILE_LIST%

set CLASSPATH=%CLASSPATH%%PATCH_LIST%%LIB_LIST%%EXT_LIST%


echo classpath=%CLASSPATH%
%JAVA_HOME%\bin\java -Djava.security.policy=%NBHOME%\nb.policy ^
                     -Dnetbeans.home=%NBHOME% ^
                     -Dnetbeans.user=%NBUSER% ^
                     org.netbeans.Main
