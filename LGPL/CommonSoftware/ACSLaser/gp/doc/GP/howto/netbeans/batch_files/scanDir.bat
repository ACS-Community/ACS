@echo off
SET FILE_LIST=
IF not exist "%1" goto NO_DIR
FOR %%f IN ( "%1\*.jar" ) DO CALL add2path.bat %%f
FOR %%f IN ( "%1\*.zip" ) DO CALL add2path.bat %%f
:NO_DIR