#! /bin/bash

### 
### mschilli (2005-04-27)
###
### This script creates a Java Helpset from 
### 1) the meta-files in the doc directory
### 2) the html-files in the Acs documents section
### 
### The result will be stored in the lib directory
###

DIR=createHelp.tmp

rm -rf $DIR >/dev/null 2>&1
mkdir $DIR
mkdir $DIR/html
cd $DIR

###
### get helpset metafiles from doc-directory
###
cp -r ../../doc/* .
find . -name "CVS" -exec rm -rf {} 2>/dev/null \;
find . -name ".cvsignore" -exec rm {} \;

###
### get html content from Acs-Documents
###
# export the "most recent version no later than today" [cvs doc]
cvs -Q export -D today ACS/Documents/ACSCommandCenter
mv ACS/Documents/ACSCommandCenter/* html
rm -rf ACS


###
### create archive
###
jar cf ../../lib/AcsCommandCenterHelp.jar *

cd ..
rm -rf $DIR

echo done.


