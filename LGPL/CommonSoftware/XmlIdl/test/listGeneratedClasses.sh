#! /bin/bash

#
# Extract all java sources from all xmltest*.jar files to tmp directories.
# TAT can then compare the content of all files with the generated ref file.
# We do not remove the java files that the normal IDL compiler has generated
# because if our XmlIdl compiler erroneously creates files with clashing names
# then we should also detect this in case of files getting overwritten.
#

idlJarFiles=$(find "`pwd`/../lib" -name "xmltest*.jar" | sort)

mkdir -p unpacked_jars
cd unpacked_jars

for jarFile in $idlJarFiles; do
  idlName=$(basename $jarFile .jar)
  echo "======================= IDL file $idlName"
  echo
  mkdir -p $idlName
  cd $idlName
  jar xf $jarFile src
  cd src
  javaFiles=$(find . -name "*.java" | sort)
  for i in $javaFiles; do
    echo "----------------------- Java file $i"
    cat "$i"
    echo
  done
  cd ../..
done

cd ..

