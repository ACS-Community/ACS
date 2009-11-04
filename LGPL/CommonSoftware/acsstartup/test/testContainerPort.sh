#! /bin/bash

acsdataClean -all > /dev/null

if [ "`acsList`" != "" ]
then
    echo "ERROR - other instance of ACS seem to be running. Cannot run this test."
    exit 1
fi

function testParams
{
echo "---------"
echo "--Params: $@"
acsstartupContainerPort "$@"
echo ""
}

export testParams

#-----------------------------------------------------------
echo "*Testing basic params*"
testParams -cpp  cpp1
testParams -java java1
testParams -py   py1
echo ""

echo "*Testing funny named params*"
testParams -cpp  funny/name/cpp1
testParams -java funny/name/java1
testParams -py   funny/name/py1
echo ""

echo "*Testing some more params*"
testParams -cpp  -name cpp2
testParams -java -name java2
testParams -py   -name py2
echo ""

echo "*Testing manager param*"
testParams -cpp  -name cpp2 -m corbaloc::some_host:666/Manager
testParams -java -name java2 -m corbaloc::some_host:666/Manager
testParams -py   -name py2 -m corbaloc::some_host:666/Manager
echo ""

echo "*Testing CDB param*"
testParams -cpp  -name cpp2 -m corbaloc::some_host:666/Manager   -d corbaloc::some_host:123/DAL
testParams -java -name java2 -m corbaloc::some_host:666/Manager  -d corbaloc::some_host:123/DAL
testParams -py   -name py2 -m corbaloc::some_host:666/Manager    -d corbaloc::some_host:123/DAL
echo ""

echo "*Testing replacement exe param*"
testParams -cpp  -name cpp2  -e fakeContainerExe
testParams -java -name java2 -e fakeContainerExe
testParams -py   -name py2   -e fakeContainerExe
echo ""

echo "*Testing remote host*"
testParams -cpp  -name cppRH  -remoteHost 127.0.0.1
testParams -java -name javaRH -remoteHost 127.0.0.1
testParams -py   -name pyRH   -remoteHost 127.0.0.1
echo ""

echo "*Testing different baseports"
testParams -cpp  -name cppB  -b 0
testParams -java -name javaB -b 1
testParams -py   -name pyB   -b 2
echo ""

echo "*Testing custom Java container library*"
testParams -java -name java2 -custom_java fake.java.container.class
echo ""

echo "*Testing Java remote debuggable*"
testParams -java -name java2 -remoteDebuggable
echo ""

echo "*Testing passthroughProcessStart*"
testParams -java -name java2 --passthroughProcessStart="-maxHeapSize 256m"
echo ""

echo "*Testing debug*"
testParams -java -name java2 -debug
echo ""

echo "*Testing specific TCP ports*"
testParams -cpp  -name cpp3  -port 4444
testParams -java -name java3 -port 4445
testParams -py   -name py3   -port 4446
echo ""

echo "*Testing extra parameters*"
testParams -cpp  cpp3  stuff
testParams -java java3 more stuff
testParams -py   py3   yet more stuff
testParams -cpp  -name cpp3  stuff
testParams -java -name java3 more stuff
testParams -py   -name py3   yet more stuff
echo ""

echo "*Hard tests*"
testParams -cpp -name cpp4 -port 5555 -remoteHost 127.0.0.1 -m corbaloc::someManagerHost:3200/Manager -d corbaloc::someDALHost:3211/DAL -e myMaciContainer -b 9 -debug
testParams -cpp -name java4 -port 5556 -remoteHost 127.0.0.1 -m corbaloc::someManagerHost:3200/Manager -d corbaloc::someDALHost:3211/DAL -e myJavaContainer -b 9 -custom_java my.own.container

testParams -cpp -name cpp4 -port 5555 -remoteHost 127.0.0.1 -m corbaloc::someManagerHost:3200/Manager -d corbaloc::someDALHost:3211/DAL -e myMaciContainer -b 9 extra args
testParams -cpp -name java4 -port 5556 -remoteHost 127.0.0.1 -m corbaloc::someManagerHost:3200/Manager -d corbaloc::someDALHost:3211/DAL -e myJavaContainer -b 9 -custom_java my.own.container extra args
