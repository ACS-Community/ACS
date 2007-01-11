Instruction for testing supplier (samp)-consumer in Java
-------------------------------------------------

1. compile acssamp both in src and test


2. prepare the CDB 

cd <...>/acssamp/ws/test
mkdir -p  CDB/MACI/Containers/Container
cp ENVIRONMENTS/wsTat/Container.xml CDB/MACI/Containers/Container/Container.xml


3. start ACS

export ACS_CDB=<...>/acssamp/ws/test
acsStart
acsStartContainer -cpp Container


4. start Java supplier

acsStartJava alma.acssamp.jtest.acssampSupplier
(it samples LAMP1/brightness; using ObjExp it is possible to change the value of brightness)


5. start Java consumer

acsStartJava alma.acssamp.jtest.acssampJTest

On stdout the value of the brightness (with associated timestamp should be written)

