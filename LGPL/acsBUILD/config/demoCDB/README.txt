demoCDB
-----------

This is an exmaple of CDB provided with ACS and good for giving demos.

In order to use it:

      export ACS_CDB = $ACSDATA/config/demoCDB

in the window where the ACS services are started from.

Components are deployed on the following default containers:

- bilboContainer
  is a C++ container

- frodoContainer 
  is a Java container
  Some components rely on the availability of other components 
  allocated to bilboContainer

With respect to the defaultCDB this one has the following differences:

- only components that can be activated with the pure 
  ACS installation are defined in Components.xml
- LAMP1.brightness has a min and max value useful for demonstrating
  the concept.
- PBEND_B_01 is archiving monitor data at 1s frequency and not 10s,
  to demonstrate monitoring functionality.


For more details on ACS demos look here:

      http://almasw.hq.eso.org/almasw/bin/view/ACS/DemoMaterial

This wiki discussion will be kept up to date.
