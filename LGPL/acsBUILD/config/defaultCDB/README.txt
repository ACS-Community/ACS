defaultCDB
-----------

This is the default CDB provided with ACS.

It is provided as an example and as a basic configuration to run ACS
and to play with it.

It is used by default if the ACS_CDB environment variable is not set
somewhere else.

Notice that some components are related to the ALMA Archive and
defined in Components.xml for convenience, but not functional unless
the ALMA Archive itself is installed.

Components are deployed on the following default containers:

- bilboContainer
  is a C++ container

- frodoContainer 
  is a Java container
  Some components rely on the availability of other components 
  allocated to bilboContainer

- aragornContainer
  is a Python container

It is necessary to have the three containers running to get all functionality.

