'''
The Common subpackage is of primary interest to all
ALMA developers wishing to use Python. The modules here
are of common use to everyone. A few brief descriptions
on the modules follow:
- Callbacks contains complete BACI callback implementations. This
  functionality has not been provided in Java or C++.
- CDBAccess contains an easy to use class which provides read-only access
  to the ACS configuration database.
- DurationHelper is a class which wraps the acstime::Duration structure
- EpochHelper is a class which wraps the acstime::Epoch structure
- Err contains the class which all (Python) ACS Error System generated
  exceptions/completions are derived from. Look at this module to see
  exactly what functionality is provided.
- Log contains a class capable of accessing the ACS logging system. See the
  Logger class for details.
- QoS contains functions for changing the quality of service attributes of
  CORBA object references.
- TimeHelper provides some general purpose classes and functions for dealing
  with the unique ACS time format.
'''
__revision__ = "$Id: __init__.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
