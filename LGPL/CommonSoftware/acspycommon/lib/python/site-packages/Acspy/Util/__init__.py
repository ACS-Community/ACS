'''
Util is a quite complicated subpackage most developers can safely ignore. In a
nutshell, many utility functions designed to:
* access manager and CORBA services
* obtain ACS_INSTANCE related info
* find files located in the $INTROOT/$ACSROOT directory structure
* access the Naming Service in a nice way
* etc
can be found here. Most of the modules in this package really should only be used internally by ACS
but perhaps other subsystems can make use of it too. Some of the general use modules are: BaciHelper,
Profiler, and Scheduler.
'''
__revision__ = "$Id: __init__.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
