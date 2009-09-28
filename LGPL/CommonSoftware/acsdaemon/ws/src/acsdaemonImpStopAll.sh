#! /bin/bash
. acsstartupAcsPorts

HOST=`getIP`

acsdaemonImpStop -r corbaloc::$HOST:2981/NamingServiceImp
acsdaemonImpStop -r corbaloc::$HOST:2982/NotificationServiceImp
acsdaemonImpStop -r corbaloc::$HOST:2983/ConfigurationDatabaseImp
acsdaemonImpStop -r corbaloc::$HOST:2984/ManagerImp
acsdaemonImpStop -r corbaloc::$HOST:2985/ACSLogServiceImp
acsdaemonImpStop -r corbaloc::$HOST:2986/LoggingServiceImp
acsdaemonImpStop -r corbaloc::$HOST:2987/InterfaceRepositoryImp
acsdaemonImpStop -r corbaloc::$HOST:2988/AlarmServiceImp
