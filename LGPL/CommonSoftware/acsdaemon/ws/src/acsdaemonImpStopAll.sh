#! /bin/bash
. acsstartupAcsPorts

HOST=`getIP`

acsdaemonImpStop -r corbaloc::$HOST:3015/NamingServiceImp
acsdaemonImpStop -r corbaloc::$HOST:3016/NotificationServiceImp
acsdaemonImpStop -r corbaloc::$HOST:3017/ConfigurationDatabaseImp
acsdaemonImpStop -r corbaloc::$HOST:3018/ManagerImp
acsdaemonImpStop -r corbaloc::$HOST:3019/ACSLogServiceImp
acsdaemonImpStop -r corbaloc::$HOST:3020/LoggingServiceImp
acsdaemonImpStop -r corbaloc::$HOST:3021/InterfaceRepositoryImp
