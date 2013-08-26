TMCDB SQL TABLE DEFINITIONS     VERSION 2.2.1  2010-08-22T0000:00:00.0
NOTE
"
=============================================
|  EXTRA SW CONFIGURATION TABLES FOR TMCDB  |
=============================================

This file contains the definition of the following tables for the TMCDB:
 - TMCDBVersion
 - AcsService
 - MasterComponent
 - NetworkDeviceSnmpConfig
 - SnmpTrapSink
 - NetworkPowerstrip
 - PowerstripSocket
" 
ENDNOTE

INCLUDE "classpath:/generic/TMCDB_swconfigcore.ddl"

MODELNAME SwExt

// The TMCDBVersion table records the database name, version and date from this file.  
// It is automatically filled by the generated CreateHsqldbTables.sql script 
// (and by scripts for other DBs, such as CreateOracleTables.sql, generated in later modules).
// There is only one row in this table.  It must be updated each time the structure of the database changes.
TABLE TMCDBVersion
     DBName                  LONGVARCHAR (32)            NOT NULL
     DBVersion               LONGVARCHAR (32)            NOT NULL
     DBDate                  LONGVARCHAR (32)            NOT NULL
     KEY DBName
ENDTABLE


// The AcsService table records the ACS services to be run when starting ACS.
//
// ServiceType              The type of service that this entry describes. 
//                          Must be one of NAMING, IFR, CDB, NOTIFICATION, LOGGING, MANAGER, ALARM, LOGPROXY
// ServiceInstanceName      Only applicable for ServiceType = 'NOTIFICATION' that runs with multiple instances.
//                          For the four notify service instances required by ACS, use their proper names 
//                          ("NotifyEventChannelFactory", "AlarmNotifyEventChannelFactory", "LoggingNotifyEventChannelFactory", "ArchiveNotifyEventChannelFactory"). 
//                          For additional notify services that can host user-created notification channels, use a freely chosen name. 
//                          See also ChannelMapping#NotificationService. 
// ComputerId               Reference to the computer on which this service should be run.
//                          Note that spreading the ACS services over different computers can improve performance. 
TABLE AcsService
    AcsServiceId            INTEGER                 NOT NULL
    ConfigurationId         INTEGER                 NOT NULL
    ServiceType             LONGVARCHAR (12)        NOT NULL
    ServiceInstanceName     LONGNAME                NULL
    ComputerId              INTEGER                 NOT NULL
    KEY AcsServiceId GENERATED
    CONSTRAINT AcsServiceConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration CASCADING INVERSE AGGREGATION
    CONSTRAINT AcsServiceComputer FOREIGN KEY (ComputerId) REFERENCES Computer CASCADING AGGREGATION
    CONSTRAINT AcsServiceServiceType CHECK (ServiceType IN ('NAMING', 'IFR', 'CDB', 'NOTIFICATION', 'LOGGING', 'MANAGER', 'ALARM', 'LOGPROXY'))
ENDTABLE


// Entries in the MasterComponent table are the software master components for a particular software subsystem.
TABLE MasterComponent
     MasterComponentId       INTEGER                     NOT NULL
     ComponentId             INTEGER                     NOT NULL
     SubsystemName           LONGNAME                    NOT NULL
     KEY MasterComponentId GENERATED FROM ComponentId
     CONSTRAINT MComponentId FOREIGN KEY (ComponentId) REFERENCES Component
ENDTABLE


// The NetworkDeviceSnmpConfig table adds SNMP related information to records of the NetworkDevice table.
// (The reason for splitting this up into two tables is just to keep the core tables clear of SNMP stuff.)
// 
// NetworkDeviceId          Tied one-to-one to the NetworkDevice table ID
// SnmpXmlClob              This XML contains the entire "List of SNMP items" applicable to this network device. 
//                          Note that flags (PropagateNA, AcsAlarm) that are meant per monitoring "item" must be included in the XML!
//                          TODO: Support Oracle type "XMLType" instead of CLOB, which is true as well for XML CLOBs in other TMCDB tables.
// SnmpCommunity            Used for security.
// Netgroup                 Flattened tree of "snmp group names" for GUI display (slash-separated node names).
// PropagateNA              Flag indicating if N/A should be migrated upward 
//                          (e.g. whether a disk that suddenly becomes unresponsive should show also the computer in state NOTAVAILABLE.)
//  AcsAlarm                'NEVER' (default): Never raise an ACS alarm for an SNMP fault from this network device;
//                          'ALLOWSUPPRESSION': Normally raise an ACS alarm, but allow the snmp trap processing system to suppress that alarm 
//                                              when running in some special test mode (currently not implemented).
//                          'ALWAYS': Always raise an ACS alarm for a fault from this device
// 
TABLE NetworkDeviceSnmpConfig
    NetworkDeviceId         INTEGER                 NOT NULL
    SnmpXmlClob             XMLCLOB                 NOT NULL
    PropagateNA             BOOLEAN                 DEFAULT FALSE
    AcsAlarm                LONGVARCHAR (16)        DEFAULT 'NEVER'
    SnmpCommunity           LONGNAME                NULL
    Netgroup                LONGNAME                NULL
    KEY NetworkDeviceId
    CONSTRAINT NetDevSnmpConfigNetDev FOREIGN KEY (NetworkDeviceId) REFERENCES NetworkDevice
    CONSTRAINT NetDevSnmpConfigAcsAlarm CHECK (AcsAlarm IN ('NEVER', 'ALWAYS', 'ALLOWSUPPRESSION'))
ENDTABLE


// The SnmpTrapSink table contains the host and port of the SNMP trap sink daemon
// which is supposed to receive SNMP traps from the various monitored network devices.
// 
// ConfigurationId          Reference to the Configuration for which we describe the single trap sink daemon.
//                          (We need the Configuration to ensure that there is only a single trap sink configured.)
// TrapSinkComputerId       Reference to the computer that hosts the trap sink daemon
// TrapPort                 Trap sink daemon port
// TrapSourcesNetworkMask   Mask for network addresses from where the trapsink daemon should accept traps
// SnmpTrapCommunity        Security string, generally different from NetworkDeviceSnmpConfig.SnmpCommunity
//
TABLE SnmpTrapSink
    ConfigurationId         INTEGER                 NOT NULL
    TrapSinkComputerId      INTEGER                 NOT NULL
    TrapPort                INTEGER                 NOT NULL
    TrapSourcesNetworkMask  LONGNAME                NOT NULL
    SnmpTrapCommunity       LONGNAME                NULL
    KEY ConfigurationId
//    CONSTRAINT SnmpTrapSinkConfig FOREIGN KEY (ConfigurationId) REFERENCES Computer(ConfigurationId)
// TODO: think about a constraint that "TrapSinkComputerId" must belong to the same Configuration as "ConfigurationId"
    CONSTRAINT SnmpTrapSinkConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration
    CONSTRAINT SnmpTrapSinkComputer FOREIGN KEY (TrapSinkComputerId) REFERENCES Computer
ENDTABLE


// Referenced by SNMP table PowerstripSocket
// Note that this table is not as empty as it looks here, because the NetworkDeviceId and PK/FK will be generated based on the "extends" clause.
TABLE NetworkPowerstrip EXTENDS NetworkDevice
ENDTABLE


// The PowerstripSocket table describes a single socket of a powerstrip network device.
// It links to the network device that gets its power from this socket, which can be useful 
// both to powercycle devices, but also to know which devices will fail if a power supply gets too hot etc.
// 
// PowerstripSocketId       Artificial ID
// NetworkPowerstripId      Reference to the powerstrip device which this socket is part of.
// SocketNumber             Number of this socket within the powerstrip
// PoweredNetworkDeviceId   Reference to the network device which gets its power from this socket.
//                          Note that a NetworkDevice can be modeled to get power from one or more sockets, 
//                          which accommodates servers with dual power supply.
//                          However we have no constraint in place that enforces that these 2 sockets come from the same NetworkPowerstrip.
//                          TODO: Maybe later we could model the constraint that PowerstripSocketId and NetworkPowerstripId must be different,
//                                because a powerstrip can never get the power from itself. Currently the application will need to take care of this.
// SocketName               An additional optional human readable name to refer to this socket.
TABLE PowerstripSocket
    PowerstripSocketId      INTEGER                 NOT NULL
    NetworkPowerstripId     INTEGER                 NOT NULL
    SocketNumber            INTEGER                 NOT NULL
    PoweredNetworkDeviceId  INTEGER                 NULL
    SocketName              LONGNAME                NULL
    KEY PowerstripSocketId GENERATED FROM NetworkPowerstripId SocketNumber
    CONSTRAINT PwrstripSockNetPowerstrip FOREIGN KEY (NetworkPowerstripId) REFERENCES NetworkPowerstrip
    CONSTRAINT PwrstripSockNetDevice FOREIGN KEY (PoweredNetworkDeviceId) REFERENCES NetworkDevice
ENDTABLE
