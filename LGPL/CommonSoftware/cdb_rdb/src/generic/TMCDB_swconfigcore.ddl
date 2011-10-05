TMCDB SQL TABLE DEFINITIONS     VERSION 2.2.1  2010-08-22T0000:00:00.0

NOTE
"
============================================
|  CORE SW CONFIGURATION TABLES FOR TMCDB  |
============================================

This file contains the definition of the following tables for the TMCDB:
 ComponentType
 Configuration
 Schemas
 NetworkDevice
 Computer
 LoggingConfig
 NamedLoggerConfig
 Manager
 Container
 ContainerStartupOption
 Component
 BACIProperty
 Location
 Contact
 AlarmCategory
 FaultFamily
 AlarmCategoryFamily
 FaultMember
 DefaultMember
 FaultCode
 AlarmDefinition
 ReductionLink
 ReductionThreshold
 EventChannel
 Event
 NotificationServiceMapping
 DomainsMapping
 ChannelMapping
"
ENDNOTE

MODELNAME SwCore

// The ComponentType  characterizes a "type" of component.
// For example, the DataCapture component is dynamically created by Control
// whenever a scheduling block is executed.
// The ComponentType table can be used to describe this kind of component.
TABLE ComponentType
     ComponentTypeId         INTEGER                     NOT NULL
     IDL                     LONGNAME                    NOT NULL
     KEY ComponentTypeId     GENERATED FROM IDL
ENDTABLE

// The Configuration table is a major partitioning of the database.  The TMCDB is capable of supporting
// multiple, independent configurations.  This is especially useful for simulations, for example; but it
// also implies that TMCDB databases can be combined or split apart.
// If a Configuration is kept in the database only for historical reasons, but it doesn't reflect
// any hardware or simulation deployment, then its Active flag should be false.
TABLE Configuration
    ConfigurationId         INTEGER                 NOT NULL
    ConfigurationName       NAME                    NOT NULL
    FullName                LONGNAME                NOT NULL
    Active                  BOOLEAN                 NOT NULL
    CreationTime            TSTAMP                  NOT NULL
    Description             TEXT                    NOT NULL
    KEY ConfigurationId GENERATED FROM ConfigurationName
ENDTABLE


// This table stores the system XML Schemas. The ComponentType table
// references the URN column. This table also stores the XML Schemas
// not directly associated with Component types.
//
// Note that schemas depend on the Configuration, because Hardware ICDs change from one release to the other,
// and the TMCDB needs to support several releases at the same time (via different Configurations).
TABLE Schemas
    SchemaId                INTEGER                 NOT NULL
    URN                     LONGVARCHAR (512)       NOT NULL
    ConfigurationId         INTEGER                 NOT NULL
    Schema                  XMLCLOB                 NULL
    KEY SchemaId GENERATED FROM URN ConfigurationId
    CONSTRAINT SchemasConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration CASCADING INVERSE AGGREGATION
ENDTABLE


// The "NetworkDevice" is used as a super-type only for "Computer" in the core set of TMCDB tables, 
// but also as super-type for various SNMP-related tables (extended set of TMCDB tables).
// NetworkDeviceId      PK
// NetworkName          The unique network DNS kind of name, that can be translated to a network address.
// ConfigurationId      FK: References the Configuration.
// PhysicalLocation     Description where the network device is deployed.
// Name                 A descriptive name.
TABLE NetworkDevice
    NetworkDeviceId         INTEGER                 NOT NULL
    NetworkName             LONGNAME                NOT NULL
    ConfigurationId         INTEGER                 NOT NULL
    PhysicalLocation        LONGNAME                NULL
    Name                    LONGNAME                NULL
    KEY NetworkDeviceId GENERATED FROM NetworkName ConfigurationId
    CONSTRAINT NetworkDeviceConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration CASCADING INVERSE AGGREGATION
ENDTABLE

// The entries in this table are the computers that belong to the configuration.
// Included are the full host name, whether the computer is real-time or not, and whether it is a uni-processor or an SMP.
TABLE Computer EXTENDS NetworkDevice
    ProcessorType           CHAR (3)                NOT NULL
    RealTime                BOOLEAN                 NOT NULL
    Diskless                BOOLEAN                 NOT NULL
    CONSTRAINT ComputerProcessorType CHECK (ProcessorType IN ('uni', 'smp'))
ENDTABLE

// An entry in the LoggingConfig table describes the logging configuration for a process such as a container or the ACS manager
// which sends log records to a central log service.
// Log levels (thresholds) can be specified here as a default for all loggers that run inside this process,
// or can be refined by entries in the NamedLoggerConfig table.
// Attribute description:
// MinLogLevelDefault         All logs with priority lower than this value will be discarded and never sent to the logging system. 
//                            On a normally running system, priority is kept to INFO level (4) or higher to avoid flooding the logging system. 
//                            While debugging, it might be useful to increase the verbosity of the system by setting the priority to 0 or 2.
// MinLogLevelLocalDefault    Same as "minLogLevelDefault", but controlling the printing of the log to stdout independently of sending the log to the log service.
//                            Note that printing logs on the command line degrades performance much more than sending them to the log service.
//                            This value can be overridden by the env variable "ACS_LOG_STDOUT"
// CentralizedLogger          Name of the service representing the logging service. This is the name used to query the Manager for the reference to the logging service. 
//                            In the current installations the default value is normally used. The value can be changed to distribute logs to different instances of the service 
//                            in order to improve performance and scalability of the system. In the future it will be possible to federate instances of the logging service, 
//                            but this is not implemented yet.
// DispatchPacketSize         In order to improve performance and reduce network traffic, containers do not send immediately logs to the logging system. 
//                            This parameter specifies how many logs are packaged together and sent to the logging system in one call. 
//                            Note that the real package size may be smaller if sending off the records is also triggered by a timer and/or by the log level. 
//                            For debugging purposes it may be convenient to set the cache to 0, to avoid losing logs when a Container crashes. 
// ImmediateDispatchLevel     Normally a number of log records are sent together to the logging system, as described for "DispatchPacketSize". 
//                            The "ImmediateDispatchLevel" triggers sending all cached log records immediately once a record with the given (or higher) log level appears, 
//                            even before the specified packet size has been reached.
// FlushPeriodSeconds         If log records are queued locally in order to send a bunch of them together to the remote log service, we still may want to send 
//                            packages with fewer records after a certain time. This makes sure that log receivers see the messages in time, even if very few records get produced. 
//                            This value sets the time period in seconds after which the log record queue should be flushed if it contains log records, 
//                            regardless of the resulting 'dispatchPacketSize'.  A value "0" turns off the time-triggered flushing.
// MaxLogQueueSize            Log records are stored in a queue not only to send them in a packet over the wire (see dispatchPacketSize), but also to not lose any records 
//                            in times when the log service is not available (e.g. during container start, or any kind of network and service failure). 
//                            Thus they get stored in a queue, which gets drained once the log service becomes available. However, logging should not compete for memory with the functional parts of the software, so we limit this queue. 
//                            Values below "DispatchPacketSize"  will be ignored, as we first must queue the records that should be sent together.
// Constraint description:    
// MaxLogQueueSize 
TABLE LoggingConfig
    LoggingConfigId         INTEGER                     NOT NULL
    MinLogLevelDefault      TINYINT                     DEFAULT 2
    MinLogLevelLocalDefault TINYINT                     DEFAULT 2
    CentralizedLogger       LONGVARCHAR (16)            DEFAULT 'Log'
    DispatchPacketSize      TINYINT                     DEFAULT 10
    ImmediateDispatchLevel  TINYINT                     DEFAULT 10
    FlushPeriodSeconds      TINYINT                     DEFAULT 10
    MaxLogQueueSize         INTEGER                     DEFAULT 1000
    MaxLogsPerSecond		INTEGER						DEFAULT -1
    KEY LoggingConfigId GENERATED
ENDTABLE

// An entry in the NamedLoggerConfig table specifies the configuration of a particular logger in the process / application
// whose logging settings are configured in the referenced table LoggingConfig.
// Given the name of a logger, its log levels can be tweaked, overriding LoggingConfig.MinLogLevelDefault and LoggingConfig.MinLogLevelLocalDefault.
// The logger name can identify the logger used by the container, or the logger used by the ORB, or a logger used by a component that runs inside the container.
//
// Attribute description:
// TODO
// Constraint description:
TABLE NamedLoggerConfig
    NamedLoggerConfigId     INTEGER                     NOT NULL
    LoggingConfigId         INTEGER                     NOT NULL
    Name                    LONGVARCHAR (64)            NOT NULL
    MinLogLevel             TINYINT                 DEFAULT 2
    MinLogLevelLocal        TINYINT                 DEFAULT 2
    KEY NamedLoggerConfigId GENERATED FROM LoggingConfigId Name  
    CONSTRAINT NamedLoggerConfigLoggingConfig FOREIGN KEY (LoggingConfigId) REFERENCES LoggingConfig  CASCADING INVERSE COMPOSITION
ENDTABLE

// The entries in this table are the managers that belong to the configuration.
// LoggingConfigId  Link to the logging configuration for a container.
// ServerThreads    The number of threads allocated to the CORBA infrastructure for the handling of concurrent invocations.
// TODO attribute description
TABLE Manager
    ManagerId                 INTEGER                     NOT NULL
    ConfigurationId           INTEGER                     NOT NULL
    LoggingConfigId           INTEGER                     NOT NULL
    Startup                   LONGVARCHAR (256)           NULL
    ServiceComponents         LONGVARCHAR (256)           NULL
    ServiceDaemons            LONGVARCHAR (256)           NULL
    Timeout                   INTEGER                     DEFAULT 50
    ClientPingInterval        INTEGER                     DEFAULT 60
    AdministratorPingInterval INTEGER                     DEFAULT 45
    ContainerPingInterval     INTEGER                     DEFAULT 30
    ServerThreads             TINYINT                     DEFAULT 10
    KEY ManagerId GENERATED FROM ConfigurationId LoggingConfigId Startup ServiceComponents Timeout ClientPingInterval AdministratorPingInterval ContainerPingInterval ServerThreads 
    CONSTRAINT ManagerLoggingConfig FOREIGN KEY (LoggingConfigId) REFERENCES LoggingConfig CASCADING AGGREGATION
    CONSTRAINT ManagerConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration CASCADING INVERSE AGGREGATION
ENDTABLE

// The entries in this table are the containers that belong to the configuration.
// If this is a real-time container and kernel modules are to be loaded on startup, their directory location is specified in KernelModuleLocation.
// Note that the CDB fields "UseIFR" and "DALtype" are currently not mapped.
//
// ContainerId      UID.
// ContainerName    Simple name of the container. This name and the "Path" together must be unique.
// Path             Slash-separated name hierarchy, which logically belongs to the "ContainerName". 
//                  HSO TODO: The separation of container name and path seems not used in practice. Should we go back to just one field that can contain slashes, like the CDB has it?
// ConfigurationId  Reference to the Configuration.
// LoggingConfigId  Link to the logging configuration for a container.
// ImplLang         The programming language (PL) this container is written in (see constraint ContainerImplLang below).
//                  The container PL must match the component PL for all components configured to run in this container.
// KernelModule     Contains a comma-separated list of kernel module names to be loaded.
// ComputerId       Links to the computer on which this container is supposed to run.
//                  TODO: At the moment this link is not required. Should we leave it like this, interpreting the missing host data
//                        in the sense that the manager or OMC can find a default host (e.g. local host)?
//                        Making it required makes manually starting containers by neither ACS nor the OMC impossible, e.g. in unit tests.
// TypeModifiers    Optional space-separated list of type modifiers such as "debug pipeline-mode single-threaded".
// StartOnDemand    Determines whether the container is started on demand by the ACS manager (if true), 
//                  or manually by a user via cmd line or by the OMC (if false). See COMP-3476.
// KeepAliveTime    The time in seconds for which the container should not be shut down even when it no longer hosts any components. Negative values mean infinite time.
//                  Only applies when the container is started automatically by ACS.
// ServerThreads    The number of threads allocated to the CORBA infrastructure for the handling of concurrent invocations.
// ManagerRetry     How many times the Container shall try to contact the Manager upon startup before bailing out. 0 means forever.
// CallTimeout      Standard timeout in seconds for remote (CORBA) calls. Every call will timeout after this period of time, ensuring protection from deadlock. 
//                  Notice that ACS QoS features can be used to trim specific calls.
// PingInterval     The Manager pings periodically all containers to check if they are healthy. The time interval in seconds for this heartbeat check 
//                  can be specified here, to override Manager.ContainerPingInterval which is the default for all containers.
// Recovery         By default when a Container that crashes is restarted, the system tries to reload all the same Components that were active at the time of the crash. 
//                  This can lead to problems, for example, trying to debug a component that causes the crash of the container just at activation time. 
//                  Therefore it is possible to set this options to prevent reloading the components and getting into a deadlock situation.
// AutoloadSharedLibs Blank-separated list of shared libraries (C++ container only) that must be automatically loaded by the container.
//                  It does not seem necessary to break down storage further, e.g. using tables "SharedLibs" and "ContainerToAutoloadedSharedLibs".
//
TABLE Container
    ContainerId             INTEGER                 NOT NULL
    ContainerName           LONGNAME                NOT NULL
    Path                    LONGNAME                NOT NULL 
    ConfigurationId         INTEGER                 NOT NULL
    LoggingConfigId         INTEGER                 NOT NULL
    ImplLang                LONGVARCHAR (6)         NOT NULL
    RealTime                BOOLEAN                 DEFAULT FALSE
    RealTimeType            LONGVARCHAR (4)         DEFAULT 'NONE'
    KernelModuleLocation    TEXT                    NULL
    KernelModule            TEXT                    NULL
    ComputerId              INTEGER                 NULL
    TypeModifiers           LONGVARCHAR (64)        NULL
    StartOnDemand           BOOLEAN                 DEFAULT FALSE
    KeepAliveTime           INTEGER                 DEFAULT -1
    ServerThreads           INTEGER                 DEFAULT 5
    ManagerRetry            INTEGER                 DEFAULT 10
    CallTimeout             INTEGER                 DEFAULT 30
    PingInterval            INTEGER                 NULL
    Recovery                BOOLEAN                 DEFAULT TRUE
    AutoloadSharedLibs      LONGVARCHAR (1024)      NULL
    KEY ContainerId GENERATED FROM ContainerName Path ConfigurationId
    CONSTRAINT ContainerConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration CASCADING INVERSE AGGREGATION
    CONSTRAINT ContainerLoggingConfig FOREIGN KEY (LoggingConfigId) REFERENCES LoggingConfig CASCADING AGGREGATION
    CONSTRAINT ContainerComputer FOREIGN KEY (ComputerId) REFERENCES Computer CASCADING AGGREGATION
    CONSTRAINT ContainerImplLang CHECK (ImplLang IN ('java', 'cpp', 'py'))
    CONSTRAINT ContainerRealTimeType CHECK (RealTimeType IN ('NONE', 'ABM', 'CORR'))
ENDTABLE

// The entries in this table are configuration options to be used when starting the container.
// 
// ContStartOptId   Artificial ID
// ContainerId      Reference to the Container
// OptionType       An enum that defines the intended recipient of the option, which will determine the position and possible wrapping
//                  of the options when passing all container options to the container daemon API or the command line.
//                  'ENV_VAR': An option to be appended to an environment variable before running any scripts or application.
//                             Note that setting env vars here does not affect the container daemon itself, but the container that gets started.
//                             As of ACS 8.x this feature is not yet implemented, also not in the OMC.
//                  'EXEC_ARG': An option targeted at the language-neutral executable (e.g. container daemon or 'acsStartContainer' start script).
//                  'EXEC_ARG_LANG': An option targeted at the underlying language-specific executable 
//                                   (e.g. acsStartJavaContainer and acsStartJava in case of a java container).
//                                   This corresponds to an option that can be given to acsStartContainer inside the wrapper option
//                                   "--passthroughProcessStart", for example --passthroughProcessStart="-clientVM".
//                  'CONT_ARG': An option targeted at the container application (passed to the container's main method).
//                              This corresponds to a container flag that can be given to acsStartContainer inside the wrapper option
//                              "-p CONTAINER_FLAGS" (or "--passthrough=CONTAINER_FLAGS").
// OptionName       For 'EXEC_ARG', 'EXEC_ARG_LANG', and 'CONT_ARG': A name that is intended for humans to better view/edit/query the option.
//                              The option name that actually gets passed on must still be included in attribute OptionValue,
//                              because there are too many different syntaxes in use (name value, name=value, namevalue, -Dname=value etc)
//                              ACS will provide a java helper class that delivers possible container option names to GUI applications.
//                  For 'ENV_VAR': Name of the environment variable to which this option will be appended.
// OptionValue      The option itself, using the syntax appropriate for the intended script or application.
TABLE ContainerStartupOption
    ContStartOptId          INTEGER                 NOT NULL
    ContainerId             INTEGER                 NOT NULL
    OptionType              LONGVARCHAR (16)        NOT NULL
    OptionName              LONGNAME                NOT NULL
    OptionValue             LONGNAME                NOT NULL
    KEY ContStartOptId GENERATED
    CONSTRAINT ContStartOptContainer FOREIGN KEY (ContainerId) REFERENCES Container CASCADING INVERSE COMPOSITION
    CONSTRAINT ContStartOptType CHECK (OptionType IN ('ENV_VAR', 'EXEC_ARG', 'EXEC_ARG_LANG', 'CONT_ARG'))
ENDTABLE

// Entries in the Component table are the software components for the given configuration that run in containers.
// Attribute description:
// ContainerId Statically defined component instances link to the container they must be run in (Container.ContainerId). 
//             Otherwise -1 to indicate that the container will be assigned only at runtime. 
//             We do not use NULL because with JDBC it would be awkward to distinguish NULL from 0.
// ImplLang    the PL this component is written in (see constraint ComponentImplLang below).
//             This attribute be redundant with Container.Type for all static components and some dynamic components.
//             Other dynamic components only get a container assigned at runtime, in which case it is useful to keep the component PL language separately.
// IDL         The IDL type, such as "IDL:alma/MOUNT_ACS/Mount:1.0"
// Path        The optional hierarchical path, not including the component name itself. Empty string if the component name does not have a qualifying path.
//             This path was used in the old CDB for nesting component configurations, and it is not clear yet how much of it will be exposed in the TMCDB.
// IsAutostart If true, then the ACS manager is supposed to trigger the starting of this component, before another client may ask for it later.
// IsDefault   If true, this static component instance will be considered the default instance of its IDL type.
//             The value is ignored for partially configured dynamic components, for which it must be provided at runtime (@Todo: check if there are cases where it still makes sense for dyn comps)
// IsStandaloneDefined This attribute is only needed for the roundtrip from the old CDB to the TMCDB and back. If true, the component config info comes from a single xml file.
//             @Todo: perhaps use one attribute "classicCdbMapping" which could contain many such data items, e.g. in Java properties format.
// IsControl Is it a control device?
// KeepAliveTime The inertia for unloading components whose clients no longer need them. If negative, the component will never be unloaded automatically.
// MinLogLevel Optional log level for this component. Value -1 denotes NULL, since 0 is a valid setting.
// MinLogLevelLocal Optional stdout log level for for this component. Value -1 denotes NULL, since 0 is a valid setting.
TABLE Component
     ComponentId             INTEGER                 NOT NULL
     ComponentTypeId         INTEGER                 NOT NULL
     ComponentName           LONGNAME                NOT NULL
     ConfigurationId         INTEGER                 NOT NULL
     ContainerId             INTEGER                 NULL
     ImplLang                LONGVARCHAR (6)         NOT NULL
     RealTime                BOOLEAN                 NOT NULL
     Code                    LONGNAME                NOT NULL
     Path                    LONGNAME                NOT NULL
     IsAutostart             BOOLEAN                 NOT NULL
     IsDefault               BOOLEAN                 NOT NULL
     IsStandaloneDefined     BOOLEAN                 NULL
     IsControl               BOOLEAN                 NOT NULL
     KeepAliveTime           INTEGER                 NOT NULL
     MinLogLevel             TINYINT                 NOT NULL
     MinLogLevelLocal        TINYINT                 NOT NULL
     XMLDoc                  XMLCLOB                 NULL
     URN                     LONGVARCHAR (512)       NULL
     KEY ComponentId GENERATED FROM Path ComponentName ConfigurationId
     CONSTRAINT ComponentIDL FOREIGN KEY (ComponentTypeId) REFERENCES ComponentType 
     CONSTRAINT ComponentContainer FOREIGN KEY (ContainerId) REFERENCES Container 
     CONSTRAINT ComponentConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration CASCADING INVERSE AGGREGATION
     CONSTRAINT ComponentImplLang CHECK (ImplLang IN ('java', 'cpp', 'py'))
ENDTABLE

// Entries in the BACIProperty table describe configuration for a single baci property.
// BACIPropertyId                    Artificial ID
// ComponentId                       Reference to the component that owns this baci property
// PropertyName                      Name of the property as defined in the component's IDL interface
// description                       Description of the function and purpose of the Property.
// isSequence                        TODO: Check if this attribute can be removed. It is not needed by ACS, but perhaps by CONTROL? Always set to "false".
//                                         Usage at AOS on 2011-03-10: 0: 598844, 1: 64660
// format                            A printf-like formatting string, can be used by GUIs.
// units                             Name of unit which this property's value represents.
// resolution                        Bit pattern representing the significant bits in the property's value.
//                                   TODO: Why defined as a string instead of a 64 bit number? Right now the string can only hold a 32 bit number with 10 digits.
//                                         Usage at AOS on 2011-03-10: '0': 64448, '1': 597798, '10': 212, '65535': 1046
// archive_priority                  Seems not used any more (probably was used as log level before ACS used NC for archiving monitoring data)
//                                     1, 2, 3, 15 used at AOS
// archive_min_int                   The shortest interval at which the value of a property gets archived, even if the on-change setup would cause shorter intervals. 
//                                   (This interval should not be shorter than min_timer_trig).
// archive_max_int                   Sets a fixed time trigger, to archive the property value even if it does not change.
// archive_mechanism                 Determines which monitoring framework should be used for this property: 'notification_channel' or 'monitor_collector'
// archive_suppress                  Can be used to enable, disable archiving of the property w/o changing the other values of archiving like archive_max_int, and archive_min_int.
// default_timer_trig                Specifies how frequently the property value is sent to the callback method of a monitor of this property.
// min_timer_trig                    Sets the shortest interval at which a monitor gets notified (what archive_min_int is for archiving). 
// initialize_devio                  TODO copy descriptions from BACI.XSD
// min_delta_trig                    
// default_value           
// graph_min               
// graph_max               
// min_step                
// archive_delta           
// alarm_high_on           
// alarm_low_on            
// alarm_high_off          
// alarm_low_off           
// alarm_timer_trig        
// min_value               
// max_value               
// bitDescription          
// whenSet                           Color index 
// whenCleared             
// statesDescription       
// condition               
// alarm_on                
// alarm_off               
// alarm_fault_family      
// alarm_fault_member      
// alarm_level             
// Data                              XML string containing all non-standard attributes (e.g. when extending ROdouble property with new attributes).
TABLE BACIProperty
    BACIPropertyId          INTEGER                  NOT NULL
    ComponentId             INTEGER                  NOT NULL
    PropertyName            NAME                     NOT NULL
    description             TEXT                     NOT NULL
    format                  LONGVARCHAR (16)         NOT NULL
    units                   LONGVARCHAR (24)         NOT NULL
    resolution              LONGVARCHAR (10)         NOT NULL
    archive_priority        INTEGER                  NOT NULL
    archive_min_int         DOUBLE                   NOT NULL
    archive_max_int         DOUBLE                   NOT NULL
    archive_mechanism       LONGVARCHAR (24)         NOT NULL
    archive_suppress        BOOLEAN                  NOT NULL
    default_timer_trig      DOUBLE                   NOT NULL
    min_timer_trig          DOUBLE                   NOT NULL
    initialize_devio        BOOLEAN                  NOT NULL
    min_delta_trig          DOUBLE                   NULL
    default_value           TEXT                     NOT NULL
    graph_min               DOUBLE                   NULL
    graph_max               DOUBLE                   NULL
    min_step                DOUBLE                   NULL
    archive_delta           DOUBLE                   NOT NULL
    archive_delta_percent   DOUBLE                   NULL
    alarm_high_on           DOUBLE                   NULL
    alarm_low_on            DOUBLE                   NULL
    alarm_high_off          DOUBLE                   NULL
    alarm_low_off           DOUBLE                   NULL
    alarm_timer_trig        DOUBLE                   NULL
    min_value               DOUBLE                   NULL
    max_value               DOUBLE                   NULL
    bitDescription          TEXT                     NULL
    whenSet                 TEXT                     NULL
    whenCleared             TEXT                     NULL
    statesDescription       TEXT                     NULL
    condition               TEXT                     NULL
    alarm_on                TEXT                     NULL
    alarm_off               TEXT                     NULL
    alarm_fault_family      TEXT                     NULL
    alarm_fault_member      TEXT                     NULL
    alarm_level             INTEGER                  NULL
    Data                    TEXT                     NULL
    KEY BACIPropertyId GENERATED FROM PropertyName ComponentId
    CONSTRAINT BACIPropArchMech CHECK (archive_mechanism IN ('notification_channel', 'monitor_collector'))
    CONSTRAINT BACIPropertyCompId FOREIGN KEY (ComponentId) REFERENCES Component CASCADING INVERSE COMPOSITION
ENDTABLE


// The following tables contain the alarm definitions and configuration
TABLE Location
	LocationId			INTEGER		NOT NULL
	Building			LONGNAME	NULL
	Floor				NAME		NULL
	Room				LONGNAME	NULL
	Mnemonic			LONGNAME	NULL
	LocationPosition	LONGNAME 	NULL
	KEY LocationId GENERATED FROM Building Floor Room Mnemonic LocationPosition
ENDTABLE

TABLE Contact
	ContactId			INTEGER		NOT NULL
	ContactName			LONGNAME	NOT NULL
	Email				LONGNAME	NULL
	Gsm					LONGNAME	NULL
	KEY ContactId GENERATED FROM ContactName
ENDTABLE

TABLE AlarmCategory
	AlarmCategoryId		INTEGER		NOT NULL
	AlarmCategoryName	NAME		NOT NULL
	Description			TEXT		NOT NULL
	Path				LONGNAME	NOT NULL
	IsDefault			BOOLEAN		NOT NULL
	ConfigurationId		INTEGER		NOT NULL
	KEY AlarmCategoryId GENERATED FROM AlarmCategoryName ConfigurationId
	CONSTRAINT AlarmCategoryConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration CASCADING INVERSE AGGREGATION
ENDTABLE

TABLE FaultFamily
	FaultFamilyId	INTEGER		NOT NULL
	FamilyName		LONGNAME	NOT NULL
	AlarmSource		LONGNAME	DEFAULT 'ALARM_SYSTEM_SOURCES'
	HelpURL			LONGNAME	NULL
	ContactId		INTEGER		NOT NULL
	ConfigurationId	INTEGER		NOT NULL
	KEY FaultFamilyId GENERATED FROM FamilyName ConfigurationId
	CONSTRAINT FaultFamilyContact FOREIGN KEY (ContactId) REFERENCES Contact CASCADING AGGREGATION
	CONSTRAINT FaultFamilyConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration CASCADING INVERSE AGGREGATION
ENDTABLE

TABLE AlarmCategoryFamily
	AlarmCategoryId		INTEGER		NOT NULL
	FaultFamilyId		INTEGER		NOT NULL
	KEY AlarmCategoryId FaultFamilyId
	CONSTRAINT ACFCategoryId FOREIGN KEY (AlarmCategoryId) REFERENCES AlarmCategory CASCADING AGGREGATION
	CONSTRAINT ACFFamilyId FOREIGN KEY (FaultFamilyId) REFERENCES FaultFamily CASCADING AGGREGATION
ENDTABLE

TABLE FaultMember
	FaultMemberId	INTEGER		NOT NULL
	MemberName		LONGNAME	NOT NULL
	FaultFamilyId	INTEGER		NOT NULL
	LocationId		INTEGER		NULL
	KEY FaultMemberId GENERATED FROM MemberName FaultFamilyId
	CONSTRAINT FaultMemFamilyRef FOREIGN KEY (FaultFamilyId) REFERENCES FaultFamily CASCADING INVERSE AGGREGATION
	CONSTRAINT FaultMemLocationRef FOREIGN KEY (LocationId) REFERENCES Location CASCADING AGGREGATION
ENDTABLE

TABLE DefaultMember
	DefaultMemberId INTEGER     NOT NULL
	FaultFamilyId   INTEGER     NOT NULL
	LocationID      INTEGER     NULL
	KEY DefaultMemberId GENERATED FROM FaultFamilyId 
	CONSTRAINT DefaultMemberFaultFamilyRef FOREIGN KEY (FaultFamilyId) REFERENCES FaultFamily CASCADING INVERSE AGGREGATION
	CONSTRAINT DefaultMemberLocationRef FOREIGN KEY (LocationID) REFERENCES Location CASCADING AGGREGATION
ENDTABLE

TABLE FaultCode
	FaultCodeId			INTEGER		NOT NULL
	FaultFamilyId		INTEGER		NOT NULL
	CodeValue			INTEGER		NOT NULL
	Priority			INTEGER		NOT NULL
	Cause				LONGNAME	NULL
	Action				TEXT		NULL
	Consequence			TEXT		NULL
	ProblemDescription	TEXT		NOT NULL
	IsInstant			BOOLEAN		NOT NULL
	KEY FaultCodeId GENERATED FROM FaultFamilyId CodeValue
	CONSTRAINT CodeFaultFamilyRef FOREIGN KEY (FaultFamilyId) REFERENCES FaultFamily CASCADING INVERSE AGGREGATION
	CONSTRAINT PriorityValue CHECK (Priority IN (0,1,2,3))
ENDTABLE

// The entries in this table represent alarm triplets referenced by node reduction rules or multiplicity reduction rules.
// Note that since ACS 9.1 this AlarmDefinition table no longer references tables FaultMember and FaultCode 
// (and thus indirectly also table FaultFamily), but instead has its own fields for FF, FM, and FC.
// This is a loss of referential integrity in case of "straight" node reduction rules that connect two triplets,
// but it allows using wildcards for the FF, FM (or even FC), which became necessary with the refactoring of baci property alarms.
//
// AlarmDefinitionId    UID.
// FaultFamily		  The FF of the alarm triplet, regexp supported.
// FaultMember        The FM of the alarm triplet, regexp supported.
// FaultCode          The FC of the alarm triplet, regexp supported.
//
TABLE AlarmDefinition
	AlarmDefinitionId	INTEGER	NOT NULL
	ConfigurationId  INTEGER NOT NULL
	FaultFamily		LONGNAME	NOT NULL
	FaultMember		LONGNAME	NOT NULL
	FaultCode		LONGNAME	NOT NULL
	KEY AlarmDefinitionId	GENERATED FROM ConfigurationId FaultFamily FaultMember FaultCode
	CONSTRAINT AlarmDefinitionConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration CASCADING INVERSE AGGREGATION
ENDTABLE


// An entry in the ReductionLink table represents reduction rule (node or multiplicity),
// where the distinction is given through the Type attribute.
// Multiplicity reduction rule: 
//           If a sufficiently many "child" alarms are active, they are reduced to a single "parent" alarm.
//           The "parent" alarm is created by the alarm service.
// Node reduction rules: 
//           If both the "parent" and the "child" alarm are active, then the "child" alarm will be flagged as "reduced".
//           The "parent" alarm will not be created by the system though.
// ReductionLinkId    UID
// ParentAlarmDefId   AlarmDefinition for the "parent" alarm.
// ChildAlarmDefId    AlarmDefinition for the "child" alarm.
// Type               RR type: 'MULTIPLICITY' or 'NODE'
// Action             'CREATE', or 'REMOVE'. Currently we only use 'CREATE'. 
//                    @TODO: Describe the meaning of 'REMOVE' (should that RR be removed?)
// ConfigurationId    Ref to TMCDB Configuration.
//
TABLE ReductionLink
    ReductionLinkId     INTEGER             NOT NULL
    ParentAlarmDefId    INTEGER             NOT NULL
    ChildAlarmDefId     INTEGER             NOT NULL
    Type                LONGVARCHAR (12)    NOT NULL
    Action              LONGVARCHAR (6)     NOT NULL
    ConfigurationId     INTEGER             NOT NULL
    KEY ReductionLinkId	GENERATED FROM ParentAlarmDefId ChildAlarmDefId
    CONSTRAINT RLParentRef FOREIGN KEY (ParentAlarmDefId) REFERENCES AlarmDefinition CASCADING AGGREGATION
    CONSTRAINT RLChildRef FOREIGN KEY (ChildAlarmDefId) REFERENCES AlarmDefinition CASCADING AGGREGATION
    CONSTRAINT ReductionLinkType CHECK (Type IN ('MULTIPLICITY', 'NODE'))
    CONSTRAINT ReductionLinkAction CHECK (Action IN ('CREATE', 'REMOVE'))
    CONSTRAINT ReductionLinkConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration CASCADING INVERSE AGGREGATION
ENDTABLE


// An entry in the ReductionThreshold table refers to an AlarmDefinition that is the parent in a set of multiplicity reduction rules.
// Several child alarms that map to this parent alarm (ReductionLink entries) will be reduced to the parent alarm
// if more than the given "Value" of child alarms are active.
//
TABLE ReductionThreshold
    AlarmDefinitionId       INTEGER     NOT NULL
    Value                   INTEGER     NOT NULL
    ConfigurationId         INTEGER     NOT NULL
    KEY AlarmDefinitionId
    CONSTRAINT RTAlarmRef FOREIGN KEY (AlarmDefinitionId) REFERENCES AlarmDefinition CASCADING AGGREGATION
    CONSTRAINT RTConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration CASCADING INVERSE AGGREGATION
ENDTABLE


// Schema which describes an ACS event channel. At the moment, the only info included here are some
// Quality of Service and Administrative properties that are applicable to the type of notification channels ACS utilizes.
// All of the inline schema documentation found here is also available in the ACS notification channel tutorial or
// directly from OMG - http://www.omg.org/technology/documents/formal/notification_service.htm
TABLE EventChannel
	EventChannelId          INTEGER         NOT NULL
	ConfigurationId         INTEGER         NOT NULL
	Name                    LONGNAME        NOT NULL
	Path                    LONGNAME        NOT NULL
	IntegrationLogs			BOOLEAN			DEFAULT FALSE
	MaxQueueLength			INTEGER			DEFAULT 0
	MaxConsumers			INTEGER			DEFAULT 0
	MaxSuppliers			INTEGER			DEFAULT 0
	RejectNewEvents			BOOLEAN			DEFAULT TRUE
	DiscardPolicy			LONGVARCHAR(20)	DEFAULT 'AnyOrder'
	EventReliability		LONGVARCHAR(20)	DEFAULT 'BestEffort'
	ConnectionReliability	LONGVARCHAR(20)	DEFAULT 'BestEffort'
	Priority				SMALLINT		DEFAULT 0
	Timeout					INTEGER			DEFAULT 0
	OrderPolicy				LONGVARCHAR(20)	DEFAULT 'AnyOrder'
	StartTimeSupported		BOOLEAN			DEFAULT FALSE
	StopTimeSupported		BOOLEAN			DEFAULT FALSE
	MaxEventsPerConsumer	INTEGER			DEFAULT 0
	KEY EventChannelId GENERATED FROM Name Path ConfigurationId
	CONSTRAINT EventChannelConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration CASCADING INVERSE AGGREGATION
	CONSTRAINT EventChannelDiscardPolicy CHECK (DiscardPolicy IN ('AnyOrder', 'FifoOrder', 'LifoOrder', 'PriorityOrder', 'DeadlineOrder'))
	CONSTRAINT EventChannelOrderPolicy CHECK (OrderPolicy IN ('AnyOrder', 'FifoOrder', 'LifoOrder', 'PriorityOrder', 'DeadlineOrder'))
	CONSTRAINT EventChannelEventReliability CHECK (EventReliability IN ('BestEffort', 'Persistent'))
	CONSTRAINT EventChannelConReliability CHECK (ConnectionReliability IN ('BestEffort', 'Persistent'))
ENDTABLE

// Schema describing an individual event sent by some supplier on the channel.
// Does not contain much at the moment.
TABLE Event
    EventId                 INTEGER         NOT NULL
    EventChannelId          INTEGER         NOT NULL
    Name                    LONGNAME        NOT NULL
    MaxProcessTime          DOUBLE          DEFAULT '2.0'
    KEY EventId GENERATED FROM EventChannelId Name
    CONSTRAINT EventEventChannelRef FOREIGN KEY (EventChannelId) REFERENCES EventChannel CASCADING INVERSE AGGREGATION
ENDTABLE

// NotificationServiceMapping, together with the child tables DomainsMapping and ChannelMapping,
// allows to define for every NC the Notify Service that should host the NC.
// This corresponds to the CDB file MACI/Channels/Channels.xml whose schema is at 
// http://www.eso.org/projects/alma/develop/acs/OnlineDocs/ACS_docs/schemas/urn_schemas-cosylab-com_Channels_1.0/complexType/NotificationServiceMapping.html
TABLE NotificationServiceMapping
    NotificationServiceMappingId    INTEGER         NOT NULL
    ConfigurationId                 INTEGER         NOT NULL
    DefaultNotificationService      LONGNAME        NOT NULL
    KEY NotificationServiceMappingId GENERATED FROM ConfigurationId
    CONSTRAINT NotServMapConfig FOREIGN KEY (ConfigurationId) REFERENCES Configuration CASCADING INVERSE AGGREGATION
ENDTABLE

// "NC domain" mapping to a particular event service.
// See http://www.eso.org/projects/alma/develop/acs/OnlineDocs/ACS_docs/schemas/urn_schemas-cosylab-com_Channels_1.0/complexType/DomainsMappingElement.html
//
// Name                     The domain name, e.g. "ALARMSYSTEM"
// NotificationService      The notify service to be used for all NCs of this domain, e.g. "AlarmNotifyService".
//                          TODO: replace this with a reference to AcsService once Alma fully populates that table.
TABLE DomainsMapping
    DomainsMappingId                INTEGER         NOT NULL
    Name                            LONGNAME        NOT NULL
    NotificationService             LONGNAME        NOT NULL
    NotificationServiceMappingId    INTEGER         NOT NULL
    KEY DomainsMappingId GENERATED FROM NotificationServiceMappingId Name
    CONSTRAINT DomainsNotServMapRef FOREIGN KEY (NotificationServiceMappingId) REFERENCES NotificationServiceMapping CASCADING INVERSE AGGREGATION
ENDTABLE

// NC mapping to a particular event service.
// See http://www.eso.org/projects/alma/develop/acs/OnlineDocs/ACS_docs/schemas/urn_schemas-cosylab-com_Channels_1.0/complexType/ChannelMappingElement.html
// 
// Table design issue: One entry in this table can reference a single NC by name, or a group of NCs by name wildcards.
// If wildcards are used, we cannot reference the NC in table EventChannel, where it may not even be configured
// since NC QoS configuration is optional. Therefore we simply store the NC(s) by name in the "Name" attribute,
// without enforcing referential integrity.
// 
// Name                     The NC name (wildcards are allowed), e.g. "CONTROL_*"
// NotificationService      The notify service to be used for this NC (group), e.g. "ControlNotifyService".
//                          TODO: replace this with a reference to AcsService once Alma fully populates that table.
TABLE ChannelMapping
    ChannelMappingId                INTEGER         NOT NULL
    Name                            LONGNAME        NOT NULL
    NotificationService             LONGNAME        NOT NULL
    NotificationServiceMappingId    INTEGER         NOT NULL
    KEY ChannelMappingId GENERATED FROM NotificationServiceMappingId Name
    CONSTRAINT ChannelNotServMapRef FOREIGN KEY (NotificationServiceMappingId) REFERENCES NotificationServiceMapping CASCADING INVERSE AGGREGATION
ENDTABLE
