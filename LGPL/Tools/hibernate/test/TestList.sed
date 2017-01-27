s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/----------T--:--:--.---/g
s/Time: [0-9,.]*/Time: xxx/g
s/timestamp: [0-9]*/timestamp: xxxxxxxxxxxx/g
s/registered: [0-9,a-f,\-]*/registered: xxxxxxxxxxxxx/g
s/JDBCResultSet@[0-9,a-f]*/JDBCResultSet@xxxxxxx/g
s/Type@[0-9,a-f]*/Type@xxxxxxx/g
s/Impl@[0-9,a-f]*/Impl@xxxxxxx/g
s/Instantiating session factory with properties:.*/Instantiating session factory with properties: XXXXXXXX/g
s/Registering SessionFactory:.*/Registering SessionFactory: UUID_here/g
