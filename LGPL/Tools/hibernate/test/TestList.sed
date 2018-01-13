s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/----------T--:--:--.---/g
s/Time: [0-9,.]*/Time: xxx/g
s/timestamp: [0-9]*/timestamp: xxxxxxxxxxxx/g
s/registered: [0-9,a-f,\-]*/registered: xxxxxxxxxxxxx/g
s/Instantiating session factory with properties:.*/Instantiating session factory with properties: XXXXXXXX/g
s/Registering SessionFactory:.*/Registering SessionFactory: UUID_here/g
s/@[0-9a-f]*/@xxxxxxx/g
