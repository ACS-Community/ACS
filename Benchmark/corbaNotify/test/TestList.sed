s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/----------T--:--:--.---/g
s/corbaloc::[a-z,A-Z,0-9,_,.,-]*:[0-9]*/corbaloc::xxxx:xxxx/g
s/-OAIAddr [0-9]*\.[0-9]*\.[0-9]*\.[0-9]*/-OAIAddr xxx.xxx.xxx.xxx/g
s/-OAport [0-9]*/-OAport xxx/g
s/ in [0-9]* ms/ in xxx ms/g
s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]*/--:--:--.---/g
s/Number of events received: 7 \[7\]/Number of events received: X [Y]/g
s/Number of events received: 8 \[8\]/Number of events received: X [Y]/g
