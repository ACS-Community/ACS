s/-OAIAddr [0-9]*.[0-9]*.[0-9]*.[0-9]*/-OAIAddr a.b.c.d/g
s/-OAport [0-9][0-9][0-9][0-9]/-OAport nnnn/g
s/-Djava.endorsed.dirs=.*/-Djava.endorsed.dirs=xxxxx/g
s/....-..-..T..:..:....../yyyy-mm-ddTHH:mm:ss.mmm/g
s/Cache saved to.*/Cache saved to.../g
s/.*org.jacorb.naming.NameServer .*/org.jacorb.naming.NameServer.../g
s/Name Service without channel entries in the endpoint [0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9][0-9][0-9][0-9]/Name Service without channel entries in the endpoint XX.XX.XX.XX:PP/g
s/Channel@ARCHIVING\[[0-9\.]*:[0-9]*\] deleted/Channel@ARCHIVING[XZ.XX.XX.XX:PP] deleted/g
s/Channel@LOGGING\[[0-9\.]*:[0-9]*\] deleted/Channel@LOGGING[XZ.XX.XX.XX:PP] deleted/g
