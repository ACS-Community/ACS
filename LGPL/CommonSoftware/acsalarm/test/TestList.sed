s/-OAIAddr [0-9]*.[0-9]*.[0-9]*.[0-9]*/-OAIAddr a.b.c.d/g
s/-OAport [0-9][0-9][0-9][0-9]/-OAport nnnn/g
s/-Djava.endorsed.dirs=.*/-Djava.endorsed.dirs=xxxxx/g
s/....-..-..T..:..:....../yyyy-mm-ddTHH:mm:ss.mmm/g
s/Cache saved to.*/Cache saved to.../g
s/.*org.jacorb.naming.NameServer .*/org.jacorb.naming.NameServer.../g
s/There are no channels in the endpoint [0-9]*.[0-9]*.[0-9]*.[0-9]*/There are no channels in the endpoint a.b.c.d/g
