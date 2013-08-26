s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/----------T--:--:--.---/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]/---------- --:--:--/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]/---------- --:--/g
s/IOR:[a-z,0-9,.,]*/IOR:xxxxxxxxxxxxxx/g
s/iioploc:\/\/[a-z,0-9,.,]*:[0-9]*/iioploc:\/\/xxxxx\.xxxxx:xxxx/g
s/Handle: [0-9]*/Handle: xxxxxxxxx/g
s/ProcessID=\"[0-9]*\"/ProcessID=xxxxxxxxx/g
s/ThreadID=\"[0-9]*\"/ProcessID=xxxxxxxxx/g
s/v[0-9]*\.[0-9]*\.[0-9]*.* built at [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]/v-.-.- built at ---------- --:--:--/g
s/([0-9]*|[0-9]*) EXCEPTION/ (xxxxx|xxxx) EXCEPTION/g
s/enumpropTestServer_[0-9]*/enumpropTestServer_XXXX/g
s/corbaloc::[A-Z,a-z,0-9,:,\/,_,.,-]*/corbaloc::XXXX:xxxx/g
s/dat_onchangeMonitorTest_[0-9]*/dat_onchangeMonitorTest_XXXX/g
s/dat_testClient_[0-9]*/dat_testClient_XXXX/g
s/dat_testRWClient_[0-9]*/dat_testRWClient_XXXX/g
s/Recovery filename: '.*'/Recovery filename: '----'/g
s/Cache saved to '.*'/Cache saved to '----'/g
s/log_cache\.dat_[0-9,a-z,A-Z]*_[0-9]*/log_cache.dat__XXXXXXXX_XXX/g
