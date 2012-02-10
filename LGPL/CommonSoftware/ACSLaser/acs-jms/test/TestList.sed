s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/\(0x[a-z,A-Z,0-9]*\)/xxxxxxx/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]/----------T--:--/g
s/-D user\.dir=[^ ]*/-D user\.dir=xxx/g
s/ChannelId='[0-9]*'/ChannelId='xxx'/g
s/TimeMillis='[0-9]*'/TimeMillis='xxx'/g
s/ConsumerProxyName='alma.acsjms.test.PubSubTest-[0-9]*'/ConsumerProxyName='alma.acsjms.test.PubSubTest-xxx'/g
