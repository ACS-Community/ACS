import getpass
import re
import sys

class AcsTestLogChecker:

	REL_PATH_FILE_PID="./tmp/pid_test"

	def __init__(self, num_proc, prefix):
		self.pid = self.read_pid()
		self.num_proc = int(num_proc)
		self.prefix = prefix
		self.user_name = getpass.getuser() # User name of the user executing the test
		self.log_file = self.read_log_file() 
		
	def set_num_proc(self,num_proc):
		self.num_proc = num_proc
				
	def l(self,str):
		if not self.prefix:
			print str
		else:
			print "%s %s" % (self.prefix,str)
	
	def read_pid(self):
		pid = ""
		try:
			f = open(self.REL_PATH_FILE_PID, 'r')
			pid = f.read()
			f.close()
		except:
			self.l("Unexpected error reading pid file %s: %s" % (self.REL_PATH_FILE_PID,sys.exc_info()[0]))
			raise
		return pid
		
	def read_log_file(self):
		# Create path of tmp log file
		tmp_log = "/tmp/%s_test%s.%d"%(self.user_name,self.pid,self.num_proc)
		
		str = ""
		try:
			f = open(tmp_log, 'r')
			str = f.read()
			f.close()
		except:
			self.l("Unexpected error reading file %s: %s" % (tmp_log,sys.exc_info()[0]))
			raise
			
		return str	

	def find_pattern(self,pattern):
		p = re.compile(pattern)
		return p.findall(self.log_file)

	def check_pattern_n_times_in_range(self,pattern,min_occ,max_occ):
		m = self.find_pattern(pattern)
		if m is not None and min_occ <= len(m) and len(m) <= max_occ:
			self.l("OK - Number of instances of pattern '%s' is between the required range" % (pattern))
		else:
			self.l("FAIL - Found %d instances of pattern '%s' but range is [%d, %d]" % (len(m), pattern, min_occ, max_occ))
		return len(m)
		
	def check_pattern_exists_at_least(self,pattern,min_occ):
		m = self.find_pattern(pattern)
		if min_occ <= len(m):
			self.l("OK - Number of instances of pattern '%s' is equal or greater than the minimum required" % (pattern))
		else:
			self.l("FAIL - Found %d instances of pattern '%s' but the minimum is %d" % (len(m), pattern, min_occ))
		return len(m)		
			
	def check_pattern_not_exists(self,pattern):
		m = self.find_pattern(pattern)
		if len(m) <= 0:
			self.l("OK - Pattern '%s' not found" % (pattern))
		else:
			self.l("FAIL - Pattern '%s' found %d times" % (pattern, len(m)))
		return len(m)
		
	def check_pattern_exists(self,pattern):
		m = self.find_pattern(pattern)
		if len(m) > 0:
			self.l("OK - Pattern '%s' found" % (pattern))
		else:
			self.l("FAIL - Pattern '%s' not found" % (pattern, len(m)))
		return len(m)
	
def get_pattern_debug_stats_channel(channel_name):
	str =  "DEBUG\ \[eventGUI\]\s+DEBUG STATISTICS OF NOTIFICATION CHANNEL " + channel_name + "\n"
	str += "(\s+Supplier names.+)\n"
	str += "(\s+Consumer names.+)\n"
	str += "(\s+Supplier admin names.+)\n"
	str += "(\s+Consumer admin names.+)"
	return str	
	
def get_pattern_stats_channel(channel_name):
	str =  "INFO\ \[eventGUI\]\s+STATISTICS OF NOTIFICATION CHANNEL " + channel_name + "\n"
	str += "(\s+There are \d+ suppliers\, \d+ consumers)\n"
	str += "(\s+Number of events in queues\:(\s\d+\,?)*)"
	return str
	
def get_pattern_stats_channel_with_tqs(channel_name):
	str = get_pattern_stats_channel(channel_name)
	str += "\n(\s+Size of queues in bytes\:(\s\d+\,?)*)"
	return str
	
def get_pattern_stats_channel_with_toe(channel_name):
	str = get_pattern_stats_channel(channel_name)
	str += "\n(\s+Oldest event\: \w*)"
	return str	

def get_pattern_stats_factory(factory_name):
	str =  "INFO\ \[eventGUI\]\s+STATISTICS OF NOTIFICATION FACTORY " + factory_name + "\n"
	str += "(\s+Active event channels \[\d+\])\n"
	str += "((\s+.+)\n)*"
	return str
	
def test_case1():
	min_stats = 10
	max_stats = 20
	test = AcsTestLogChecker(0,"Test1")
	n = test.check_pattern_n_times_in_range("Getting Notify Services Statistics after 500ms", min_stats, max_stats)
			
	# Check factories
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Alarm"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Archive"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Logging"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("DefaultNotifyService"), n - 1, n)

	# Check channels	
	test.check_pattern_n_times_in_range(get_pattern_stats_channel("LoggingChannel"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("LoggingChannel"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_channel("ArchivingChannel"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("ArchivingChannel"), n - 1, n)
	test.check_pattern_exists_at_least(get_pattern_stats_channel("testNsStatsChannel1"), min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_debug_stats_channel("testNsStatsChannel1"), min_stats/2)
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Alarm")
		
	
def test_case2():
	min_stats = 5
	max_stats = 10
	test = AcsTestLogChecker(0,"Test2")
	n = test.check_pattern_n_times_in_range("Getting Notify Services Statistics after 1000ms", min_stats, max_stats)
	
	# Check factories
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION FACTORY Alarm")
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION FACTORY Archive")
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION FACTORY Logging")
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("DefaultNotifyService"), n - 1, n)
	
	# Check channels
	test.check_pattern_not_exists("\tSTATISTICS OF NOTIFICATION CHANNEL LoggingChannel")
	test.check_pattern_not_exists("DEBUG STATISTICS OF NOTIFICATION CHANNEL LoggingChannel")
	test.check_pattern_not_exists("\tSTATISTICS OF NOTIFICATION CHANNEL ArchivingChannel")
	test.check_pattern_not_exists("DEBUG STATISTICS OF NOTIFICATION CHANNEL ArchivingChannel")
	test.check_pattern_exists_at_least(get_pattern_stats_channel("testNsStatsChannel1"), min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_debug_stats_channel("testNsStatsChannel1"), min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_stats_channel("testNsStatsChannel2"), min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_debug_stats_channel("testNsStatsChannel2"), min_stats/2)
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Alarm")	

def test_case3():	
	min_stats = 10
	max_stats = 20
	test = AcsTestLogChecker(0,"Test3")
	n = test.check_pattern_n_times_in_range("Getting Notify Services Statistics after 500ms", min_stats, max_stats)
	
	# Check factories
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION FACTORY Alarm")
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION FACTORY Archive")
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION FACTORY Logging")
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("DefaultNotifyService"), n - 1, n)

	# Check channels
	test.check_pattern_not_exists("\tSTATISTICS OF NOTIFICATION CHANNEL LoggingChannel")
	test.check_pattern_not_exists("DEBUG STATISTICS OF NOTIFICATION CHANNEL LoggingChannel")
	test.check_pattern_not_exists("\tSTATISTICS OF NOTIFICATION CHANNEL ArchivingChannel")
	test.check_pattern_not_exists("DEBUG STATISTICS OF NOTIFICATION CHANNEL ArchivingChannel")
	test.check_pattern_not_exists("\tSTATISTICS OF NOTIFICATION CHANNEL testNsStatsChannel1")
	test.check_pattern_not_exists("DEBUG STATISTICS OF NOTIFICATION CHANNEL testNsStatsChannel1")
	test.check_pattern_exists_at_least(get_pattern_stats_channel("testNsStatsChannel2"), min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_debug_stats_channel("testNsStatsChannel2"), min_stats/2)	
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Alarm")	
	
def test_case4():
	min_stats = 10
	max_stats = 20
	test = AcsTestLogChecker(0,"Test4")
	n = test.check_pattern_n_times_in_range("Getting Notify Services Statistics after 500ms", min_stats, max_stats)
	
	# Check factories
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Alarm"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Archive"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Logging"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("DefaultNotifyService"), n - 1, n)
	
	# Check channels	
	test.check_pattern_n_times_in_range(get_pattern_stats_channel_with_toe("LoggingChannel"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("LoggingChannel"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_channel_with_toe("ArchivingChannel"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("ArchivingChannel"), n - 1, n)
	test.check_pattern_exists_at_least(get_pattern_stats_channel_with_toe("testNsStatsChannel1"), min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_debug_stats_channel("testNsStatsChannel1"), min_stats/2)
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Alarm")
	

def test_case5():	
	min_stats = 10
	max_stats = 20
	test = AcsTestLogChecker(0,"Test5")
	n = test.check_pattern_n_times_in_range("Getting Notify Services Statistics after 500ms", min_stats, max_stats)
	
	# Check factories
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Alarm"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Archive"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Logging"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("DefaultNotifyService"), n - 1, n)
	
	# Check channels	
	test.check_pattern_n_times_in_range(get_pattern_stats_channel_with_tqs("LoggingChannel"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("LoggingChannel"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_channel_with_tqs("ArchivingChannel"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("ArchivingChannel"), n - 1, n)
	test.check_pattern_exists_at_least(get_pattern_stats_channel_with_tqs("testNsStatsChannel1"), 5)
	test.check_pattern_exists_at_least(get_pattern_debug_stats_channel("testNsStatsChannel1"), 5)
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Alarm")
	
	
def main(argv):
	test_case = int(argv[0])
	if test_case == 1:
		test_case1()
	elif test_case == 2:
		test_case2()
	elif test_case == 3:
		test_case3()
	elif test_case == 4:
		test_case4()
	elif test_case == 5:
		test_case5()	
	else:
		raise BaseException("Unknown test case")

if __name__ == "__main__":
	main(sys.argv[1:])