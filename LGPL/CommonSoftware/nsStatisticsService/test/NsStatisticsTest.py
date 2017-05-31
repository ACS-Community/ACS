#!/usr/bin/env python
# @(#) $Id: NsStatisticsTest.py,v 1.4 2015/01/23 16:51:58 pcolomer Exp $
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2002
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration)
# and Cosylab 2002, All rights reserved
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307  USA
#
#------------------------------------------------------------------------------
import getpass
import re
import sys
import os

class AcsTestLogChecker(object):

	REL_PATH_FILE_PID="./tmp/pid_test"

	def __init__(self, num_proc, prefix, customLog=False):
		self.num_proc = int(num_proc)
		self.prefix = prefix

		if customLog: return

		self.pid = self.read_pid()
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

	def read_custom_log_file(self, path):
		# Create path of tmp log file

		str = ""
		try:
			f = open(path, 'r')
			str = f.read()
			f.close()
		except:
			self.l("Unexpected error reading file %s: %s" % (path, sys.exc_info()[0]))
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
			self.l("FAIL - Pattern '%s' not found" % (pattern))
		return len(m)
	
def get_pattern_debug_stats_channel(channel_name):
	"""
	str =  "DEBUG\ \[nsStatistics\]\s+DEBUG STATISTICS OF NOTIFICATION CHANNEL " + channel_name + "\n"
	str += "(\s+Supplier names.+)\n"
	str += "(\s+Consumer names.+)\n"
	str += "(\s+Supplier admin names.+)\n"
	str += "(\s+Consumer admin names.+)"
	"""
	str =  "DEBUG\ \[nsStatistics\]\s+STATISTICS OF NOTIFICATION CHANNEL " + channel_name
	str += "\s+\[\s+"
	str += "("
	str += "Num suppliers\=\'\d+\'\s+|"
	str += "Num consumers\=\'\d+\'\s+|"
	str += "Num slowest consumers\=\'\d+\'\s+|"
	str += "Num suppliers admin\=\'\d+\'\s+|"
	str += "Num consumers admin\=\'\d+\'\s+|"
	str += "Current consumers=\'.*\'\s+|"
	str += "Current suppliers=\'.*\'\s+|"
	str += "Current slowest consumers=\'.*\'\s+|"
	str += "Current consumers admin=\'.*\'\s+|"
	str += "Current suppliers admin=\'.*\'\s+|"
	str += "All consumers=\'.*\'\s+|"
	str += "All suppliers=\'.*\'\s+|"
	str += "All slowest consumers=\'.*\'\s+|"
	str += "All consumers admin=\'.*\'\s+|"
	str += "All suppliers admin=\'.*\'\s+|"
	str += "){5,15}"
	str += "\]"
	
	return str	
	
def get_pattern_stats_channel(channel_name,tqs=False,toe=False):
	str =  "INFO\ \[nsStatistics\]\s+STATISTICS OF NOTIFICATION CHANNEL " + channel_name
	str += "\s+\[\s+"
	str += "("
	str += "Num consumers=\'MIN=\d+, MAX=\d+, AVG=\d+(\.\d+)?\'\s+|"
	str += "Avg events in queues=\'(\d+(\.\d+)?,\s)*\'\s+|"
	str += "Max events in queues=\'(\d+,\s)*\'\s+|"
	str += "Num suppliers=\'MIN=\d+, MAX=\d+, AVG=\d+(\.\d+)?\'\s+|"
	#str += 	"Num suppliers\=\'\w+\'\s+|"
	#str += 	"Num consumers\=\'\w+\'\s+|"
	#str += 	"Num events in queues=\'(\d+(\,\s)?)*\'\s+|"
	if tqs:
		str += 	"Max size of queues \[bytes\]=\'.*\'\s+|"
		str += 	"Avg size of queues \[bytes\]=\'.*\'\s+|"
	if toe:
		str += 	"Oldest event=\'\d+\'\s+|"
	str +=	"Current slowest consumers=\'.*\'\s+|"
	str +=	"All slowest consumers=\'.*\'\s+"
	str += "){4,6}"
	str += "\]"
	
	return str
	
"""	
def get_pattern_stats_channel_with_tqs(channel_name):
	str = get_pattern_stats_channel(channel_name)
	str += "\n(\s+Size of queues in bytes\:(\s\d+\,?)*)"
	return str
	
def get_pattern_stats_channel_with_toe(channel_name):
	str = get_pattern_stats_channel(channel_name)
	str += "\n(\s+Oldest event\: \w*)"
	return str	
"""

def get_pattern_stats_factory(factory_name):
	"""
	str =  "INFO\ \[nsStatistics\]\s+STATISTICS OF NOTIFICATION FACTORY " + factory_name + "\n"
	str += "(\s+Active event channels \[\d+\])\n"
	str += "((\s+.+)\n)*"
	"""
	str =  "INFO\ \[nsStatistics\]\s+STATISTICS OF NOTIFICATION FACTORY " + factory_name
	str += "\s+\[\s+"
	str += "("
	str += "Active event channels\=\'.*\'\s+|"
	str += "Num active event channels\=\'\d+\'\s+"
	str += "){1,2}"
	str += "\]"
	return str

def get_pattern_warn_freq():
	str = "WARNING \[nsStatistics\] Statistics of Notification Services will"
	str += " be obtained with a very high frequency \(less than 1 minute time interval\)"
	return str
	
def test_case1():
	min_stats = 1
	max_stats = 2
	test = AcsTestLogChecker(2,"Test1")
	
	test.check_pattern_not_exists(get_pattern_warn_freq())

	# Check factories
	n = test.check_pattern_n_times_in_range(get_pattern_stats_factory("Alarm"), min_stats, max_stats)
	min_val = 1 if n == 1 else (n - 1)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Archive"), min_val, n + 1)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Logging"), min_val, n + 1)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("DefaultNotifyService"), min_val, n + 1)			
			
	# Check channels	
	test.check_pattern_n_times_in_range(get_pattern_stats_channel("Logging#LoggingChannel"), min_val, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("Logging#LoggingChannel"), min_val, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_channel("Archive#ArchivingChannel"), min_val, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("Archive#ArchivingChannel"), min_val, n)
	test.check_pattern_exists_at_least(get_pattern_stats_channel("DefaultNotifyService#testNsStatsChannel1", True, True), min_val) # min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_debug_stats_channel("DefaultNotifyService#testNsStatsChannel1"), min_val) # min_stats/2)
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Alarm")
		
	
def test_case2():
	min_stats = 3
	max_stats = 15
	test = AcsTestLogChecker(2,"Test2")

	test.check_pattern_exists(get_pattern_warn_freq())	
	
	# Check factories
	n = test.check_pattern_n_times_in_range(get_pattern_stats_factory("DefaultNotifyService"), min_stats, max_stats)
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION FACTORY Alarm")
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION FACTORY Archive")
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION FACTORY Logging")
	
	# Check channels
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Logging#LoggingChannel")
	test.check_pattern_not_exists("DEBUG [nsStatistics] STATISTICS OF NOTIFICATION CHANNEL Logging#LoggingChannel")
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Archive#ArchivingChannel")
	test.check_pattern_not_exists("DEBUG [nsStatistics] STATISTICS OF NOTIFICATION CHANNEL Archive#ArchivingChannel")
	test.check_pattern_exists_at_least(get_pattern_stats_channel("DefaultNotifyService#testNsStatsChannel1", True, True), min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_debug_stats_channel("DefaultNotifyService#testNsStatsChannel1"), min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_stats_channel("DefaultNotifyService#testNsStatsChannel2", True, True), min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_debug_stats_channel("DefaultNotifyService#testNsStatsChannel2"), min_stats/2)
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Alarm")	

def test_case3():	
	min_stats = 3
	max_stats = 25
	test = AcsTestLogChecker(2,"Test3")
	
	test.check_pattern_exists(get_pattern_warn_freq())
	
	# Check factories
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION FACTORY Alarm")
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION FACTORY Archive")
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION FACTORY Logging")
	n = test.check_pattern_n_times_in_range(get_pattern_stats_factory("DefaultNotifyService"), min_stats, max_stats)

	# Check channels
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Logging#LoggingChannel")
	test.check_pattern_not_exists("DEBUG [nsStatistics] STATISTICS OF NOTIFICATION CHANNEL Logging#LoggingChannel")
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Archive#ArchivingChannel")
	test.check_pattern_not_exists("DEBUG [nsStatistics] STATISTICS OF NOTIFICATION CHANNEL Archive#ArchivingChannel")
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL DefaultNotifyService#testNsStatsChannel1")
	test.check_pattern_not_exists("DEBUG [nsStatistics] STATISTICS OF NOTIFICATION CHANNEL DefaultNotifyService#testNsStatsChannel1")
	test.check_pattern_exists_at_least(get_pattern_stats_channel("DefaultNotifyService#testNsStatsChannel2",True,True), min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_debug_stats_channel("DefaultNotifyService#testNsStatsChannel2"), min_stats/2)	
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Alarm")	
	
def test_case4():
	min_stats = 4
	max_stats = 25
	test = AcsTestLogChecker(0,"Test4")
	
	test.check_pattern_exists(get_pattern_warn_freq())
	
	# Check factories
	n = test.check_pattern_n_times_in_range(get_pattern_stats_factory("Alarm"), min_stats, max_stats)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Archive"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Logging"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("DefaultNotifyService"), n - 1, n)
	
	# Check channels	
	test.check_pattern_n_times_in_range(get_pattern_stats_channel("LoggingChannel",toe=True), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("LoggingChannel"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_channel("ArchivingChannel",toe=True), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("ArchivingChannel"), n - 1, n)
	test.check_pattern_exists_at_least(get_pattern_stats_channel("testNsStatsChannel1",toe=True), min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_debug_stats_channel("testNsStatsChannel1"), min_stats/2)
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Alarm")
	

def test_case5():	
	min_stats = 4
	max_stats = 25
	test = AcsTestLogChecker(0,"Test5")
	
	test.check_pattern_exists(get_pattern_warn_freq())
	
	# Check factories
	n = test.check_pattern_n_times_in_range(get_pattern_stats_factory("Alarm"), min_stats, max_stats)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Archive"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Logging"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("DefaultNotifyService"), n - 1, n)
	
	# Check channels	
	test.check_pattern_n_times_in_range(get_pattern_stats_channel("LoggingChannel",tqs=True), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("LoggingChannel"), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_channel("ArchivingChannel",tqs=True), n - 1, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("ArchivingChannel"), n - 1, n)
	test.check_pattern_exists_at_least(get_pattern_stats_channel("testNsStatsChannel1",tqs=True), min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_debug_stats_channel("testNsStatsChannel1"), min_stats/2)
	test.check_pattern_not_exists("STATISTICS OF NOTIFICATION CHANNEL Alarm")
	
def test_case6():
	min_stats = 1
	max_stats = 2
	test = AcsTestLogChecker(6, "Test6", True)

	test.log_file = test.read_custom_log_file(os.path.join(os.environ.get("ACS_TMP"), "NsStatisticsComponentTest_frodoContainer.log"))

	test.check_pattern_not_exists(get_pattern_warn_freq())

	# Check factories
	n = test.check_pattern_n_times_in_range(get_pattern_stats_factory("Alarm"), min_stats, max_stats)
	min_val = 1 if n == 1 else (n - 1)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Archive"), min_val, n + 1)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("Logging"), min_val, n + 1)
	test.check_pattern_n_times_in_range(get_pattern_stats_factory("DefaultNotifyService"), min_val, n + 1)			
			
	# Check channels	
	test.check_pattern_n_times_in_range(get_pattern_stats_channel("Logging#LoggingChannel"), min_val, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("Logging#LoggingChannel"), min_val, n)
	test.check_pattern_n_times_in_range(get_pattern_stats_channel("Archive#ArchivingChannel"), min_val, n)
	test.check_pattern_n_times_in_range(get_pattern_debug_stats_channel("Archive#ArchivingChannel"), min_val, n)
	test.check_pattern_exists_at_least(get_pattern_stats_channel("DefaultNotifyService#testNsStatsChannel1", True, True), min_val) # min_stats/2)
	test.check_pattern_exists_at_least(get_pattern_debug_stats_channel("DefaultNotifyService#testNsStatsChannel1"), min_val) # min_stats/2)
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
	elif test_case == 6:
		test_case6()	
	else:
		raise BaseException("Unknown test case")

if __name__ == "__main__":
	main(sys.argv[1:])
