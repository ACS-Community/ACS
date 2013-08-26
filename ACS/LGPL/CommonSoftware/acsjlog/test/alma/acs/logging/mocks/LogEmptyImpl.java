package alma.acs.logging.mocks;

import org.omg.CORBA.Any;
import org.omg.CORBA.IntHolder;
import org.omg.DsLogAdmin.AdministrativeState;
import org.omg.DsLogAdmin.AvailabilityStatus;
import org.omg.DsLogAdmin.ForwardingState;
import org.omg.DsLogAdmin.InvalidAttribute;
import org.omg.DsLogAdmin.InvalidConstraint;
import org.omg.DsLogAdmin.InvalidGrammar;
import org.omg.DsLogAdmin.InvalidLogFullAction;
import org.omg.DsLogAdmin.InvalidMask;
import org.omg.DsLogAdmin.InvalidParam;
import org.omg.DsLogAdmin.InvalidRecordId;
import org.omg.DsLogAdmin.InvalidThreshold;
import org.omg.DsLogAdmin.InvalidTime;
import org.omg.DsLogAdmin.InvalidTimeInterval;
import org.omg.DsLogAdmin.IteratorHolder;
import org.omg.DsLogAdmin.Log;
import org.omg.DsLogAdmin.LogDisabled;
import org.omg.DsLogAdmin.LogFull;
import org.omg.DsLogAdmin.LogIdAlreadyExists;
import org.omg.DsLogAdmin.LogLocked;
import org.omg.DsLogAdmin.LogMgr;
import org.omg.DsLogAdmin.LogOffDuty;
import org.omg.DsLogAdmin.LogRecord;
import org.omg.DsLogAdmin.NVPair;
import org.omg.DsLogAdmin.OperationalState;
import org.omg.DsLogAdmin.TimeInterval;
import org.omg.DsLogAdmin.UnsupportedQoS;
import org.omg.DsLogAdmin.WeekMaskItem;

import alma.Logging.AcsLogServiceOperations;
import alma.Logging.LogStatistics;
import alma.Logging.XmlLogRecord;

public class LogEmptyImpl implements AcsLogServiceOperations 
{
	public Log copy(IntHolder id) {
		return null;
	}
	public Log copy_with_id(int id) throws LogIdAlreadyExists {
		return null;
	}
	public int delete_records(String grammar, String c) throws InvalidConstraint, InvalidGrammar {
		return 0;
	}
	public int delete_records_by_id(long[] ids) {
		return 0;
	}
	public void flush() throws UnsupportedQoS {
	}
	public AdministrativeState get_administrative_state() {
		return null;
	}
	public AvailabilityStatus get_availability_status() {
		return null;
	}
	public short[] get_capacity_alarm_thresholds() {
		return null;
	}
	public long get_current_size() {
		return 0;
	}
	public ForwardingState get_forwarding_state() {
		return null;
	}
	public TimeInterval get_interval() {
		return null;
	}
	public short get_log_full_action() {
		return 0;
	}
	public short[] get_log_qos() {
		return null;
	}
	public int get_max_record_life() {
		return 0;
	}
	public long get_max_size() {
		return 0;
	}
	public long get_n_records() {
		return 0;
	}
	public OperationalState get_operational_state() {
		return null;
	}
	public NVPair[] get_record_attribute(long id) throws InvalidRecordId {
		return null;
	}
	public WeekMaskItem[] get_week_mask() {
		return null;
	}
	public int id() {
		return 0;
	}
	public int match(String grammar, String c) throws InvalidConstraint, InvalidGrammar {
		return 0;
	}
	public LogMgr my_factory() {
		return null;
	}
	public LogRecord[] query(String grammar, String c, IteratorHolder i) throws InvalidConstraint, InvalidGrammar {
		return null;
	}
	public LogRecord[] retrieve(long from_time, int how_many, IteratorHolder i) {
		return null;
	}
	public void set_administrative_state(AdministrativeState state) {
	}
	public void set_capacity_alarm_thresholds(short[] threshs) throws InvalidThreshold {
	}
	public void set_forwarding_state(ForwardingState state) {		
	}
	public void set_interval(TimeInterval interval) throws InvalidTimeInterval, InvalidTime {
	}
	public void set_log_full_action(short action) throws InvalidLogFullAction {
	}
	public void set_log_qos(short[] qos) throws UnsupportedQoS {
	}
	public void set_max_record_life(int life) {
	}
	public void set_max_size(long size) throws InvalidParam {
	}
	public void set_record_attribute(long id, NVPair[] attr_list) throws InvalidAttribute, InvalidRecordId {
	}
	public int set_records_attribute(String grammar, String c, NVPair[] attr_list) 
			throws InvalidConstraint, InvalidGrammar, InvalidAttribute {
		return 0;
	}
	public void set_week_mask(WeekMaskItem[] masks) throws InvalidMask, InvalidTimeInterval, InvalidTime {
	}
	public void write_recordlist(LogRecord[] list) throws LogDisabled, LogOffDuty, LogLocked, LogFull {
	}
	public void write_records(Any[] records) throws LogDisabled, LogOffDuty, LogLocked, LogFull {
	}
	
	@Override
	public void writeRecords(XmlLogRecord[] xmlLogRecords) {
		
	}
	@Override
	public void destroy() {
	}
	@Override
	public LogStatistics getStatistics() {
		return new LogStatistics();
	}
}
