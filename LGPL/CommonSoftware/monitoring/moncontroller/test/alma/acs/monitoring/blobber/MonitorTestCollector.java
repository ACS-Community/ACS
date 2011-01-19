package alma.acs.monitoring.blobber;

import org.omg.CORBA.Context;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.DomainManager;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.NVList;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.Object;
import org.omg.CORBA.Policy;
import org.omg.CORBA.Request;
import org.omg.CORBA.SetOverrideType;

import alma.ACS.ComponentStates;
import alma.MonitorCollectorErr.DeviceAlreadyRegistredEx;
import alma.MonitorCollectorErr.DeviceNotRegistredEx;
import alma.MonitorCollectorErr.RegisteringDeviceProblemEx;
import alma.MonitorCollectorErr.StartMonitoringProblemEx;
import alma.MonitorCollectorErr.StopMonitoringProblemEx;
import alma.TMCDB.MonitorCollector;
import alma.TMCDB.MonitorDataBlock;
import alma.TMCDB.propertySerailNumber;

public class MonitorTestCollector implements MonitorCollector {

    private static final long serialVersionUID = -7497756569813748359L;

    private DataLock<MonitorDataBlock[]> myDataLock = new DataLock<MonitorDataBlock[]>();

    public MonitorTestCollector() {
    }

    @Override
    public MonitorDataBlock[] getMonitorData() {
        MonitorDataBlock[] outArray = this.myDataLock.take();
        if (outArray == null) {
            outArray = new MonitorDataBlock[0];
        }
        return outArray;
    }

    public void setMonitorData(MonitorDataBlock[] inData) {
        this.myDataLock.put(inData);
    }

    @Override
    public void deregisterMonitoredDevice(String componentName)
            throws DeviceNotRegistredEx {
    }

    @Override
    public void registerMonitoredDevice(String componentName,
            String serialNumber) throws RegisteringDeviceProblemEx,
            DeviceAlreadyRegistredEx {
    }

    @Override
    public void registerMonitoredDeviceWithMultipleSerial(String componentName,
            propertySerailNumber[] serialNumbers)
            throws RegisteringDeviceProblemEx, DeviceAlreadyRegistredEx {
    }

    @Override
    public void startMonitoring(String componentName)
            throws StartMonitoringProblemEx {
    }

    @Override
    public void stopMonitoring(String componentName)
            throws StopMonitoringProblemEx {
    }

    @Override
    public ComponentStates componentState() {
        return null;
    }

    @Override
    public String name() {
        return "MonitorTestCollector";
    }

    @Override
    public Request _create_request(Context ctx, String operation,
            NVList arg_list, NamedValue result) {
        return null;
    }

    @Override
    public Request _create_request(Context ctx, String operation,
            NVList arg_list, NamedValue result, ExceptionList exclist,
            ContextList ctxlist) {
        return null;
    }

    @Override
    public Object _duplicate() {
        return null;
    }

    @Override
    public DomainManager[] _get_domain_managers() {
        return null;
    }

    @Override
    public Object _get_interface_def() {
        return null;
    }

    @Override
    public Policy _get_policy(int policy_type) {
        return null;
    }

    @Override
    public int _hash(int maximum) {
        return 0;
    }

    @Override
    public boolean _is_a(String repositoryIdentifier) {
        return false;
    }

    @Override
    public boolean _is_equivalent(Object other) {
        return false;
    }

    @Override
    public boolean _non_existent() {
        return false;
    }

    @Override
    public void _release() {
    }

    @Override
    public Request _request(String operation) {
        return null;
    }

    @Override
    public Object _set_policy_override(Policy[] policies,
            SetOverrideType set_add) {
        return null;
    }

}
