package alma.acs.monitoring.controller;

import java.util.ArrayList;

import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentLifecycleException;

/**
 * @TODO change from testng to junit
 */
public class ControllerUnitTest extends TestController {

    private static final boolean USE_DATABASE = false;

    ArrayList<Milestone> myMilestoneList = new ArrayList<Milestone>();

    protected enum Milestone {
        IS_REGISTERED, ADD_COLLECTOR
    }

    public ControllerUnitTest() throws ComponentLifecycleException {
        super(USE_DATABASE);
    }

    @BeforeClass(groups = { "controller" })
    public void setUp() throws Exception {
    }

    @Test(groups = { "controller" })
    public void testCollectorHandling() throws Exception {

        String coll1 = "COLLECTOR1";
        String coll2 = "COLLECTOR2";
        /*
         * Test that it is possible to register a collector
         */
        registerCollector(coll1);
        checkList(Milestone.IS_REGISTERED, Milestone.ADD_COLLECTOR);
        /*
         * Test that a collector is not added twice
         */
        registerCollector(coll1);
        checkList(Milestone.IS_REGISTERED);
        /*
         * Test that a collector can be added again
         */
        deregisterCollector(coll1);
        registerCollector(coll1);
        checkList(Milestone.IS_REGISTERED, Milestone.ADD_COLLECTOR);
        /*
         * Test that there is no problem trying to remove a non-existing
         * collector and then add it
         */
        deregisterCollector(coll2);
        registerCollector(coll2);
        checkList(Milestone.IS_REGISTERED, Milestone.ADD_COLLECTOR);
    }

    @Override
    protected String addCollector(String inComponentName) throws Exception {
        myMilestoneList.add(Milestone.ADD_COLLECTOR);
        return super.addCollector(inComponentName);
    }

    @Override
    protected String isRegistered(String inComponentName) throws AcsJContainerServicesEx  {
        myMilestoneList.add(Milestone.IS_REGISTERED);
        return super.isRegistered(inComponentName);
    }

    private void checkList(Milestone... inMilestones) {
        ArrayList<Milestone> list = new ArrayList<Milestone>();
        for (Milestone milestone : inMilestones) {
            list.add(milestone);
        }
        assert list.equals(this.myMilestoneList) : "Milestone sequence not the expected, expected "
                + list + ", found " + this.myMilestoneList;
        this.myMilestoneList.clear();
    }
}
