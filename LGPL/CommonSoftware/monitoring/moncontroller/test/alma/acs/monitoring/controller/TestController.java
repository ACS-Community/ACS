package alma.acs.monitoring.controller;

import java.util.HashMap;

import alma.MonitorArchiver.BlobberOperations;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.monitoring.blobber.TestBlobber;

public class TestController extends ControllerImpl {

    protected HashMap<String, BlobberOperations> myBlobberMap = new HashMap<String, BlobberOperations>();
    private boolean myUseDatabase;

    public TestController(boolean inUseDatabase) throws ComponentLifecycleException {
        this.myUseDatabase = inUseDatabase;
        initialize(null);// @TODO this will not work
    }

    @Override
    public void initialize(ContainerServices inContainerServices) throws ComponentLifecycleException {
        String name = "BLOBBER_1";
        this.myBlobberList.add(name);
        TestBlobber testBlobber1 = new TestBlobber();
        this.myBlobberMap.put(name, testBlobber1);
        testBlobber1.initialize(inContainerServices, name, this.myUseDatabase);
        name = "BLOBBER_2";
        this.myBlobberList.add(name);
        TestBlobber testBlobber2 = new TestBlobber();
        testBlobber2.initialize(inContainerServices, name, this.myUseDatabase);
        this.myBlobberMap.put(name, testBlobber2);
    }

    @Override
    protected BlobberOperations getBlobber(String inName) {
        return this.myBlobberMap.get(inName);
    }
}
