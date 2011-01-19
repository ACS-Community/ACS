package alma.acs.monitoring.controller;

import java.util.HashMap;

import alma.MonitorArchiver.BlobberOperations;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.monitoring.blobber.TestBlobber;

public class TestController extends ControllerImpl {

    protected HashMap<String, BlobberOperations> myBlobberMap = new HashMap<String, BlobberOperations>();
    private boolean myUseDatabase;

    public TestController(boolean inUseDatabase) {
        try {
            this.myUseDatabase = inUseDatabase;
            initialize(null);
        } catch (ComponentLifecycleException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    @Override
    public void initialize(ContainerServices inContainerServices)
            throws ComponentLifecycleException {
        String name = "BLOBBER_1";
        this.myBlobberList.add(name);
        this.myBlobberMap.put(name, new TestBlobber(name, this.myUseDatabase));
        name = "BLOBBER_2";
        this.myBlobberList.add(name);
        this.myBlobberMap.put(name, new TestBlobber(name, this.myUseDatabase));
    }

    @Override
    protected BlobberOperations getBlobber(String inName) throws Exception {
        return this.myBlobberMap.get(inName);
    }
}
