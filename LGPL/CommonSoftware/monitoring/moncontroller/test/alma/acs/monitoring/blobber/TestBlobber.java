package alma.acs.monitoring.blobber;

public class TestBlobber extends BlobberImpl {

    public TestBlobber(String inName, boolean inUseDatabase) {
        super();
        this.m_instanceName = inName;
        TestBlobberWorker.setUseDatabase(inUseDatabase);
        initialize();
    }

    @Override
    public String name() {
        return m_instanceName;
    }

    @Override
    protected BlobberWorker createWorker() {
        return new TestBlobberWorker();
    }

    @Override
    protected void startWorker(BlobberWorker inWorker) {
        this.myWorkerThread = new Thread(inWorker);
        this.myWorkerThread.start();
    }

}
