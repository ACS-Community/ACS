package alma.acs.monitoring.blobber;

public class DataLock<C> {

    private C myData;

    private boolean myDataAvailable;

    public synchronized void put(C inData) {
        try {
            while (this.myDataAvailable) {
                wait();
            }
            this.myDataAvailable = true;
            this.myData = inData;
            notify();
        } catch (InterruptedException e) {
        }
    }

    public synchronized C take() {
        try {
            while (!this.myDataAvailable) {
                wait();
            }
            this.myDataAvailable = false;
            notify();
        } catch (InterruptedException e) {
        }
        return this.myData;
    }
}
