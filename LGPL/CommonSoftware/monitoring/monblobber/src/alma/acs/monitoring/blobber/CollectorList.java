package alma.acs.monitoring.blobber;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import alma.MonitorArchiver.CollectorListStatus;
import alma.acs.monitoring.DAO.ComponentData;

public class CollectorList {

    private ArrayList<CollectorData> myList = new ArrayList<CollectorData>();
    private int myIndex = 0;
    private int myLastIndex = 0;

    public CollectorList() {
    }

    public CollectorListStatus add(String inCollectorName)
            throws InterruptedException {
        return add(new CollectorData(inCollectorName));
    }

    public synchronized CollectorListStatus add(CollectorData inData)
            throws InterruptedException {
        CollectorListStatus outValue = CollectorListStatus.KNOWN;
        if (!this.myList.contains(inData)) {
            if (this.myList.size() == 0) {
                this.myIndex = 0;
            }
            this.myList.add(inData);
            notify();
            outValue = CollectorListStatus.ADDED;
        }
        return outValue;
    }

    public CollectorListStatus contains(String inCollectorName) {
        return contains(new CollectorData(inCollectorName));
    }

    public synchronized CollectorListStatus contains(CollectorData inData) {
        CollectorListStatus outValue = CollectorListStatus.UNKNOWN;
        if (this.myList.contains(inData)) {
            outValue = CollectorListStatus.KNOWN;
        }
        return outValue;
    }

    public CollectorListStatus remove(String inCollectorName) {
        return remove(new CollectorData(inCollectorName));
    }

    public synchronized CollectorListStatus remove(CollectorData inData) {
        CollectorListStatus outValue = CollectorListStatus.UNKNOWN;
        int index = this.myList.indexOf(inData);
        if (index != -1) {
            if (index <= this.myIndex) {
                this.myIndex--;
            }
            this.myList.remove(inData);
            outValue = CollectorListStatus.REMOVED;
        }
        return outValue;
    }

    public synchronized int size() {
        return this.myList.size();
    }

    public synchronized CollectorData next() throws InterruptedException {
        CollectorData outData = null;
        while (this.myList.size() == 0) {
            wait();
            this.myIndex = 0;
        }
        outData = this.myList.get(this.myIndex);
        this.myLastIndex = this.myIndex;
        this.myIndex++;
        if (this.myIndex >= this.myList.size()) {
            this.myIndex = 0;
        }
        return outData;
    }

    public synchronized int getLastIndex() {
        return this.myLastIndex;
    }

    protected static class CollectorData {

        private String collectorId;

        public long lastSuccessfulAccessTime;

        /**
         * Key is property name, value is blob data
         */
        public HashMap<String, BlobData> equipmentData = new HashMap<String, BlobData>();

        /**
         * Used to store the previous value for archive-on-change values;
         */
        public Object previousValue;

        public CollectorData(String inCollectorId) {
            if (inCollectorId == null) {
                throw new IllegalArgumentException(
                        "inCollectorId cannot be null.");
            }
            this.collectorId = inCollectorId;
        }

        @Override
        public boolean equals(Object inObject) {
            boolean outResult = false;
            try {
                CollectorData data = (CollectorData) inObject;
                if (getCollectorId().equals(data.getCollectorId())) {
                    outResult = true;
                }
            } catch (Exception e) {
            }
            return outResult;
        }

        public String getCollectorId() {
            return this.collectorId;
        }
    }

    protected static class BlobData extends ComponentData {
        public List<Object> dataList = new ArrayList<Object>();

        public void reset() {
            super.reset();
            dataList.clear();
        }
    }

}
