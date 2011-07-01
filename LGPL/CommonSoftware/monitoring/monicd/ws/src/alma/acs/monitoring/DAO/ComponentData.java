package alma.acs.monitoring.DAO;

public class ComponentData {
    public String componentName;

    public String propertyName;

    public Integer index;

    public String serialNumber;

    public long startTime;

    public long stopTime;
    
    public int sampleSize;

    public String clob = "";
    
    public ComponentStatistics statistics = null;

    public void reset() {
        clob = "";
        startTime = 0;
        stopTime = 0;
        statistics = null;
        index = null;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("componentName [");
        builder.append(componentName);
        builder.append("] propertyName: [");
        builder.append(propertyName);
        builder.append("] serialNumber: [");
        builder.append(serialNumber);
        builder.append("] startTime: [");
        builder.append(startTime);
        builder.append("] stopTime: [");
        builder.append(stopTime);
// 	HSO: logging the clob in alma.archive.tmcdb.monitor.BlobberWorker.storeData(BlobData) is too much for the current tests, so I temporarily comment it out here.
//      	TODO: check where else it is used, or if we should log it when a special debug property is set.
//		builder.append("] clobBuilder: [");
//		builder.append(clob);
        builder.append("] index: [");
        builder.append(index);
        builder.append("]");
        if (statistics != null) {
            builder.append(" ");
            builder.append(statistics);
        }
        return builder.toString();
    }

	/*
	 * Next methods hashCode and equals override the ones that are inherited
	 * from Object They were added to allow ComponentData to be a key of a
	 * HashMap ( ComponenentData => MonitorCharacteristicIds )
	 */
	public int hashCode() {
		return componentName.toString().hashCode()
				+ serialNumber.toString().hashCode()
				+ propertyName.toString().hashCode() + index.hashCode();
	}

	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;

		ComponentData componentData = (ComponentData) obj;

		if (componentData.componentName.equals(this.componentName)
				&& componentData.propertyName.equals(this.propertyName)
				&& componentData.serialNumber.equals(this.serialNumber)
				&& componentData.index == this.index) {
			return true;
		}

		return false;
	}
	
	public String propertyPathname() {
		return componentName + ":" + propertyName + ":" + index;
	}
}
