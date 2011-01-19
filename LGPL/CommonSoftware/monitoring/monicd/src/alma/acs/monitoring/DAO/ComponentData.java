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
        builder.append("] clobBuilder: [");
        builder.append(clob);
        builder.append("] index: [");
        builder.append(index);
        builder.append("]");
        if (statistics != null) {
            builder.append(" ");
            builder.append(statistics);
        }
        return builder.toString();
    }
}


