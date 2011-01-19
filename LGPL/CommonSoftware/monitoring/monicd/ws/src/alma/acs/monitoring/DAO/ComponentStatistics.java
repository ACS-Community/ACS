package alma.acs.monitoring.DAO;

import java.math.BigDecimal;

public class ComponentStatistics {
    public BigDecimal min;
    public BigDecimal max;
    public BigDecimal mean;
    public BigDecimal stdDev;

    public ComponentStatistics() {
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("min: ");
        builder.append(min);
        builder.append(" max: ");
        builder.append(max);
        builder.append(" mean: ");
        builder.append(mean);
        builder.append(" stdDev: ");
        builder.append(stdDev);
        builder.append("\n");
        return builder.toString();
    }
}
