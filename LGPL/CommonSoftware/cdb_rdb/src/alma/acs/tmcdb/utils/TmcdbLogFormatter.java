package alma.acs.tmcdb.utils;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

public class TmcdbLogFormatter extends Formatter {

    private String lineSeparator = "\n";
    
    @Override
    public String format(LogRecord record) {
        StringBuffer sb = new StringBuffer();
        sb.append(String.format("%-7s", record.getLevel().getLocalizedName()));
        String message = formatMessage(record);
        sb.append(": ");
        sb.append(message);
        sb.append(" ");
        if (record.getThrown() != null) {
            try {
                StringWriter sw = new StringWriter();
                PrintWriter pw = new PrintWriter(sw);
                record.getThrown().printStackTrace(pw);
                pw.close();
            sb.append(sw.toString());
            } catch (Exception ex) {
            }
        }
        sb.append(lineSeparator);
        return sb.toString();
    }

}