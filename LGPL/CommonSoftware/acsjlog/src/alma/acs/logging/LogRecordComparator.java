package alma.acs.logging;

import java.util.Comparator;
import java.util.logging.LogRecord;

/**
 * Comparator for {@link LogRecord} objects, which uses the log level, timestamp, and log sequence number for comparison.
 * <p>
 * From {@link PriorityQueue}: "The head of this queue is the least element with respect to the specified ordering.".
 * Since we want the most important and oldest log messages first at the head, a higher level value and a smaller time value 
 * will result in a lower ordering position.
 * Level takes precedence over time.
 * @author hsommer
 * created Apr 19, 2005 2:02:34 PM
 */
public class LogRecordComparator implements Comparator<LogRecord> {
    private boolean timeOnly;
    LogRecordComparator() {
        this(false);
    }
    LogRecordComparator(boolean timeOnly) {
        this.timeOnly = timeOnly;
    }
    public int compare(LogRecord lr1, LogRecord lr2) {
        if (lr1 == lr2) return 0;            
        if (!timeOnly) {
            int lev1 = lr1.getLevel().intValue();
            int lev2 = lr2.getLevel().intValue();               
            int levelComp = (lev1>lev2 ? -1 : (lev1==lev2 ? 0 : 1));
            // if levels are different, we don't have to look at the timestamps
            if (levelComp != 0) {
                return levelComp;
            }
        }
        // same level, now let timestamp decide
        long tm1 = lr1.getMillis();
        long tm2 = lr2.getMillis();
        int timeComp = tm1<tm2 ? -1 : (tm1==tm2 ? 0 : 1);        
        if (timeComp != 0) {
            return timeComp;
        }
        // same time, now let log sequence numbers decide
        long sqn1 = lr1.getSequenceNumber();
        long sqn2 = lr2.getSequenceNumber();        
        return (sqn1<sqn2 ? -1 : (sqn1==sqn2 ? 0 : 1));
    }        
}