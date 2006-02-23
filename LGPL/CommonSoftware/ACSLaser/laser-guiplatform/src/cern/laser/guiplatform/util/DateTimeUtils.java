/*
 * DateTimeUtils.java
 *
 * Created on October 22, 2003, 11:23 AM
 */

package cern.laser.guiplatform.util;

import java.text.SimpleDateFormat;
import java.util.Date;
/**
 *
 * @author  Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class DateTimeUtils {
  
    private static Date currDate = new Date();

    /** Creates a new instance of DateTimeUtils */
    //public DateTimeUtils() {
    //}
    
    public static final String getCurrentTimestamp() {
        String dateString = null;

        currDate.setTime(System.currentTimeMillis());
        String dateFormat = "yyyy-MM-dd HH:mm:ss.S";
        SimpleDateFormat formater = new SimpleDateFormat(dateFormat);
        dateString = formater.format(currDate);
 
        return dateString;
    }
    
}
