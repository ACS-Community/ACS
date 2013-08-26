/*
 * IAssert.java
 *
 * Created on April 29, 2002, 11:05 AM
 */

package cern.gp.util;

/**
 * An interface for the Assert mechanism. Needed for different, context-dependent implementations of assertion mechanism
 * @author  vbaggiol
 */
public interface IAssert {
    /**
     * Assertion. Does nothing if the boolean condition is true; issues an error message (and possibly an error log)
     * if the condition is false.
     *
     * Method to be implemented by different Assert mechanism.
     * Currently implemented in a Netbeans-specific manner
     *
     * @param condition the condition to be checked
     * @param errMsg the message to be displayed if the condition is false
     */
    boolean doAssert(boolean condition, String message);
}
