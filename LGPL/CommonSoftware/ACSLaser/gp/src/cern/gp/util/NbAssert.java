/*
 * NbAssert.java
 *
 * Created on April 29, 2002, 9:49 AM
 */

package cern.gp.util;
import org.openide.ErrorManager;

/**
 * A Netbeans oriented implementation of the Assert mechanism. Can be called either directly, using
 * the static NbAssert.assertTrue method or from the principal Assert class
 * @see cern.spsea.util.Assert
 * @author  Vito Baggiolini
 * @created 29-04-02
 * @version 0.3 3-May-02
 */
public class NbAssert implements IAssert {
    private static NbAssert ass = null; // singleton, created by get()
    private static final Object lock = new Object();
    /**
     * implementation of the IAssert interface. Called either from the assertTrue method in this class
     * or from the assertTrue method in the main Assert class.
     * <Strong>beware:</strong> Unlike the normal {@link cern.spsea.util.Assert#assertTrue(boolean, String)}
     * method execution continues after this statement, it does not throw a RuntimeException.
     * If the program cannot continue after an assertion failure, you have to take appropriate measures yourself
     * @param condition the condition to be checked
     * @param errMsg the message to be displayed if the condition is false
     * @return the condition
     * @see cern.spsea.util.Assert
     */
    public boolean doAssert(boolean condition, String errMsg) {
        if (condition == false) {
            Throwable thr = new RuntimeException("*** Assertion failed: " + errMsg);
            //TopManager.getDefault().getErrorManager().notify(ErrorManager.ERROR, thr);
            ErrorManager.getDefault().notify(thr);
        }
        return condition;
    }
    
    /**
     * asserts a condition. Does nothing if the condition is true, shows the error message in a JDialog if
     * the condition is false
     * <Strong>beware:</strong> Unlike the normal {@link cern.spsea.util.Assert#assertTrue(boolean, String)}
     * method execution continues after this statement, it does not throw a RuntimeException.
     * If the program cannot continue after an assertion failure, you have to take appropriate measures yourself
     * @param condition the condition to be checked
     * @param errMsg the message to be displayed if the condition is false
     * @return the condition
     * @deprecated (use assertTrue() instead)
     */
//    public static boolean assert(boolean condition, String errMsg) {
//        return get().doAssert(condition, errMsg);
//    }

    /**
     * asserts a condition. Does nothing if the condition is true, shows the error message in a JDialog if
     * the condition is false
     * <Strong>beware:</strong> Unlike the normal {@link cern.spsea.util.Assert#assertTrue(boolean, String)}
     * method execution continues after this statement, it does not throw a RuntimeException.
     * If the program cannot continue after an assertion failure, you have to take appropriate measures yourself
     * @param condition the condition to be checked
     * @param errMsg the message to be displayed if the condition is false
     * @return the condition
     */
    public static boolean assertTrue(boolean condition, String errMsg) {
        return get().doAssert(condition, errMsg);
    }

    /**
     * factory method, initializes and/or returns the NbAssert singleton
     */
    private final static NbAssert get() {
        if (ass == null) {
            synchronized (lock) {
                ass = new NbAssert();
            }
        }
        return ass;
    }
    
}
//