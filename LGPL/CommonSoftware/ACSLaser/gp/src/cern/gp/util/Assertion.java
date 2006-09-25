package cern.gp.util;

/**
 *  An assertion class, that enables programmers to make certain affirmations
 *  about things that have to be true in the code. If an assertion is broken,
 *  this means that there is a bad error in the code. Often, assertions are used
 *  in to assure that the parameters of a function call are correct, e.g. if a
 *  non-null parameter has been passed.
 *
 * @author Vito Baggiolini
 *
 * @version $Revision: 1.2 $, $Date: 2006/09/25 08:52:36 $, $Author: acaproni $
 */
public class Assertion {
  private final static String NB_ERRORMANAGER_CLASSNAME = "org.openide.ErrorManager";
  private static boolean IN_NETBEANS;

  private final static String NB_ASSERT_CLASSNAME = "cern.gp.util.NbAssert";
  private static IAssert NB_ASSERT;

  private static boolean initialized = false;

  /**
   * Lazy initialization method. Checks whether we are in a Netbeans Environment or in
   * a normal environment
   */
  private final static void init() {
    if (initialized) {
      return;
    }

    // check if we can get the main Netbeans class. If yes, we're in a NB environment
    // and we handle Assertions via the NB_ASSERT class.
    // TODO we should have a more reliable method to find out whether we are
    // in a Netbeans + GUI environment. We might simply have added the openide.jar
    // in an EJB environment
    try {
      Class.forName(NB_ERRORMANAGER_CLASSNAME);
      Class.forName(NB_ASSERT_CLASSNAME);
      IN_NETBEANS = true;
    } catch (Exception ex) {
      IN_NETBEANS = false; // this is not an error, it just means that we could not find the NB class
    }

    // temporary variable, needed because we cannot try to assign the final
    // variable NB_ASSERT at several points in the try/catch clause.

    if (IN_NETBEANS) {
      try {
        Class nbClass = Class.forName(NB_ASSERT_CLASSNAME, true, Assertion.class.getClassLoader());
        NB_ASSERT = (IAssert) nbClass.newInstance();
      } catch (Exception ex) {
        System.err.println("could not find class " + NB_ASSERT_CLASSNAME + ex);
        ex.printStackTrace();
        return;
      }
    }
    initialized = true;
  }
  /**
   *  Assert a function to assert a certain property of the code. If the asserted
   *  property is false, the Assertion is violated and an RuntimeException is
   *  thrown. <p>
   *
   *  For example, assert that <code>str</code> is non-null: <p>
   *
   *  <blockquote><pre>
   *
   *  void myFunction(String str) { Assert.assertTrue(str != null, "str != null"); }
   *  </pre</blockquote> <p>
   *
   *  Example: make sure min <= max: <blockquote><pre>
   *
   *  boolean compare(int min, int max) { Assert.assertTrue(min <= max, "min <=
   *  max"); }</pre></blockquote> <p>
   *
   *  Example: make sure that num is greater or equal 0: <p>
   *
   *  <blockquote><pre>
   *
   *  double squareRoot(double num) { Assert.assertTrue(num >= 0, "num >= 0"); }
   *  </pre</blockquote> <p>
   *
   *
   *
   * @param  booleanCond the condition expressed as a boolean
   * @param  stringCond the same condition, given as a String. This is
   *      used to fill in the error message in the Exception that is launched if
   *      the Assertion is violated.
   * @throws  RuntimeException  if the Assertion is violated. The message in the
   *      exception is the string in the <code>stringCond</code> parameter.
   * @return the condition, which in this implementation is always true (a RuntimeException is thrown on false condition)
   */
  public static boolean assertTrue(boolean booleanCond, String stringCond) {
    init();
    if (booleanCond == false) {
      if (IN_NETBEANS) {
        NB_ASSERT.doAssert(booleanCond, stringCond);
      } else {
        throw new RuntimeException("*** Assertion failed: " + stringCond);
      }
    }
    return booleanCond;
  }

  /**
   *  The main program for the Assert class
   *
   *@param  args  The command line arguments
   */
  public static void main(String[] args) {
    int a = 1;
    int b = 1;
    int c = 2;
    assertTrue(a == b, "a == b");
    assertTrue(a == c, "a == c");
  }
}
