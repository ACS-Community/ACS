package cern.cmw.mom.pubsub;


/**
 * Public interface. The method <b>void onException(MOMException)</b> has to be
 * implemented to handle communication exception. The example below shows a possible implementation :
 * <P><blockquote><pre>
 * class myListener implements ExceptionListener {
 * ...
 * public void onException(MOMExeption e) {
 *   if (e.testException(MOMException.CONNECTION_LOST_EXCEPTION))
 *     System.out.println("CONNECTION_LOST_EXCEPTION");
 *   else
 *     if (e.testException(MOMException.CONNECTION_RECOVERED_EXCEPTION))
 *       System.out.println("CONNECTION_RECOVERED_EXCEPTION");
 * }
 * <P></blockquote></pre>
 *
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 * @see MOMException
 */
public interface ExceptionListener {
  /**
   * The exception handler called in case of communication exception
   *
   * @param e the MOMException caught
   */
  public void onException(MOMException e);
}

