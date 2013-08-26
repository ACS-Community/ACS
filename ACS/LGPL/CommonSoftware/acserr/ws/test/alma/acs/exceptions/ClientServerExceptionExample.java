/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.exceptions;

import alma.ACSErrTypeTest.ACSErrTest0Ex;
import alma.ACSErrTypeTest.wrappers.AcsJACSErrTest0Ex;

/**
 * This class provides methods that are typical of a distributed computing
 * scenario, where exceptions can be thrown locally inside the server implementation,
 * caught, sent back to the calling client over the remote communication
 * protocol, and then processed by the client.
 * 
 * @author hsommer created 25.04.2006 21:29:20
 */
public class ClientServerExceptionExample {

    /**
     * Represents an implementation method that does not rely on any other
     * remote method (lowest level). Therefore, the thrown
     * <code>AcsJACSErrTest0Ex</code> is the VM original, i.e. not converted
     * from an <code>ErrorTrace</code>.
     * 
     * @throws AcsJACSErrTest0Ex
     *             (always), caused by a NPE with message "mean NPE".
     */
    public void throwOriginalAcsJACSErrTest0Ex() throws AcsJACSErrTest0Ex {
        Throwable causedByEx = new NullPointerException("mean NPE");
        AcsJACSErrTest0Ex ex = new AcsJACSErrTest0Ex("low level ex", causedByEx);
        ex.setProperty("MyStupidProperty", "Poverty");
        throw ex;
    }

    /**
     * This method can be seen as an example for a CORBA/remote method, such as
     * the implementation of a method from a Java component's interface for
     * which in IDL the exception <code>ACSErrTest0Ex</code> is declared.
     * <p>
     * The AcsJ-style exception is converted to its CORBA-equivalent, but no new
     * exception is added to the chain (ErrorTrace).
     * 
     * @throws ACSErrTest0Ex always, internally converted from
     *                       <code>AcsJACSErrTest0Ex</code>
     * @see #throwOriginalAcsJACSErrTest0Ex()
     */
    public void throwACSErrTest0Ex() throws ACSErrTest0Ex {
        // top-level try-catch block in the interface implementation
        try {
            // the interface impl internally works with AcsJExceptions...
            throwOriginalAcsJACSErrTest0Ex();
        } catch (AcsJACSErrTest0Ex e) {
            // but to the outside we must convert it to be CORBA compliant
            throw e.toACSErrTest0Ex();
        }
    }

    /**
     * Represents an implementation method of some client application that calls
     * a remote method, here {@link #throwACSErrTest0Ex()}.
     * <p>
     * The <code>AcsJACSErrTest0Ex</code> that will be thrown by this "client"
     * method is caused by an exception in the "remote" method. That remote
     * exception is in general not the VM original, but has been converted from
     * an <code>ACSErrTest0Ex</code> with an <code>ErrorTrace</code> which is the
     * format the exception took on to travel over the CORBA wire.
     * <p>
     * Once caught on the client side, the remote exception is wrapped by a new
     * <code>AcsJACSErrTest0Ex</code> exception whose message is "remote call
     * failed". Thus unlike in method {@link #throwConvertedAcsJACSErrTest0Ex()}
     * here we actually add a new exception to the top of the chain (and thus add an 
     * ErrorTrace in the CORBA picture).
     * 
     * @param extractErrorTraceAutomatically if true, uses the constructor from a Throwable
     *        which internally finds out that it got indeed an ACS exception,
     *        and then extracts the ErrorTrace automatically;
     *        if false, explicitly gets the ErrorTrace, and uses the respective constructor.
     * @throws AcsJACSErrTest0Ex (always)
     * @see #throwACSErrTest0Ex()
     */
    public void throwWrapperAcsJACSErrTest0Ex(boolean extractErrorTraceAutomatically) throws AcsJACSErrTest0Ex {
        try {
            // make (=fake) a remote call
            throwACSErrTest0Ex();
        } catch (ACSErrTest0Ex e) {            
            AcsJACSErrTest0Ex acsJACSErrTest0Ex = null;
            if (extractErrorTraceAutomatically) {
                acsJACSErrTest0Ex = new AcsJACSErrTest0Ex("remote call failed", e);
            }
            else {
                acsJACSErrTest0Ex = new AcsJACSErrTest0Ex("remote call failed", e.errorTrace);
            }
            
            throw acsJACSErrTest0Ex;
        }
    }

    /**
     * Represents an implementation method of some client application that calls
     * a remote method, here {@link #throwACSErrTest0Ex()}.
     * <p>
     * The <code>AcsJACSErrTest0Ex</code> that will be thrown by this "client"
     * method is caused by an exception in the "remote" method. That remote
     * exception is in general not the VM original, but has been converted from
     * an <code>ACSErrTest0Ex</code>/<code>ErrorTrace</code> which is the
     * format the exception took on to travel over the CORBA wire.
     * <p>
     * Once caught on the client side, the remote exception is converted to an
     * <code>AcsJACSErrTest0Ex</code> exception which does not have an
     * independent text message. Thus unlike in method
     * {@link #throwWrapperAcsJACSErrTest0Ex(boolean)} here we do not add a new
     * exception to the top of the chain (or ErrorTrace in the CORBA picture).
     * 
     * @throws AcsJACSErrTest0Ex (always)
     * @see #throwACSErrTest0Ex()
     */
    public void throwConvertedAcsJACSErrTest0Ex() throws AcsJACSErrTest0Ex {
        try {
            // make a remote call
            throwACSErrTest0Ex();
        } catch (ACSErrTest0Ex e) {
            throw AcsJACSErrTest0Ex.fromACSErrTest0Ex(e);
        }
    }

}
