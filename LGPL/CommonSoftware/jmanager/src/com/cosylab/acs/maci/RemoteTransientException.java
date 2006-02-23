/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

import abeans.pluggable.RemoteException;

/**
 * Defined specialized <code>RemoteException</code>, this implementation
 * defines an transient remote exception (e.g. CORBA::TRANSIENT()).
 * Remote exception is thrown when a processing of a request fails because of an
 * underlying failure in the remote layer. For example, if some request fails to execute
 * on the remote layer and the remote server returns an error response, that response 
 * should be packed into the <code>RemoteException</code>.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class RemoteTransientException extends RemoteException
{
}
