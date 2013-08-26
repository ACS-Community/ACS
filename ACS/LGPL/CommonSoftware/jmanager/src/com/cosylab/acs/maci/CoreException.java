/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

/**
 * Temporary exception. 
 */
public class CoreException extends Exception
{
	private static final long serialVersionUID = 6132394967371046130L;

	public CoreException(String msg) {
		super(msg);
	}
	public CoreException(String msg, Throwable cause) {
		super(msg, cause);
	}
}
