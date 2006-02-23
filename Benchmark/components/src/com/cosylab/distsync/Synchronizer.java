/*
 * Copyright (c) 2003 by Cosylab d.o.o.
 *
 * The full license specifying the redistribution, modification, usage and other
 * rights and obligations is included with the distribution of this project in
 * the file license.html. If the license is not included you may find a copy at
 * http://www.cosylab.com/legal/abeans_license.htm or may write to Cosylab, d.o.o.
 *
 * THIS SOFTWARE IS PROVIDED AS-IS WITHOUT WARRANTY OF ANY KIND, NOT EVEN THE
 * IMPLIED WARRANTY OF MERCHANTABILITY. THE AUTHOR OF THIS SOFTWARE, ASSUMES
 * _NO_ RESPONSIBILITY FOR ANY CONSEQUENCE RESULTING FROM THE USE, MODIFICATION,
 * OR REDISTRIBUTION OF THIS SOFTWARE.
 */

package com.cosylab.distsync;

/**
 * DOCUMENT ME!
 *
 * @author $Author: dfugate $
 * @version $Revision: 1.1 $
 */
public interface Synchronizer
{
	/**
	 * DOCUMENT ME!
	 *
	 * @param token DOCUMENT ME!
	 */
	public void enter(String token);

	/**
	 * DOCUMENT ME!
	 *
	 * @param token DOCUMENT ME!
	 */
	public void waitAndLeave(String token);
}

/* __oOo__ */
