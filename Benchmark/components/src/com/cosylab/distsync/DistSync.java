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

import java.rmi.Naming;


/**
 * DOCUMENT ME!
 *
 * @author kzagar To change the template for this generated type comment go to
 *         Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and
 *         Comments
 */
public class DistSync
{
	/**
	 * DOCUMENT ME!
	 *
	 * @param args DOCUMENT ME!
	 */
	public static void main(String[] args)
	{
		if(args.length == 0) {
			System.err.println("usage: java -jar distsync.jar <rmi-registry-host>");
			System.err.println("<rmi-registry-host> must not be localhost!");
			System.exit(1);
		}
		//if (System.getSecurityManager() == null) {
		//	System.setSecurityManager(new RMISecurityManager());
		//}
		//switch()
		String name = "//" + args[0] + "/RemoteConcurrentFactory";

		try {
			RemoteConcurrentFactory engine = new RemoteConcurrentFactoryImpl();
			Naming.rebind(name, engine);
			System.out.println("RemoteConcurrentFactory bound");
		} catch (Exception e) {
			System.err.println("RemoteConcurrentFactory exception: "
			    + e.getMessage());
			e.printStackTrace();
		}
	}
}

/* __oOo__ */
