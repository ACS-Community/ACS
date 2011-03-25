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
package alma.acs.classloading;

/** 
 * @author hsommer
 * created Aug 25, 2004 3:23:04 PM
 */
public class TestMain 
{
	TestMain() {
		// expecting alma.acs.classloading.AcsSystemClassLoader
		System.out.println("current class loader is " + getClass().getClassLoader().getClass().getName());
	}

	public static void main(String[] args)
	{
		System.out.println("TestMain#main called with #args=" + args.length);
				
		System.out.println("current thread class loader is " + Thread.currentThread().getContextClassLoader().getClass().getName());
		
		TestMain instance = new TestMain();
	}
}
