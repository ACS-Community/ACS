/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package com.cosylab.cdb.browser;


import java.util.LinkedHashMap;
/**
 * CDBLogicTest tests saving/loading a file, clearing as well as reading a file. Each time a method is 
 * called setUp and tearDown are run.
 * 
 * @author
 */
public class CDBLogicTest extends junit.framework.TestCase
{
    CDBLogic   myLogic;
    
    public CDBLogicTest(String name)
    {
	super(name);
    }
    
    protected void setUp()
	{
	}
    
    protected void tearDown()
    {
    }
    
    public void testSetKey()
    {
	CDBLogic.setKey("TestValueForKey"); 
	String newKey = CDBLogic.getKey();
	//System.out.println("Retrieved Key is:" + newKey);
	assertEquals("/rootTestValueForKey", newKey);
    }
    
    public void testSetKey2()
    {
	CDBLogic.setKey("");
	String newKey = CDBLogic.getKey();
	//System.out.println("Retrieved Key is:" + newKey);
	assertEquals("/root", newKey);
    }
   
}







