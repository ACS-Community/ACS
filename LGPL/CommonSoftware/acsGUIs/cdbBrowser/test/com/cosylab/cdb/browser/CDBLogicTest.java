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







