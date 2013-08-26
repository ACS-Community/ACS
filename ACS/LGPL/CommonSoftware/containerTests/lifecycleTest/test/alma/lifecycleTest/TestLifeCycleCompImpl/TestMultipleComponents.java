/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
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

package alma.lifecycleTest.TestLifeCycleCompImpl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;
import junit.framework.Assert;
import junit.framework.TestCase;
import alma.acs.component.client.ComponentClientTestCase;
import alma.lifecycleTest.TestLifeCycleComp;
import alma.lifecycleTest.TestLifeCycleCharComp;

public class TestMultipleComponents extends ComponentClientTestCase
{
    private static String componentBaseName;
    private static String componentType;
    private static int numberOfComponents;
    private List<TestLifeCycleComp> simpleComponents;
    private List<TestLifeCycleCharComp> charComponents;
    /**
	 * @throws java.lang.Exception
	 */
	public TestMultipleComponents() throws Exception
	{
		super(TestMultipleComponents.class.getName());
	}
    
    protected void setUp() throws Exception {
        super.setUp();
    }
   

	/**
	 * @throws Exception
	 */
    public void testGetGivenComponents() throws Exception
    {
        if(componentType.equals("SIMPLE")){
		    simpleComponents = new ArrayList<TestLifeCycleComp>();
            for(int i=1 ; i<=numberOfComponents;i++){
                TestLifeCycleComp t = alma.lifecycleTest.TestLifeCycleCompHelper.narrow(getContainerServices().getComponent(componentBaseName+i));
                simpleComponents.add(t);
                t.dummyInterface();
            }
        }else if (componentType.equals("CHAR")){
		    charComponents = new ArrayList<TestLifeCycleCharComp>();
            for(int i=1 ; i<=numberOfComponents;i++){
                TestLifeCycleCharComp t = alma.lifecycleTest.TestLifeCycleCharCompHelper.narrow(getContainerServices().getComponent(componentBaseName+i));
                charComponents.add(t);
                t.on();
            }
        }else{
            System.out.println("SIMPLE or CHAR only!");    
            
        }
    }
	public static void main(String[] args)
	{
        if(args.length != 3) System.exit(1);
        
        componentBaseName = args[0];
        numberOfComponents = Integer.parseInt(args[1]);
        componentType = args[2];
		junit.textui.TestRunner.run(TestMultipleComponents.class);
	}

}

