/*
 * Created on Jun 27, 2003
 *
 * To change the template for this generated file go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
package alma.acs.testsupport.tat;

import junit.framework.TestCase;
import java.io.FileNotFoundException;
/**
 * @author hsommer
 *
 * To change the template for this generated type comment go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class TATJUnitRunnerTest extends TestCase
{

	/**
	 * Constructor for JUnitRunnerTest.
	 * @param name
	 */
	public TATJUnitRunnerTest(String name)
	{
		super(name);
	}

	public void testAllIsFine()
	{
		System.out.println("all is fine");
	}
	
	public void testWithFailure()
	{
		String str1 = "otis";
		String str2 = "driftwood";
		
		assertEquals("lousy string comparison...", str1, str2);
		
		System.out.println("I guess there was a failure...");
	}
	
	public void testWithError()
	{
		throw new NullPointerException("oops");
	}
	
	
	public static void main(String[] args)
	{
                try{
        		TATJUnitRunner.run(TATJUnitRunnerTest.class);
                }catch(FileNotFoundException ex){
                        System.err.print("Error opening file:"+ex.getMessage());
                }

	}

}
