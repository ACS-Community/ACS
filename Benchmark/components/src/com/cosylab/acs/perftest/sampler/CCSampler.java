package com.cosylab.acs.perftest.sampler;

import java.util.Iterator;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;

import com.cosylab.acs.perftest.client.ComponentClientSingleton;

/**
 * 
 * @author anzez
 *
 * CCSampler is ComponentClient initializer/finalizer sampler.
 * It is required to be run before and after all tests are performed.
 *
 */
public class CCSampler extends AbstractJavaSamplerClient
{
	public static final String DEFAULT_MANAGER = "corbaloc::192.168.1.4:3000/Manager";
	public static final String CLIENT_NAME = "TestingClient";
	public static final long INITIALIZE = 1;
	
	/**
	 * Do any initialization required by this client. It is generally
	 * recommended to do any initialization such as getting parameter
	 * values in the setupTest method rather than the runTest method
	 * in order to add as little overhead as possible to the test.
	 * 
	 * @param context  the context to run with. This provides access
	 *                  to initialization parameters.
	 */
	public void setupTest(JavaSamplerContext context)
	{
		// NOTE: This function is only called once for one thread
		getLogger().debug(whoAmI() + "\tsetupTest()");
		listParameters(context);
	}

	/**
	 * Perform a single sample.
	 * Perform a single sample for each iteration.  This method
	 * returns a <code>SampleResult</code> object.
	 * <code>SampleResult</code> has many fields which can be
	 * used.  At a minimum, the test should use
	 * <code>SampleResult.sampleStart</code> and
	 * <code>SampleResult.sampleEnd</code>to set the time that
	 * the test required to execute.  It is also a good idea to
	 * set the sampleLabel and the successful flag.
	 * 
	 * @see org.apache.jmeter.samplers.SampleResult#sampleStart()
	 * @see org.apache.jmeter.samplers.SampleResult#sampleEnd()
	 * @see org.apache.jmeter.samplers.SampleResult#setSuccessful(boolean)
	 * @see org.apache.jmeter.samplers.SampleResult#setSampleLabel(String)
	 * 
	 * @param context  the context to run with. This provides access
	 *                 to initialization parameters.
	 * 
	 * @return         a SampleResult giving the results of this
	 *                 sample.
	 */
	public SampleResult runTest(JavaSamplerContext context)
	{
		String managerLoc = context.getParameter("Manager", DEFAULT_MANAGER);
		String clientName = context.getParameter("ClientName", CLIENT_NAME);
		long init = context.getLongParameter("Initialize", INITIALIZE);
		
		SampleResult results = new SampleResult();
		
		try
		{
			if (init != 0) {
				ComponentClientSingleton.prepareInstance(null, managerLoc, clientName);
				results.setSampleLabel("Initializing ComponentClient!");
			} else {
				ComponentClientSingleton.destroyInstance();
				results.setSampleLabel("Finalizing ComponentClient!");
			}
			results.setTime(0);
			results.setSuccessful(true);
		}
		catch (Exception e)
		{
			results.setSuccessful(false);
			results.setResponseCode(e.getMessage());
			results.setSampleLabel("ERROR: " + e.getMessage());
			getLogger().error(this.getClass().getName() + ": Error during sample", e);
		}

		if (getLogger().isDebugEnabled())
		{
			getLogger().debug(whoAmI() + "\trunTest()" + "\tTime:\t" + results.getTime());
			listParameters(context);
		}

		return results;
	}

	/**
	 * Do any clean-up required by this test.
	 * 
	 * @param context  the context to run with. This provides access
	 *                  to initialization parameters.
	 */
	public void teardownTest(JavaSamplerContext context)
	{
		// NOTE: This function is only called once for one thread
		getLogger().debug(whoAmI() + "\tteardownTest()");
		listParameters(context);
	}

	/**
	 * Provide a list of parameters which this test supports.  Any
	 * parameter names and associated values returned by this method
	 * will appear in the GUI by default so the user doesn't have
	 * to remember the exact names.  The user can add other parameters
	 * which are not listed here.  If this method returns null then
	 * no parameters will be listed.  If the value for some parameter
	 * is null then that parameter will be listed in the GUI with
	 * an empty value.
	 * 
	 * @return  a specification of the parameters used by this
	 *           test which should be listed in the GUI, or null
	 *           if no parameters should be listed.
	 */
	public Arguments getDefaultParameters()
	{
		Arguments params = new Arguments();
		params.addArgument("Manager", DEFAULT_MANAGER);
		params.addArgument("ClientName", CLIENT_NAME);
		params.addArgument("Initialize", String.valueOf(INITIALIZE));
		return params;
	}

	/**
	 * Dump a list of the parameters in this context to the debug log.
	 *
	 * @param context  the context which contains the initialization
	 *                  parameters.
	 */
	private void listParameters(JavaSamplerContext context)
	{
		if (getLogger().isDebugEnabled())
		{
			Iterator argsIt = context.getParameterNamesIterator();
			while (argsIt.hasNext())
			{
				String name = (String) argsIt.next();
				getLogger().debug(name + "=" + context.getParameter(name));
			}
		}
	}

	/**
	 * Generate a String identifier of this test for debugging
	 * purposes.
	 * 
	 * @return  a String identifier for this test instance
	 */
	private String whoAmI()
	{
		StringBuffer sb = new StringBuffer();
		sb.append(Thread.currentThread().toString());
		sb.append("@");
		sb.append(Integer.toHexString(hashCode()));
		return sb.toString();
	}

}
