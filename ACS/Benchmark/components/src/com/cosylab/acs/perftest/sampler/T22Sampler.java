package com.cosylab.acs.perftest.sampler;

import java.util.Iterator;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;

import com.cosylab.acs.perftest.client.BCTClient;

/**
 * 
 * @author anzez
 *
 * T22Sampler is JMeter's Java sampler client class to measure time of:
 *  - [TEST_2_2_*] Asynchronous operations throughput/scalability
 *
 */
public class T22Sampler extends AbstractJavaSamplerClient
{
	private static final String TEST_PREFIX = "TEST_2_2_";
	
	public static final String TEST = "1_1";
	public static final long SIZE_OF_SET = 1;
	public static final long DELAY = 0;
	
	private String m_testName, m_instanceID;
	private String m_test;
	
	private BCTClient m_client;
	
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
		
		long tn = context.getLongParameter("ThreadNum", 0);
		m_test = context.getParameter(TEST_PREFIX, TEST);
	
		m_testName = TEST_PREFIX + m_test;
		try {
			String str = java.net.InetAddress.getLocalHost().getHostName();
			m_instanceID = str + ":";
		} catch (Exception e) {}
		m_instanceID += tn;
		
		m_client = null;
		try {
			m_client = new BCTClient();
		}
		catch (Exception e) {
			e.printStackTrace(System.err);
		}
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
		long sizeOfSet = context.getLongParameter("SizeOfSet", SIZE_OF_SET);
		long delay = context.getLongParameter("Delay", DELAY);
		
		SampleResult results = new SampleResult();
		boolean b = true;
		
		try
		{
			switch(m_test.charAt(0)) {
			case '1':
				b = m_client.getPropertyAsync(sizeOfSet, delay);
				break;
			case '2':
				b = m_client.setPropertyAsync(sizeOfSet, delay);
				break;
			case '3':
				b = m_client.actionAsync(sizeOfSet, delay);
				break;
			default:
				throw new Exception("Invalid TEST selection!");
			}

			switch(m_test.charAt(2)) {
			case '1':
				results.setTime(m_client.getMidTime() - m_client.getStartTime());
				break;
			case '2':
				results.setTime(m_client.getEndTime() - m_client.getStartTime());
				break;
			default:
				throw new Exception("Invalid TEST selection!");
			}
			results.setSuccessful(b);
			results.setSampleLabel(m_testName + "(" + sizeOfSet + ", " + delay + ") @" + m_instanceID);
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
		
		m_client = null;
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
		params.addArgument("ThreadNum", "${__threadNum}");
		params.addArgument(TEST_PREFIX, TEST);
		params.addArgument("SizeOfSet", String.valueOf(SIZE_OF_SET));
		params.addArgument("Delay", String.valueOf(DELAY));
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
