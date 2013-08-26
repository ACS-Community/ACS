package com.cosylab.acs.perftest.sampler;

import java.util.Iterator;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import alma.acs.container.corba.OrbConfigurator;

/**
 * 
 * @author anzez
 *
 * T112Sampler is JMeter's Java sampler client class to measure time of:
 *  - [TEST_1_1_2] Corba initialization 
 *
 */
public class T112Sampler extends AbstractJavaSamplerClient
{
	private static final String TEST_PREFIX = "TEST_1_1_2";
	
	private String m_testName, m_instanceID;
	
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

		m_testName = TEST_PREFIX;
		try {
			String str = java.net.InetAddress.getLocalHost().getHostName();
			m_instanceID = str + ":";
		} catch (Exception e) {}
		m_instanceID += tn;
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
		SampleResult results = new SampleResult();
		
		try
		{
			long time = System.currentTimeMillis();

			POA rootPOA = null;
			OrbConfigurator orbConf = OrbConfigurator.getOrbConfigurator();
			ORB orb = ORB.init(orbConf.getOptions(), orbConf.getProperties());
			rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
			rootPOA.the_POAManager().activate();		

			results.setTime(System.currentTimeMillis() - time);
			results.setSuccessful(true);
			results.setSampleLabel(m_testName + "() @" + m_instanceID);

			if (orb != null)
				orb.destroy();
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
		params.addArgument("ThreadNum", "${__threadNum}");
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
