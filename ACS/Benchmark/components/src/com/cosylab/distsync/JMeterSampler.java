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

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;


/**
 * DOCUMENT ME!
 *
 * @author kzagar To change the template for this generated type comment go to
 *         Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and
 *         Comments
 */
public class JMeterSampler implements JavaSamplerClient
{
	/* (non-Javadoc)
	 * @see org.apache.jmeter.protocol.java.sampler.JavaSamplerClient#setupTest(org.apache.jmeter.protocol.java.sampler.JavaSamplerContext)
	 */
	public void setupTest(JavaSamplerContext arg0)
	{
	}

	/* (non-Javadoc)
	 * @see org.apache.jmeter.protocol.java.sampler.JavaSamplerClient#runTest(org.apache.jmeter.protocol.java.sampler.JavaSamplerContext)
	 */
	public SampleResult runTest(JavaSamplerContext ctx)
	{
		SampleResult result = new SampleResult();

		try {
			RemoteConcurrentFactory rcf = (RemoteConcurrentFactory)Naming
				.lookup(ctx.getParameter("RemoteConcurrentFactory"));
			RemoteCyclicBarrier rcb = rcf.getCyclicBarrier(ctx.getParameter(
				        "CyclicBarrierName"), ctx.getIntParameter("Parties"));
			long t0 = System.currentTimeMillis();
			rcb.barrier();
			result.setTime(System.currentTimeMillis() - t0);
			result.setSuccessful(true);
			result.setSampleLabel("Synchronized on barrier "
			    + ctx.getParameter("CyclicBarrierName"));
		} catch (Exception e) {
			result.setSuccessful(false);
			result.setResponseCode(e.getMessage());
			result.setSampleLabel("ERROR: " + e.getMessage());
		}

		return result;
	}

	/* (non-Javadoc)
	 * @see org.apache.jmeter.protocol.java.sampler.JavaSamplerClient#teardownTest(org.apache.jmeter.protocol.java.sampler.JavaSamplerContext)
	 */
	public void teardownTest(JavaSamplerContext arg0)
	{
	}

	/* (non-Javadoc)
	 * @see org.apache.jmeter.protocol.java.sampler.JavaSamplerClient#getDefaultParameters()
	 */
	public Arguments getDefaultParameters()
	{
		Arguments args = new Arguments();
		args.addArgument("RemoteConcurrentFactory",
		    "//localhost/RemoteConcurrentFactory");
		args.addArgument("CyclicBarrierName", "cyclicBarrier");
		args.addArgument("Parties", "ENTER NUMBER OF PARTIES");

		return args;
	}
}

/* __oOo__ */
