/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
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
package alma.acs.tmcdb.logic;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;

import junit.framework.TestCase;
import alma.acs.logging.ClientLogManager;
import alma.acs.tmcdb.ContStartOptType;
import alma.acs.tmcdb.Container;
import alma.acs.tmcdb.ContainerStartupOption;
import alma.acs.tmcdb.logic.ContainerStartupOptionHelper.WrapperOptionParser;

/**
 * stand-alone test for {@link ContainerStartupOptionHelper}.
 * @author hsommer
 */
public class ContainerStartupOptionHelperTest extends TestCase {

	private ContainerStartupOptionHelper optionHelper;
	private Logger logger;

	private final String testFlags = "-e myOtherContainerExecutable --managerReference=corbalocToOtherManager " + 
									"--passthroughProcessStart=\"-maxHeapSize 100m -clientVM -D mytest.prop=dontdoit\" " +
									"--passthrough=\"-myDummyContainerArg 77\"";

	protected void setUp() {
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(getName(), false);
		logger.info("----- " + getName() + " -----");
		optionHelper = new ContainerStartupOptionHelper(logger);
	}
	
	public void testWrapperOptionParser() {
		WrapperOptionParser wop = new WrapperOptionParser();
		
		// test null option string
		wop.parseAll(null);
		assertEquals("", wop.getWrappedOptionsContainerExecutable());
		assertEquals("", wop.getWrappedOptionsContainerArgs());
		assertEquals("", wop.getRemainingOptions());
		
		// test empty option string
		wop.parseAll("");
		assertEquals("", wop.getWrappedOptionsContainerExecutable());
		assertEquals("", wop.getWrappedOptionsContainerArgs());
		assertEquals("", wop.getRemainingOptions());
		
		// test without wrapper options
		String flags1 = "-a b what  --stupid option";
		wop.parseAll(flags1);
		assertEquals("", wop.getWrappedOptionsContainerExecutable());
		assertEquals("", wop.getWrappedOptionsContainerArgs());
		assertEquals(flags1, wop.getRemainingOptions());
		
		// test without wrapper options, plus padded spaces
		wop.parseAll("  " + flags1 + " ");
		assertEquals("", wop.getWrappedOptionsContainerExecutable());
		assertEquals("", wop.getWrappedOptionsContainerArgs());
		assertEquals(flags1, wop.getRemainingOptions());
		
		// test with empty wrapper options
		String flags2 = "--passthroughProcessStart=\"\" --passthrough=\"\"";
		wop.parseAll(flags2);
		assertEquals("", wop.getWrappedOptionsContainerExecutable());
		assertEquals("", wop.getWrappedOptionsContainerArgs());
		assertEquals("", wop.getRemainingOptions());
		
		// test with empty wrapper options and other options
		wop.parseAll(" --passthroughProcessStart=\"\"  -a=b  --passthrough=\"\"c d ");
		assertEquals("", wop.getWrappedOptionsContainerExecutable());
		assertEquals("", wop.getWrappedOptionsContainerArgs());
		assertEquals("-a=b  c d", wop.getRemainingOptions());
		
		// test with multiple wrapper option occurrence and other options
		wop.parseAll(" --passthrough = \"one option\"  -a=b  --passthrough=\"another option\"c d ");
		assertEquals("", wop.getWrappedOptionsContainerExecutable());
		assertEquals("one option another option", wop.getWrappedOptionsContainerArgs());
		assertEquals("-a=b  c d", wop.getRemainingOptions());
		
		// test malformatted quotes around wrapped options
		try {
			wop.parseAll("--passthrough=bla missing quotes");
			fail("expected IllegalArgumentException for missing quotes");
		} catch (IllegalArgumentException ex) {
			assertEquals("Wrapper option at pos.0 must be followed by '=' and single or double quotes.", ex.getMessage());
		}
		
		try {
			wop.parseAll("--passthrough=\"bla missing closing quotes");
			fail("expected IllegalArgumentException for missing closing quotes");
		} catch (IllegalArgumentException ex) {
			assertEquals("Wrapper option at pos.0 must be followed by '=' and a pair of '\"' chars around the wrapped options.", ex.getMessage());
		}
		
		try {
			wop.parseAll("--passthrough='bla unequal quotes\"");
			fail("expected IllegalArgumentException for unequal quotes");
		} catch (IllegalArgumentException ex) {
			assertEquals("Wrapper option at pos.0 must be followed by '=' and a pair of '\'' chars around the wrapped options.", ex.getMessage());
		}
		
		try {
			wop.parseAll("--passthrough=.'bad char before quotes\"");
			fail("expected IllegalArgumentException for unequal quotes");
		} catch (IllegalArgumentException ex) {
			assertEquals("Wrapper option at pos.0 must be followed by '=' and single or double quotes.", ex.getMessage());
		}
		
		// one more test, with the flags used for other tests
		wop.parseAll(testFlags);
		assertEquals("-maxHeapSize 100m -clientVM -D mytest.prop=dontdoit", wop.getWrappedOptionsContainerExecutable());
		assertEquals("-myDummyContainerArg 77", wop.getWrappedOptionsContainerArgs());
		assertEquals("-e myOtherContainerExecutable --managerReference=corbalocToOtherManager", wop.getRemainingOptions());
	}
	
	/**
	 * Tests parsing of an options string into the 3 categories.
	 */
	public void testFromOptionsString() {
		Container container = new Container();
		logger.info("Will convert the following flags string: " + testFlags);
		
		Collection<ContainerStartupOption> options = optionHelper.convertFlagsString(container, testFlags);
		
		assertEquals("Expected 3 ContainerStartupOption instances, 2 for the wrapper contents and 1 for the unwrapped options.", 3, options.size());
		for (ContainerStartupOption option : options) {
			assertSame("container reference should be as provided", container, option.getContainer());
			assertEquals(ContainerStartupOptionHelper.OPTION_NAME_LEGACY_CONCATENATED, option.getOptionName());
			if (option.getOptionType().equals(ContStartOptType.EXEC_ARG)) {
				assertEquals("all unwrapped options expected here.", 
						"-e myOtherContainerExecutable --managerReference=corbalocToOtherManager", 
						option.getOptionValue() );
				logger.info("EXEC_ARG was OK");
			}
			else if (option.getOptionType().equals(ContStartOptType.EXEC_ARG_LANG)) {
				assertEquals("--passthroughProcessStart options expected here.", 
						"-maxHeapSize 100m -clientVM -D mytest.prop=dontdoit", 
						option.getOptionValue() );
				logger.info("EXEC_ARG_LANG was OK");
			}
			else if (option.getOptionType().equals(ContStartOptType.CONT_ARG)) {
				assertEquals("--passthrough options expected here.", 
						"-myDummyContainerArg 77", 
						option.getOptionValue() );
				logger.info("CONT_ARG was OK");
			}
		}
	}
	
	
	public void testToOptionsString() {
		Container container = new Container();
		
		ContainerStartupOption execOpt1 = new ContainerStartupOptionForTest(container, ContStartOptType.EXEC_ARG, "-e myOtherContainerExecutable");
		ContainerStartupOption execOpt2 = new ContainerStartupOptionForTest(container, ContStartOptType.EXEC_ARG, "--managerReference=corbalocToOtherManager");
		ContainerStartupOption execLangOpt1 = new ContainerStartupOptionForTest(container, ContStartOptType.EXEC_ARG_LANG, "-maxHeapSize 100m");
		// next option is not "atomic", but that should be tolerated
		ContainerStartupOption execLangOpt2 = new ContainerStartupOptionForTest(container, ContStartOptType.EXEC_ARG_LANG, "-clientVM -D mytest.prop=dontdoit");
		ContainerStartupOption contOpt1 = new ContainerStartupOptionForTest(container, ContStartOptType.CONT_ARG, "-myDummyContainerArg 77");
		
		// give options in mixed order with respect to type
		List<ContainerStartupOption> options = new ArrayList<ContainerStartupOption>();
		options.add(execOpt1);
		options.add(execLangOpt1);
		options.add(contOpt1);
		options.add(execLangOpt2);
		options.add(execOpt2);
		
		String flags = optionHelper.convertContainerStartupOptions(options);
		assertEquals("Converted ContainerStartupOptions should match the fixed test flags string", testFlags, flags);
	}

	private class ContainerStartupOptionForTest extends ContainerStartupOption {
		private static final long serialVersionUID = 5456679798313111043L;

		ContainerStartupOptionForTest(Container container, ContStartOptType type, String value) {
			setContainer(container);
			setOptionType(type);
			setOptionValue(value);
		}
	}
}
