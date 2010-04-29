package alma.acs.tmcdb.logic;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;

import junit.framework.TestCase;
import alma.acs.logging.ClientLogManager;
import alma.acs.tmcdb.Container;
import alma.acs.tmcdb.ContainerStartupOption;
import alma.acs.tmcdb.logic.ContainerStartupOptionHelper.OptionType;

public class ContainerStartupOptionHelperTest extends TestCase {

	private ContainerStartupOptionHelper optionHelper;
	private Logger logger;

	private final String testFlags = "-e myOtherContainerExecutable --managerReference=corbalocToOtherManager " + 
									"--passthroughProcessStart=\"-maxHeapSize 100m -clientVM -D mytest.prop=dontdoit\" " +
									"--passthrough=\"-myDummyContainerArg 77\"";

	protected void setUp() {
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(getName(), false);
		optionHelper = new ContainerStartupOptionHelper(logger);
	}
	
	
	public void testFromOptionsString() {
		Container container = new Container();
		logger.info("Will convert the following flags string: " + testFlags);
		
		Collection<ContainerStartupOption> options = optionHelper.convertFlagsString(container, testFlags);
		
		assertEquals("Whole flags string should become one TMCDB option since we don't parse it yet.", 1, options.size());
		ContainerStartupOption option = options.iterator().next();
		
		assertSame("container reference should be as provided", container, option.getContainer());
		assertEquals(OptionType.EXEC_ARG.toString(), option.getOptionType());
		assertEquals(ContainerStartupOptionHelper.OPTION_NAME_LEGACY_CONCATENATED, option.getOptionName());
		assertEquals("flags string should become the option value", testFlags, option.getOptionValue());
	}
	
	
	public void testToOptionsString() {
		Container container = new Container();
		
		ContainerStartupOption execOpt1 = new ContainerStartupOptionForTest(container, OptionType.EXEC_ARG, "-e myOtherContainerExecutable");
		ContainerStartupOption execOpt2 = new ContainerStartupOptionForTest(container, OptionType.EXEC_ARG, "--managerReference=corbalocToOtherManager");
		ContainerStartupOption execLangOpt1 = new ContainerStartupOptionForTest(container, OptionType.EXEC_ARG_LANG, "-maxHeapSize 100m");
		// next option is not "atomic", but that should be tolerated
		ContainerStartupOption execLangOpt2 = new ContainerStartupOptionForTest(container, OptionType.EXEC_ARG_LANG, "-clientVM -D mytest.prop=dontdoit");
		ContainerStartupOption contOpt1 = new ContainerStartupOptionForTest(container, OptionType.CONT_ARG, "-myDummyContainerArg 77");
		
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
		ContainerStartupOptionForTest(Container container, ContainerStartupOptionHelper.OptionType type, String value) {
			setContainer(container);
			setOptionType(type.toString());
			setOptionValue(value);
		}
	}
}
