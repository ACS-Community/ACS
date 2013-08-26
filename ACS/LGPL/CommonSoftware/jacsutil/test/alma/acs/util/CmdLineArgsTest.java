package alma.acs.util;

import junit.framework.TestCase;

/**
 * Unit test for {@link CmdLineArgs}.
 * Does not require a running ACS environment.
 * @author hsommer
 */
public class CmdLineArgsTest extends TestCase {

	/**
	 * If the options are known, it's better to register them before parsing.
	 * Then options names don't need a leading '-', and even values that look like options 
	 * can be recognized (for example "-temperature -273").
	 */
	public void testRegisteredOptions() {
		CmdLineArgs cmdArgs = new CmdLineArgs();
		
		CmdLineRegisteredOption rcloDebug = new CmdLineRegisteredOption("-debug", 0);
		cmdArgs.registerOption(rcloDebug);
		
		CmdLineRegisteredOption rcloPlay = new CmdLineRegisteredOption("-players", 2);
		cmdArgs.registerOption(rcloPlay);

		CmdLineRegisteredOption rcloRated = new CmdLineRegisteredOption("rated", 1);
		cmdArgs.registerOption(rcloRated);

		CmdLineRegisteredOption rcloTemperature = new CmdLineRegisteredOption("-temperature", 1);
		cmdArgs.registerOption(rcloTemperature);
		
		// parse the args using the argument length info provided to RegisteredCmdLineOption
		String[] optArgs1 = {"rated", "3", "-temperature", "-273", "-players", "bernie", "ert", "oscar", "-debug"};
		cmdArgs.parseArgs(optArgs1);

		// override one setting
		String[] optArgs2 = {"rated", "5"};
		cmdArgs.parseArgs(optArgs2);

		
		assertTrue(cmdArgs.isSpecified(rcloDebug));
		assertEquals("Option without argument must not produce values", 0, cmdArgs.getValues(rcloDebug).length);
		
		String[] valuesRated = cmdArgs.getValues(rcloRated);
		assertEquals("Option with one parameter must have one value.", 1, valuesRated.length);		
		assertEquals("bad processing of overwritten arg (second parsing)" + rcloRated.getName(), "5", cmdArgs.getValues(rcloRated)[0]);
		
		assertEquals("failed to process arg whose value starts with '-'" + rcloTemperature.getName(), "-273", cmdArgs.getValues(rcloTemperature)[0]);
		
		String[] valuesPlayers = cmdArgs.getValues(rcloPlay);
		assertEquals("Option with minimum 2 args but effectively 3 args not parsed correctly.", 3, valuesPlayers.length);		
		assertEquals("bad processing of arg " + rcloPlay.getName(), "oscar", cmdArgs.getValues(rcloPlay)[2]);		
	}
	
	
	/**
	 * Usually it also works with options that were not known beforehand.
	 */
	public void testUnregisteredOptions() {
		CmdLineArgs cmdArgs = new CmdLineArgs();
		
		// parse the args, guessing what is an option or a value
		String[] optArgs1 = {"-rated", "3", "-players", "bernie", "ert", "oscar", "-debug"};
		cmdArgs.parseArgs(optArgs1);

		// override one setting
		String[] optArgs2 = {"-rated", "5"};		
		cmdArgs.parseArgs(optArgs2);
		
		// check dumping out the merged args with their values in original order
		String[] mergedArgsAndValues = cmdArgs.getAllArgs();
		assertEquals(optArgs1.length, mergedArgsAndValues.length);
		assertEquals("5", mergedArgsAndValues[1]);
		assertEquals("oscar", mergedArgsAndValues[5]);
		
		// check recognition of args 
		CmdLineOption[] recognizedArgs = cmdArgs.getRecognizedArgs();
		assertNotNull(recognizedArgs);
		assertTrue(recognizedArgs.length == 3);
		
		// check values
		String[] ratedValues = cmdArgs.getValues(recognizedArgs[0]);
		assertEquals("5", ratedValues[0]);
	}

	
	public void testBadArgsForUnregisteredOptions() {
		CmdLineArgs cmdArgs = new CmdLineArgs();
		
		// parse the args, guessing what is an option or a value. Will not recognize "players" as an option
		String[] optArgs1 = {"-rated", "3", "players", "bernie", "ert"};
		cmdArgs.parseArgs(optArgs1);
		
		// check dumping out the merged args with their values in original order
		String[] dumpedArgs = cmdArgs.getAllArgs();
		assertEquals(optArgs1.length, dumpedArgs.length);
		for (int i = 0; i < optArgs1.length; i++) {
			assertEquals(optArgs1[i], dumpedArgs[i]);	
		}
		
		CmdLineOption[] recognizedArgs = cmdArgs.getRecognizedArgs();
		assertNotNull(recognizedArgs);
		assertTrue(recognizedArgs.length == 1);

		String[] ratedValues = cmdArgs.getValues(recognizedArgs[0]);
		assertEquals(4, ratedValues.length);
	}
	
	
	public void testRegisteredOptionsWithAlternativeNames() {
		CmdLineArgs cmdArgs = new CmdLineArgs();
		
		CmdLineRegisteredOption rcloNoRecovery = new CmdLineRegisteredOption("-norecovery", "-nr", 0);
		cmdArgs.registerOption(rcloNoRecovery);
		
		CmdLineRegisteredOption rcloManager = new CmdLineRegisteredOption("-manager", 1);
		cmdArgs.registerOption(rcloManager);

		// parse the args using the argument length info provided to RegisteredCmdLineOption
		String[] optArgs = {"-manager", "Gianni", "-nr"};
		cmdArgs.parseArgs(optArgs);
		
		// check if option given by alternative name was recognized
		assertTrue(cmdArgs.isSpecified(rcloNoRecovery));
		// make sure we don't get false positives
		assertFalse(cmdArgs.isSpecified(new CmdLineRegisteredOption("DummyOption", 2)));
	}
}
