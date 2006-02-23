package alma.ACS.MasterComponentImpl.statemachine;

import alma.acs.genfw.runtime.sm.AcsStateActionException;

public interface AlmaSubsystemActions
{
	// todo: enum pattern
//	// action IDs for generic handling by the generated state machine
//	int ACTION_initSubsysPass1 = 0;
//	int ACTION_initSubsysPass2 = 1;
//	int ACTION_reinitSubsystem = 2;
//	int ACTION_shutDownSubsysPass1 = 3;
//	int ACTION_shutDownSubsysPass2 = 4;

	// action methods to be implemented by the state machine user application
	
	void initSubsysPass1() throws AcsStateActionException;

	void initSubsysPass2() throws AcsStateActionException;

	void reinitSubsystem() throws AcsStateActionException;

	void shutDownSubsysPass1() throws AcsStateActionException;

	void shutDownSubsysPass2() throws AcsStateActionException;

}
