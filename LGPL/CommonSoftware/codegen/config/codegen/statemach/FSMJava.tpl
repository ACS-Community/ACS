«DEFINE Root FOR StateMachine»
  «EXPAND ActionInterface»
  «EXPAND SuperContextClass»
  «EXPAND AbstractStateClass»
  «EXPAND StateClass(Name) FOREACH TopState.SubVertex»
«ENDDEFINE»
 

«DEFINE ActionInterface FOR StateMachine»
  «FILE "FSM/"Name"Actions.java"»
    package FSM;

    public interface «Name»Actions {
      «FOREACH DeepAction AS act EXPAND»
        void «act.Name»();
      «ENDFOREACH»
    }
  «ENDFILE»
«ENDDEFINE»


«DEFINE SuperContextClass FOR StateMachine»
  «FILE "FSM/"Name"Context.java"»
    package FSM;
    import FSM.states.*;

    /**
     * The super context class for the «Name» state machine.
     */
    public class «Name»Context 
    {	    
      private «Name»StateAbstract m_currentState;
      private «Name»Actions m_actionDelegate;
      
      // state objects
    «FOREACH TopState.DeepSubState AS st EXPAND»
      public «st.Name»State m_state«st.Name»;
    «ENDFOREACH»

      public «Name»Context(«Name»Actions actions) {
        m_actionDelegate = actions;
    «FOREACH TopState.DeepSubState AS st EXPAND»
      «IF st.isToplevelState»
        m_state«st.Name» = new «st.Name»State(this);
      «ELSE»
        m_state«st.Name» = new «st.Name»State(this, m_state«st.Container.Name»);
      «ENDIF»
    «ENDFOREACH»
        // initial state
        m_currentState = m_state«TopState.InitialState»;
      }
      
      public void setState(«Name»StateAbstract newState, String eventName) {
        m_currentState = newState;
        m_currentState.entry(); // executes the entry action
      }
      
      // delegates incoming events to current state class
    «FOREACH TopState.DistinctDeepOutEvent AS ev EXPAND»
      public void «ev.Name»() {
        m_currentState.«ev.Name»();
      }
    «ENDFOREACH»
    «FOREACH TopState.DistinctDeepInternalEvent AS ev EXPAND»
      public void «ev.Name»() {
        m_currentState.«ev.Name»();
      }
    «ENDFOREACH»

      // delegates actions to user-provided action handler
    «FOREACH DeepAction AS act EXPAND»
      public void «act.Name»() {
        m_actionDelegate.«act.Name»();
      }
    «ENDFOREACH»
    }
  «ENDFILE»
«ENDDEFINE»





«DEFINE AbstractStateClass FOR StateMachine»
  «FILE "FSM/states/"Name"StateAbstract.java"» «NONL»
    package FSM.states;
    
    import FSM.«Name»Context;
    import alma.acs.genfw.runtime.sm.AcsState;
    
    /**
     * Abstract state class. 
     * Concrete subclass receives delegation calls from the SM's super context class.
     */
    public abstract class «Name»StateAbstract implements AcsState
    {
      protected «Name»Context m_superContext;
    
      public «Name»StateAbstract(«Name»Context superContext) {
        m_superContext = superContext;
      }
      
      // events
    «FOREACH TopState.DistinctDeepOutEvent AS ev EXPAND»
      public void «ev.Name»() {}
    «ENDFOREACH» 
    // internal events
    «FOREACH TopState.DistinctDeepInternalEvent AS ev EXPAND»
      public void «ev.Name»() {}
    «ENDFOREACH» 
      // entry, exit, do events
      public void entry() {}
      public void exit() {}
      public void doX() {}
    }
  «ENDFILE»
«ENDDEFINE»




«DEFINE StateClass(superStateName) FOR CompositeState»
  «FILE "FSM/states/"Name"State.java"» «NONL»
    package FSM.states;
    
    import FSM.«StateMachine.Name»Context;
    import alma.acs.genfw.runtime.sm.AcsState;
    
    public class «Name»State «NONL»
    «IF isToplevelState» «NONL»
      extends «superStateName»StateAbstract «NONL»
    «ELSE» «NONL»
      extends «superStateName»SubStateAbstract 
    «ENDIF»    
    {
      public «Name»SubStateAbstract m_subState;
      
    «EXPAND StateConstructor(superStateName)»

      public void setSubstate(«Name»SubStateAbstract subState) {
        m_subState = subState;
      }
      
      public String stateName() {
		return "«Name»";
      }

      public AcsState[] getStateHierarchy() 
      {
        AcsState[] substates = m_subState.getStateHierarchy();
        AcsState[] hierarchy = new AcsState[substates.length + 1];
        hierarchy[0] = this;
        System.arraycopy(substates, 0, hierarchy, 1, substates.length);
        return hierarchy;
      }
      
      public void entry() 
      {  // todo: generate entry action if required
      }
      
      
    public void setSubstate(«Name»SubStateAbstract newSubState, String eventName) {
      «Name»SubStateAbstract oldSubState = m_subState;
      if (oldSubState != newSubState) {
        //todo m_superContext.logTransition(oldSubState, newSubState, eventName);
        m_subState = newSubState;
      }
		
      // always propagate state change upwards «NONL»
      «IF isToplevelState»
        m_superContext.setState(this, eventName);
      «ELSE»
        m_context.setSubstate(this, eventName);
      «ENDIF» 
		
      if (oldSubState != newSubState) {
        m_subState.entry();
      }
    }

    // events handled by this state:
    «FOREACH DistinctDeepOutEvent AS ev EXPAND»
      public void «ev.Name»() {
      }
    «ENDFOREACH»

    }
  «ENDFILE»
  
  «FILE "FSM/states/"Name"SubStateAbstract.java"» «NONL»
    package FSM.states;
    
    import FSM.«StateMachine.Name»Context;
    import alma.acs.genfw.runtime.sm.AcsState;
    
    /**
     * Abstract class for substates of composite state '«Name»'.
     */
    public abstract class «Name»SubStateAbstract implements AcsState
    {
      public «StateMachine.Name»Context m_superContext;
      
      // the nested state «Name»State serves as both state class and context class for its substates
      public «Name»State m_context;
    
      public «Name»SubStateAbstract(«StateMachine.Name»Context superContext, «Name»State context) {
        m_superContext = superContext;
        m_context = context;
      }
      
      public abstract AcsState[] getStateHierarchy();
      public abstract String stateName();
      public abstract void entry();
      
    «FOREACH DistinctDeepOutEvent AS ev EXPAND»
      public void «ev.Name»() {
        System.err.println("illegal call");
      }
    «ENDFOREACH»
    }
  «ENDFILE»
  
  «EXPAND StateClass(Name) FOREACH SubVertex»  
«ENDDEFINE»


«DEFINE StateConstructor(superStateName) FOR State»
    «IF isToplevelState»
      public «Name»State(«StateMachine.Name»Context superContext) {
      super(superContext);
    «ELSE»
      public «Name»State(«StateMachine.Name»Context superContext, «superStateName»State context) {
      super(superContext, context);
    «ENDIF» «NONL»
      }
«ENDDEFINE»



«DEFINE StateClass(superStateName) FOR SimpleState»
  «FILE "FSM/states/"Name"State.java"» «NONL»
    package FSM.states;
    
    import alma.acs.genfw.runtime.sm.AcsSimpleState;
    import alma.acs.genfw.runtime.sm.AcsState;
    
    «IF hasDoAction»
    import alma.acs.genfw.runtime.sm.AcsDoActivity;
    import alma.acs.genfw.runtime.sm.AcsStateActionException;
    «ENDIF»
    
    import FSM.«StateMachine.Name»Context;
    
    public class «Name»State extends «superStateName»SubStateAbstract implements AcsSimpleState
    {
    «IF hasDoAction»
	private AcsDoActivity m_doActivity;
    «ENDIF»

    «EXPAND StateConstructor(superStateName)»
    
	public String stateName() 
	{
		return "«Name»";
	}

	public AcsState[] getStateHierarchy() 
	{
		return new AcsState[] {this};
	}

	public void activate(String eventName) 
	{
		synchronized (m_superContext) {		
			m_context.setSubstate(this, eventName);
		}
	}
	
    public void entry() 
    {
    «IF hasDoAction»
      // perform do/ action asynchronously
      if (m_doActivity == null) {
        // todo : get error state from a new stereotype (instead of name "Error")
        m_doActivity = new AcsDoActivity("«Name»", m_superContext.m_state«CompletionTransition.TargetVertex.Name», m_superContext.m_stateError) {
          public void runActions() throws AcsStateActionException 
          {
            «EXPAND ActionMethodCall FOR DoAction»
          }
        };
      }
      m_doActivity.execute();
    «ENDIF»
	}

	public void exit()
	{
    «IF hasDoAction»
      m_doActivity.terminateActions();
    «ENDIF»
	}
    
    // events handled by this state:
    «EXPAND ConcreteEventMethod FOREACH OutTransition»    
    
    }
    
  «ENDFILE»
«ENDDEFINE»

«DEFINE StateClass(superStateName) FOR StateVertex»
«ENDDEFINE»


«DEFINE ConcreteEventMethod FOR Transition»
  «IF hasTrigger»
    public void «Trigger.Name»() {    
    «IF hasAction»
      «EXPAND ActionMethodCall FOR Action»
    «ENDIF»
    exit();
    m_superContext.m_state«TargetVertex.Name».activate("«Trigger.Name»");
    }
  «ENDIF»
«ENDDEFINE»



«DEFINE ActionMethodCall FOR ActionSequence»
  «EXPAND ActionMethodCall FOREACH Action» «NONL»
«ENDDEFINE»

«DEFINE ActionMethodCall FOR Action»
  m_superContext.«Name»(); «NONL»
«ENDDEFINE»
