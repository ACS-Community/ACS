package alma.demo.ErrorSystemComp;

import java.lang.NullPointerException;
import alma.acs.component.ComponentImplBase;
import alma.demo.ErrorSystemOperations; 
import alma.ErrorSystemExample.ErrorSystemExampleEx;
import alma.ErrorSystemExample.NothingCanBeScheduledErrorEx;
import alma.ErrorSystemExample.PipelineProcessingRequestErrorEx;
import alma.ErrorSystemExample.wrappers.AcsJErrorSystemExampleEx;
import alma.ErrorSystemExample.wrappers.AcsJNothingCanBeScheduledErrorEx;
import alma.ErrorSystemExample.wrappers.AcsJPipelineProcessingRequestErrorEx;


public class ErrorSystemImpl 
    extends ComponentImplBase 
        implements ErrorSystemOperations {

    
    public ErrorSystemImpl() {
    }

    //////////////////////////////////////////
    // IDL Methods
    //////////////////////////////////////////

    public void tryToScheduleSomething() throws NothingCanBeScheduledErrorEx {
        
        String schedblock = null;
        try {
            if(schedblock != null) {
                //Schedule it! 
            } else {
                throw new NullPointerException("Can't scheduled a null SB!");
            }
        } catch (NullPointerException e) {
            //Create the wrapper class for the NothingCanBeScheduledErrorEx using the 
            //exception that was caught.
            AcsJNothingCanBeScheduledErrorEx e2 = new AcsJNothingCanBeScheduledErrorEx(e);
            // use the method in the generated wrapper class to prepare our regular error
            // for CORBA transport. Basically allowing all the information of what it is and 
            // where it came from and why it happened to be retained after its been sent
            // over CORBA
            throw e2.toNothingCanBeScheduledErrorEx();
        }
    }
    public void tryToProcessSomething() throws PipelineProcessingRequestErrorEx {
        //Trying to process something.
        try {
            //To process something there must have been something scheduled.
            //Since there was nothing scheduled the Nothing can be scheduled error was thrown
            tryToScheduleSomething();
            //Since we threw an exception that has the base class of type ErrorSystemExample 
            //we will show that we can catch it here.
        } catch(NothingCanBeScheduledErrorEx e) {
            //Create one of the wrapper-classes using the caught exception.
            AcsJPipelineProcessingRequestErrorEx e2 = new AcsJPipelineProcessingRequestErrorEx(e);
            // use the method in the generated wrapper class to prepare our regular error
            // for CORBA transport. Basically allowing all the information of what it is and 
            // where it came from and why it happened to be retained after its been sent
            // over CORBA
            throw e2.toPipelineProcessingRequestErrorEx();
        }
    }

    public void usingWrapperClasses1() throws NothingCanBeScheduledErrorEx {
        //In this method I want to show how you can use the base-class 
        //wrapper-class
        try {
            throw new AcsJNothingCanBeScheduledErrorEx("Couldn't scheduled due to bad weather!");
        } catch(AcsJErrorSystemExampleEx e){
            throw ((AcsJNothingCanBeScheduledErrorEx)e).toNothingCanBeScheduledErrorEx();
        }
    }
    public void usingWrapperClasses2() {
        //In this method I want to show how you can use the base-class 
        //wrapper-class
        try {
            throw new AcsJNothingCanBeScheduledErrorEx("Testing printStackTrace!");
        } catch(AcsJErrorSystemExampleEx e){
            e.printStackTrace();
        }
    }
    
}
