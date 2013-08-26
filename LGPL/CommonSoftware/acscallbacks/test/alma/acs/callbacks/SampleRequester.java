/*
 * Created on Oct 21, 2004 by mschilli
 */
package alma.acs.callbacks;

import java.util.Arrays;
import java.util.logging.Logger;

import alma.ACS.CBDescIn;
import alma.ACS.CBstring;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.demo.DemoCallbackResponder;
import alma.demo.DemoCallbackResponderHelper;



/**
 *
 * @author mschilli
 */
public class SampleRequester extends ComponentClientTestCase {

	
	public SampleRequester() throws Exception {
		super(SampleRequester.class.getName());
	}


	
	public void setUp () throws Exception {

		super.setUp();

		cs = super.getContainerServices();
		logger = cs.getLogger();

		// --- connect to component 
		logger.info("connecting to DemoCallbackResponder component");
		responder = DemoCallbackResponderHelper.narrow(cs.getComponent("DEMOCALLBACKRESPONDER"));

	}
	
	
	ContainerServices cs;
	Logger logger; 
	DemoCallbackResponder responder;
	

	public void testA () throws Exception {
		
		// --- activate callback object
		
		ResponseReceiver x = new ResponseReceiver() {
			public void incomingResponse (String s) {System.out.println("Incoming String: "+s);}
			public void incomingResponse (String[] s) {System.out.println("Incoming StringArray: "+Arrays.asList(s));}
			public void incomingResponse (int s) {System.out.println("Incoming int: "+s);}
			public void incomingResponse (int[] s) {System.out.println("Incoming intArray: "+s.length);}
			public void incomingResponse (double s) {System.out.println("Incoming double: "+s);}
			public void incomingResponse (double[] s) {System.out.println("Incoming doubleArray: "+s.length);}
			public void incomingResponse (Object s) {System.out.println("Incoming Object: "+s);}
			public void incomingException (Exception s) {System.out.println("Responding failed: "+s);}
		};
		
		CBDescIn descIn = RequesterUtil.giveDescIn();
		
		logger.info("setting string callback");
		CBstring cb = RequesterUtil.giveCBString(cs, x);
		responder.revertString("Hallo", cb, descIn);

		logger.info("setting string callback again");
		responder.revertString("Welt", cb, descIn);
		
		logger.info("setting long callback");
		responder.countString("Hallo", RequesterUtil.giveCBLong(cs, x), descIn);

		logger.info("setting long[] callback");
		responder.convertString("Hallo", RequesterUtil.giveCBLongSequence(cs, x), RequesterUtil.giveDescIn());

		logger.info("setting string[] callback");
		responder.dummy1("Hallo", RequesterUtil.giveCBStringSequence(cs, x), RequesterUtil.giveDescIn());

		logger.info("setting double[] callback");
		responder.dummy2("Hallo", RequesterUtil.giveCBDoubleSequence(cs, x), RequesterUtil.giveDescIn());

		logger.info("setting double callback");
		responder.dummy3("Hallo", RequesterUtil.giveCBDouble(cs, x), RequesterUtil.giveDescIn());
		

	}
	


}

//
//
//
//
//
//
//
//
//
//
//
//