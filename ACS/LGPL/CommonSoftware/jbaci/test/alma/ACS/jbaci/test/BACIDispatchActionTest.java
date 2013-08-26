/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2002
 * Copyright by ESO (in the framework of the ALMA collaboration)
 * and Cosylab 2002, All rights reserved
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */


package alma.ACS.jbaci.test;

import java.util.Vector;
import java.util.logging.Logger;

import junit.framework.TestCase;

import org.omg.CORBA.Context;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.DomainManager;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.NVList;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.Policy;
import org.omg.CORBA.Request;
import org.omg.CORBA.SetOverrideType;

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.Callback;
import alma.ACS.jbaci.BACIDispatchAction;
import alma.ACS.jbaci.BACIFramework;
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACSErr.Completion;
import alma.acs.container.CleaningDaemonThreadFactory;
import alma.acs.logging.ClientLogManager;

/**
 * <code>BACIDispatchAction</code> test.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class BACIDispatchActionTest extends TestCase {

	/**
	 * Response wait time in ms. Defaults to 5s.
	 */
	private static final int RESPONSE_WAIT_TIME = 5000;
	
	/**
	 * Dummy response wait time in ms. Defaults to 2s.
	 */
	private static final int DUMMY_WAIT_TIME = 2000;

	/**
	 * Thread factory for BACI FW.
	 */
	private CleaningDaemonThreadFactory threadFactory;

	private Logger logger;
	
	/**
	 * Dispatch action. 
	 */
	class DispatchAction
	{
		public DispatchAction(
				int type,
				Object value,
				Callback callback,
				Completion completion,
				CBDescOut desc)
		{
			this.type = type;
			this.value = value;
			this.callback = callback;
			this.completion = completion;
			this.desc = desc;
		}

		/**
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		public boolean equals(Object obj) {
			DispatchAction da = (DispatchAction)obj;
			return (type == da.type && value == da.value && desc == da.desc &&
					completion == da.completion && callback == da.callback);
		}

		public int type;
		public Object value;	
		public CBDescOut desc;
		public Completion completion;
		public Callback callback;
	}

	/**
	 * Test callback dispatcher impl.
	 * Always sucessfull callback dispatcher.
	 */
	class TestCallbackDispatcher implements CallbackDispatcher
	{ 
		/**
		 * Sync. response queue.
		 */
		protected Vector responseQueue = new Vector(); 

		/**
		 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACS.Callback, alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public boolean dispatchCallback(
			int type,
			Object value,
			Callback callback,
			Completion completion,
			CBDescOut desc) {
				
			responseQueue.add(new DispatchAction(type, value, callback, completion, desc));
			
			if ((type != CallbackDispatcher.WORKING_TYPE &&
				type != CallbackDispatcher.DONE_TYPE) ||
				callback == null ||
				completion == null ||
				desc == null)
				return false;
			else
				return true;
		}

		/**
		 * Get response queue.
		 * @return response queue.
		 */
		public Vector getResponseQueue() {
			return responseQueue;
		}

	}

	/**
	 * Always disaster callback dispatcher.
	 */
	class PerfectCallbackDispatcher extends TestCallbackDispatcher
	{ 
		/**
		 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACS.Callback, alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized boolean dispatchCallback(
			int type,
			Object value,
			Callback callback,
			Completion completion,
			CBDescOut desc) {
			boolean retVal = super.dispatchCallback(type, value, callback, completion, desc);
			this.notify();
			return retVal;
		}

	}

	/**
	 * Always disaster callback dispatcher.
	 */
	class DisasterCallbackDispatcher extends TestCallbackDispatcher
	{
		/**
		 * Failure count.
		 */ 
		private int count = 0;
		
		/**
		 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACS.Callback, alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized boolean dispatchCallback(
			int type,
			Object value,
			Callback callback,
			Completion completion,
			CBDescOut desc) {
			super.dispatchCallback(type, value, callback, completion, desc);
			if (++count == 3)
				this.notify();
			return false;
		}

	}

	/**
	 * Exception callback dispatcher impl.
	 * Always throws exception.
	 */
	class ExceptionCallbackDispatcher extends TestCallbackDispatcher
	{
		/**
		 * Failure count.
		 */ 
		private int count = 0;

		/**
		 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACS.Callback, alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized boolean dispatchCallback(
			int type,
			Object value,
			Callback callback,
			Completion completion,
			CBDescOut desc) {
			super.dispatchCallback(type, value, callback, completion, desc);
			if (++count == 3)
				this.notify();
			throw new RuntimeException("test exception"); 
		}

	}

	/**
	 * First filed callback dispatcher.
	 */
	class FirstFailedCallbackDispatcher extends TestCallbackDispatcher
	{ 
		/**
		 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACS.Callback, alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized boolean dispatchCallback(
			int type,
			Object value,
			Callback callback,
			Completion completion,
			CBDescOut desc) {
			// simple impl. and thread-safe
			
			boolean first = true;
			DispatchAction[] actions = null;
			synchronized (responseQueue)
			{
				actions = new DispatchAction[responseQueue.size()];
				responseQueue.toArray(actions);
			}
			for (int i = 0; first && i < actions.length; i++)
				if (type == actions[i].type &&
					value == actions[i].value &&
					callback == actions[i].callback &&
					completion == actions[i].completion &&
					desc == actions[i].desc)
					first = false;
			super.dispatchCallback(type, value, callback, completion, desc);
			if (!first)
				this.notify();
			return !first;
		}

	}

	/**
	 * Override policy test dispatcher impl.
	 * Always throws exception.
	 */
	class OverrideTestCallbackDispatcher extends TestCallbackDispatcher
	{
		/**
		 * Constructor.
		 */
		public OverrideTestCallbackDispatcher()
		{
			this(false);
		}

		/**
		 * Constructor.
		 */
		public OverrideTestCallbackDispatcher(boolean blockingFails)
		{
			this.blockingFails = blockingFails;
		}
		
		/**
		 * Failure count.
		 */ 
		private int count = 0;

		/**
		 * Failure count.
		 */ 
		private boolean blockingFails;

		/**
		 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACS.Callback, alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized boolean dispatchCallback(
			int type,
			Object value,
			Callback callback,
			Completion completion,
			CBDescOut desc) {
			
			boolean retVal = super.dispatchCallback(type, value, callback, completion, desc);
			this.notify();
			
			// first callback will block, so that other request could do some override themselves
			if (++count == 1)
			{
				try
				{
					this.wait(RESPONSE_WAIT_TIME);
				}
				catch (InterruptedException ie) {}
				
				if (blockingFails)
					return false;
			}
				
			return retVal;				
		}

	}

	/**
	 * Test callback impl.
	 */
	class TestCallback implements Callback
	{

		/**
		 * @see alma.ACS.CallbackOperations#negotiate(long, alma.ACS.CBDescOut)
		 */
		public boolean negotiate(long arg0, CBDescOut arg1) {
			return false;
		}

		/**
		 * @see org.omg.CORBA.Object#_create_request(org.omg.CORBA.Context, java.lang.String, org.omg.CORBA.NVList, org.omg.CORBA.NamedValue, org.omg.CORBA.ExceptionList, org.omg.CORBA.ContextList)
		 */
		public Request _create_request(
			Context ctx,
			String operation,
			NVList arg_list,
			NamedValue result,
			ExceptionList exclist,
			ContextList ctxlist) {
			// auto-generated method stub
			return null;
		}

		/**
		 * @see org.omg.CORBA.Object#_create_request(org.omg.CORBA.Context, java.lang.String, org.omg.CORBA.NVList, org.omg.CORBA.NamedValue)
		 */
		public Request _create_request(
			Context ctx,
			String operation,
			NVList arg_list,
			NamedValue result) {
			// auto-generated method stub
			return null;
		}

		/**
		 * @see org.omg.CORBA.Object#_duplicate()
		 */
		public org.omg.CORBA.Object _duplicate() {
			// auto-generated method stub
			return null;
		}

		/**
		 * @see org.omg.CORBA.Object#_get_domain_managers()
		 */
		public DomainManager[] _get_domain_managers() {
			// auto-generated method stub
			return null;
		}

		/**
		 * @see org.omg.CORBA.Object#_get_interface_def()
		 */
		public org.omg.CORBA.Object _get_interface_def() {
			// auto-generated method stub
			return null;
		}

		/**
		 * @see org.omg.CORBA.Object#_get_policy(int)
		 */
		public Policy _get_policy(int policy_type) {
			// auto-generated method stub
			return null;
		}

		/**
		 * @see org.omg.CORBA.Object#_hash(int)
		 */
		public int _hash(int maximum) {
			// auto-generated method stub
			return 0;
		}

		/**
		 * @see org.omg.CORBA.Object#_is_a(java.lang.String)
		 */
		public boolean _is_a(String repositoryIdentifier) {
			// auto-generated method stub
			return false;
		}

		/**
		 * @see org.omg.CORBA.Object#_is_equivalent(org.omg.CORBA.Object)
		 */
		public boolean _is_equivalent(org.omg.CORBA.Object other) {
			// auto-generated method stub
			return false;
		}

		/**
		 * @see org.omg.CORBA.Object#_non_existent()
		 */
		public boolean _non_existent() {
			// auto-generated method stub
			return false;
		}

		/**
		 * @see org.omg.CORBA.Object#_release()
		 */
		public void _release() {
			// auto-generated method stub

		}

		/**
		 * @see org.omg.CORBA.Object#_request(java.lang.String)
		 */
		public Request _request(String operation) {
			// auto-generated method stub
			return null;
		}

		/**
		 * @see org.omg.CORBA.Object#_set_policy_override(org.omg.CORBA.Policy[], org.omg.CORBA.SetOverrideType)
		 */
		public org.omg.CORBA.Object _set_policy_override(
			Policy[] policies,
			SetOverrideType set_add) {
			// auto-generated method stub
			return null;
		}

	}

	/**
	 * Constructor for BACIDispatchActionTest.
	 * @param name
	 */
	public BACIDispatchActionTest(String name) {
		super(name);
	}

	protected void setUp() throws Exception {
		String name = getClass().getSimpleName();
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(name, false);
		logger.info("START----------------------------" + getName() + "-------------");
		threadFactory = new CleaningDaemonThreadFactory(name, logger);
		BACIFramework.INSTANCE.initialize(threadFactory);
	}

	protected void tearDown() throws Exception {
		// this should clean all the threads
		BACIFramework.INSTANCE.shutdown();

		threadFactory.cleanUp();
		logger.info("END------------------------------" + getName() + "-------------\n\n");
	}


	/**
	 * Methods that check dispatcher response.
	 */
	private void checkResponse(
		Object value,
		Completion completion,
		CBDescIn descIn,
		Callback callback,
		DispatchAction response) {
		// check reponse type
		assertEquals(CallbackDispatcher.DONE_TYPE, response.type);
		
		// check value (compare pointers)
		assertTrue(value == response.value);
		
		// check callback (compare callback)
		assertTrue(callback == response.callback);
		
		// check descriptor
		assertEquals(descIn.id_tag, response.desc.id_tag);
		
		// check completion (compare pointers)
		assertTrue(completion == response.completion);
	}

	/**
	 * Dummy wait. 
	 */
	private void dummyWait()
	{
		try
		{
			Thread.sleep(DUMMY_WAIT_TIME);
		}
		catch (InterruptedException ie) {}
	}

	/**
	 * Normal (success) test.
	 */
	public void testNormal()
	{
		Object value = new Object();
		Completion completion = CompletionUtil.generateNoErrorCompletion();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1234);
		Callback callback = new TestCallback();
		TestCallbackDispatcher dispatcher = new PerfectCallbackDispatcher();
		
		BACIDispatchAction action = new BACIDispatchAction(callback, descIn, dispatcher);
		
		synchronized (dispatcher)
		{
			action.dispatchDoneRequest(completion, value);
			
			try
			{
				// wait 
				dispatcher.wait(RESPONSE_WAIT_TIME);
			}
			catch (InterruptedException ie) {}
		}
		
		// dummy wait (just in case there is a bug and more responses will come)
		dummyWait();
		
		// only 1 response is expected
		assertEquals(1, dispatcher.getResponseQueue().size());
		DispatchAction response = (DispatchAction)dispatcher.getResponseQueue().firstElement();
		
		// check response
		checkResponse(value, completion, descIn, callback, response);
	}

	/**
	 * First failed then success test.
	 */
	public void testFirstFailed()
	{
		Object value = new Object();
		Completion completion = CompletionUtil.generateNoErrorCompletion();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1234);
		Callback callback = new TestCallback();
		TestCallbackDispatcher dispatcher = new FirstFailedCallbackDispatcher();
		
		BACIDispatchAction action = new BACIDispatchAction(callback, descIn, dispatcher);
		
		synchronized (dispatcher)
		{
			action.dispatchDoneRequest(completion, value);
			
			try
			{
				// wait 
				dispatcher.wait(RESPONSE_WAIT_TIME);
			}
			catch (InterruptedException ie) {}

		}
		
		// dummy wait (just in case there is a bug and more responses will come)
		dummyWait();

		// only 2 (failed and successful) responses are expected
		assertEquals(2, dispatcher.getResponseQueue().size());
		
		// should be the same
		assertEquals(dispatcher.getResponseQueue().get(0), dispatcher.getResponseQueue().get(1));
		
		DispatchAction response = (DispatchAction)dispatcher.getResponseQueue().firstElement();
		
		// check response
		checkResponse(value, completion, descIn, callback, response);
	}

	/**
	 * Always fails test.
	 */
	public void testAlwaysFails()
	{
		Object value = new Object();
		Completion completion = CompletionUtil.generateNoErrorCompletion();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1234);
		Callback callback = new TestCallback();
		TestCallbackDispatcher dispatcher = new DisasterCallbackDispatcher();
		
		BACIDispatchAction action = new BACIDispatchAction(callback, descIn, dispatcher);
		
		synchronized (dispatcher)
		{
			action.dispatchDoneRequest(completion, value);
			
			try
			{
				// wait 
				dispatcher.wait(RESPONSE_WAIT_TIME);
			}
			catch (InterruptedException ie) {}
		}
		
		// dummy wait (just in case there is a bug and more responses will come)
		dummyWait();
		
		// only 3 responses (retries) are expected
		assertEquals(3, dispatcher.getResponseQueue().size());
		
		// should be the same
		assertEquals(dispatcher.getResponseQueue().get(0), dispatcher.getResponseQueue().get(1));
		
		DispatchAction response = (DispatchAction)dispatcher.getResponseQueue().firstElement();
		
		// check response
		checkResponse(value, completion, descIn, callback, response);
	}

	/**
	 * Always throws exception.
	 */
	public void testException()
	{
		Object value = new Object();
		Completion completion = CompletionUtil.generateNoErrorCompletion();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1234);
		Callback callback = new TestCallback();
		TestCallbackDispatcher dispatcher = new ExceptionCallbackDispatcher();
		
		BACIDispatchAction action = new BACIDispatchAction(callback, descIn, dispatcher);
		
		synchronized (dispatcher)
		{
			action.dispatchDoneRequest(completion, value);
			
			try
			{
				// wait 
				dispatcher.wait(RESPONSE_WAIT_TIME);
			}
			catch (InterruptedException ie) {}
		}
		
		// dummy wait (just in case there is a bug and more responses will come)
		dummyWait();
		
		// only 3 responses (retries) are expected
		assertEquals(3, dispatcher.getResponseQueue().size());
		
		// should be the same
		assertEquals(dispatcher.getResponseQueue().get(0), dispatcher.getResponseQueue().get(1));
		
		DispatchAction response = (DispatchAction)dispatcher.getResponseQueue().firstElement();
		
		// check response
		checkResponse(value, completion, descIn, callback, response);
	}

	/**
	 * No error override policy test.
	 */
	public void noErrorOverrideTest(boolean blockingFails)
	{
		Object value = new Object();
		Completion completion = CompletionUtil.generateNoErrorCompletion();
		Completion completion2 = CompletionUtil.generateNoErrorCompletion();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1234);
		Callback callback = new TestCallback();
		TestCallbackDispatcher dispatcher = new OverrideTestCallbackDispatcher(blockingFails);
		
		BACIDispatchAction action = new BACIDispatchAction(callback, descIn, dispatcher);
		action.setOverridePolicy(true);
		

		synchronized (dispatcher)
		{
			// blocking request		
			action.dispatchDoneRequest(completion, value);
			
			try
			{
				// wait 
				dispatcher.wait(RESPONSE_WAIT_TIME);
			}
			catch (InterruptedException ie) {}
		}

		synchronized (dispatcher)
		{
			// to be overriden
			action.dispatchDoneRequest(completion, value);
			action.dispatchDoneRequest(completion, value);
			action.dispatchDoneRequest(completion, value);

			// to survive ;)
			action.dispatchDoneRequest(completion2, value);
			
			// release blocking request
			dispatcher.notify();
			
			try
			{
				// wait 
				dispatcher.wait(RESPONSE_WAIT_TIME);
			}
			catch (InterruptedException ie) {}
		}
		
		// dummy wait (just in case there is a bug and more responses will come)
		dummyWait();
		
		// only 2 responses are expected
		assertEquals(2, dispatcher.getResponseQueue().size());
		DispatchAction response = (DispatchAction)dispatcher.getResponseQueue().firstElement();
		DispatchAction response2 = (DispatchAction)dispatcher.getResponseQueue().get(1);
		
		// check response
		checkResponse(value, completion, descIn, callback, response);
		checkResponse(value, completion2, descIn, callback, response2);
	}

	/**
	 * Override policy test.
	 */
	public void testOverrideTest()
	{
		noErrorOverrideTest(false);
	}

	/**
	 * Override policy test.
	 */
	public void testOverrideBlockingFailsTest()
	{
		noErrorOverrideTest(true);
	}

	/**
	 * Override policy test (blocking fails, none to override).
	 */
	public void testOverrideBlockingFailsNoneToOverrideTest()
	{
		Object value = new Object();
		Completion completion = CompletionUtil.generateNoErrorCompletion();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1234);
		Callback callback = new TestCallback();
		TestCallbackDispatcher dispatcher = new OverrideTestCallbackDispatcher(true);
		
		BACIDispatchAction action = new BACIDispatchAction(callback, descIn, dispatcher);
		action.setOverridePolicy(true);
		

		synchronized (dispatcher)
		{
			// blocking request		
			action.dispatchDoneRequest(completion, value);
			
			try
			{
				// wait 
				dispatcher.wait(RESPONSE_WAIT_TIME);
			}
			catch (InterruptedException ie) {}
		}
		
		synchronized (dispatcher)
		{
			// notift blocking request
			dispatcher.notify();
		}

		// dummy wait (just in case there is a bug and more responses will come)
		dummyWait();
		
		// only 2 (failed and successful) responses are expected
		assertEquals(2, dispatcher.getResponseQueue().size());
		
		// should be the same
		assertEquals(dispatcher.getResponseQueue().get(0), dispatcher.getResponseQueue().get(1));
		
		DispatchAction response = (DispatchAction)dispatcher.getResponseQueue().firstElement();
		
		// check response
		checkResponse(value, completion, descIn, callback, response);
	}
	
	public static void main(String[] args) {
		junit.textui.TestRunner.run(BACIDispatchActionTest.class);
		// System.exit(0);
	}

}
