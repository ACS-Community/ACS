package alma.managertest.client;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import alma.managertest.DummyComponent;
import alma.managertest.DummyComponentHelper;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServicesBase;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ComponentSpec;
import si.ijs.maci.COMPONENT_SPEC_ANY;


public class managerClientTest extends ComponentClientTestCase
{
   
   static int totalComponents;// =10000;//Integer.parseInt(args[0]);
   static int nThreads; //= 20;
   static int totalContainers;// = 40;
   //this variable is to know how many times has been run to initialize the sequence from that number
   static int nRun;

   static public void main(String args[]){
	try{
	managerClientTest m = new managerClientTest();
	totalComponents = Integer.parseInt(args[0]);
	totalContainers = Integer.parseInt(args[1]);
	nThreads = Integer.parseInt(args[2]);
	m.testGetManyComponents();
	}catch(Exception e){}
   }

   public managerClientTest() throws Exception {
	super("managerClientTest");
   }

   public void testGetManyComponents(){
	final String compNameHeader = "DUMMY_COMP_";
	final AtomicInteger sequenceNumber = new AtomicInteger(0);

	totalComponents = 20000;
	totalContainers = 60;
	nThreads = 10; 
	class ComponentGetter implements Callable<Boolean> {

		int components,containers;

	    ComponentGetter(int components, int containers) {
		this.components = components;
		this.containers = containers;
	    }
	    public Boolean call() throws Exception {

		int j;
		String compName,contName;
        ComponentSpec corbaSpec = new ComponentSpec();
        corbaSpec.component_type = "IDL:alma/managertest/DummyComponent:1.0";
        corbaSpec.component_code = COMPONENT_SPEC_ANY.value; 
		for(int i=0; i<components;i++){
		    j = sequenceNumber.getAndIncrement();
		    compName = compNameHeader+(j+1);
		    contName = "javaContainer"+ (j%containers + 1);
            corbaSpec.component_name = compName; 
            corbaSpec.container_name = contName;
	            //we just loose this reference
		    try{
            ComponentInfo cInfo = null;
		    //Using AcsManagerProxy directly to set the value of Container
            cInfo = m_acsManagerProxy.get_dynamic_component(m_acsManagerProxy.getManagerHandle(), corbaSpec, false);
            DummyComponent d  = DummyComponentHelper.narrow(cInfo.reference);

            if (d == null) return false;
		    d.doNothing();

		    }catch(Exception e){
			//do nothing, keep trying other components
		    }
		     //TODO:here we can give maybe the comp number to see when the program crashed?
		    //double ret = d.returnAttributeValue(5);
		    //if (ret != 5) return false; 
		}
		return true;
	   }
	}

		
	ExecutorService pool = Executors.newFixedThreadPool(totalComponents, getContainerServices().getThreadFactory());
	CountDownLatch synchCreationStart = new CountDownLatch(totalComponents);
	List<Future<Boolean>> results = new ArrayList<Future<Boolean>>();
	//Components must be multiple of the threads
	try{

	    for (int i = 0; i < nThreads; i++) {
	        results.add(pool.submit(new ComponentGetter(totalComponents/nThreads, totalContainers)));
	    }
	    pool.shutdown();
	    int n = 0;
	    while(!pool.isTerminated()){
		n++;
		Thread.sleep(60 * 1000);
		m_logger.info("Waiting 60 seconds more. Total now is "+ n + " minutes.");
	    } 
	    //if(!pool.awaitTermination(60, TimeUnit.SECONDS)){
	    //m_logger.info("The threads haven't finished yet, after 60 seconds.");
	    //}
	    for (Future<Boolean> future : results) {
		try {
		    Boolean threadResult = future.get();
		    if(!threadResult)
		   	m_logger.info("The thead couldn't get a reference");
		   m_logger.info("CARLI b="+threadResult);
		    assertTrue(threadResult); 
		} 
		catch (ExecutionException ex) {
			m_logger.info("Unexpected exception 1"+ ex.getCause().toString());
		}
		catch (Throwable thr) {
			m_logger.info("Unexpected exception 2"+ thr.toString());
		}
	    }
	}catch(InterruptedException e){
	    assertTrue(false);
	    m_logger.info("A timeout occurs when waiting the tasks to finish execution" + e);
	}
	finally {

	    try{
	    Thread.sleep(60* 1000);//sleep 1 minute
	    }catch(InterruptedException e){}

	    //release all components
	    for(int i=0; i < totalComponents;i++){
	    	getContainerServices().releaseComponent(compNameHeader+i);
	    }
	    assertTrue(false);
	}
    }

}

