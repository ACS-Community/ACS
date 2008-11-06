package alma.acs.nc;

import org.omg.CosEventChannelAdmin.EventChannel;
import org.omg.CosNaming.Binding;
import org.omg.CosNaming.BindingIteratorHolder;
import org.omg.CosNaming.BindingListHolder;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.exceptions.AcsJException;
import alma.acscommon.NC_KIND;
import alma.acs.container.AdvancedContainerServices;
import alma.acs.container.ContainerServicesBase;
import gov.sandia.NotifyMonitoringExt.NameAlreadyUsed;
import gov.sandia.NotifyMonitoringExt.NameMapError;

import java.util.Random;

public class HelperTest extends ComponentClientTestCase
{
        private Helper helper;
        //public HelperExtTest helperExtTest;

        public HelperTest() throws Exception {
                super("HelperTest");
        }

        protected void setUp() throws Exception {
                super.setUp();
                //helper = new Helper(getContainerServices());
                helper = new HelperExtTest(getContainerServices());
        }

        protected void tearDown() throws Exception {
                super.tearDown();
        }

        public void xxxtestNamingServiceBindings() {
                BindingListHolder blh = new BindingListHolder();
                try{
                helper.getNamingService().list(100, blh, new BindingIteratorHolder());
                }catch(Exception e){System.out.println(e);}
                for (Binding binding : blh.value) {
                        System.out.println(binding.binding_name[0].id);
                }
        }

        public void testCreateChannel() throws Exception {
                getNotificationChannel("singleChannel");
        }

        public void testCreateTwiceSameChannel() throws Exception {
                String channelName = "anotherSingleChannel"; //one channel tried to be created multiple times
                 
                ChannelCreator channelCreator1 = new ChannelCreator(helper, channelName);
                ChannelCreator channelCreator2 = new ChannelCreator(helper, channelName);
                channelCreator1.start();
                channelCreator2.start();
                Thread.sleep(10000);
                //check how many times was succed on the creation
                assertEquals(1, channelCreator1.count);
                assertEquals(0, channelCreator2.count);
                assertEquals("", channelCreator1.exc);
                assertEquals("NameAlreadyUsed", channelCreator2.exc);
                channelCreator1.stop();
                channelCreator2.stop();
        }

        /////////////////////////////
        //// Test helper methods
        /////////////////////////////

        public EventChannel getNotificationChannel(String channelName) throws AcsJException {
                String factoryName = helper.getNotificationFactoryNameForChannel(channelName);
                
                EventChannel swa =  helper.getNotificationChannel(channelName, NC_KIND.value, factoryName);
                return swa;
            }
}

class HelperExtTest extends Helper{
    public HelperExtTest(ContainerServicesBase services) throws AcsJException {
        super(services);
    }
   public EventChannel createChannel(String channelName)throws AcsJException, NameAlreadyUsed, NameMapError{
           return createNotificationChannel(channelName, NC_KIND.value, getNotificationFactoryNameForChannel(channelName)); 
    } 

}

class ChannelCreator extends Thread{

    public int count = 0;
    //public statiint runThread = 0;
    public HelperExtTest ht;
    public int sleep = 0;
    public String exc = "";
    String channelName;

    public ChannelCreator(Helper ht, String channelName){
        this.channelName = channelName;
        if(ht instanceof HelperExtTest)
        this.ht = (HelperExtTest)ht;
    }
    
    public void run(){
        try{
        //Thread.sleep(sleep);
        
        System.out.println("CARLI - running 1");    
        ht.createChannel(channelName);
    
        System.out.println("CARLI - running 2");    
        count++;
        }catch (NameMapError e){
            exc = "NameMapError";
        }catch (NameAlreadyUsed e){
            exc = "NameAlreadyUsed";
        }catch (Exception e){
            exc = "Unknown";
        }
    }
}
