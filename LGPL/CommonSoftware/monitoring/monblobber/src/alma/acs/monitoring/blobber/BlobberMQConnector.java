package alma.acs.monitoring.blobber;

import java.lang.InterruptedException;
import java.lang.reflect.Constructor;

import java.util.logging.Logger;
import java.util.concurrent.LinkedBlockingQueue;

import org.apache.activemq.ActiveMQConnectionFactory;
import javax.jms.JMSException;
import javax.jms.MapMessage;
import javax.jms.Session;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;

import alma.acs.container.ContainerServices;
import alma.acs.monitoring.blobber.CollectorList.BlobData;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntCreateObjectEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;

public class BlobberMQConnector implements Runnable {

    private ContainerServices myContainerServices;
    protected Logger myLogger;

    private String location;
    private String broker_url;
    private ActiveMQConnectionFactory factory;
    private TopicConnection topicConnection;
    private TopicSession topicSession;
    private TopicPublisher topicPublisher;
    private Topic topic;
    private MapMessage message;
    private boolean mqEnabled = false;
    private BlobberPlugin blobberPlugin = null;

    private LinkedBlockingQueue<BlobData> myBlobDataQueue;
    private int insertCounter = 0;

    public BlobberMQConnector(ContainerServices inContainerServices, 
			      LinkedBlockingQueue<BlobData> inBlobDataQueue) {
        this.myContainerServices = inContainerServices;
	    this.myBlobDataQueue = inBlobDataQueue;
        this.myLogger = myContainerServices.getLogger();

        try {
            blobberPlugin = createBlobberPlugin();
        } catch (AcsJCouldntCreateObjectEx ex) {
            try {
                myContainerServices.raiseAlarm("Monitoring", "MonitorArchiver", 2);
            } catch (AcsJContainerServicesEx ex1) {
                myLogger.severe("Blobber initialization failed and alarm could not be raised.");
                // fall through to ComponentLifecycleException
            }
        }

        mqEnabled = blobberPlugin.isBrokerEnabled();

        if (mqEnabled) {
            myLogger.info("About to enable activeMQ ...");
            location = System.getenv("LOCATION");
            if (location == null) {
                location = "GENERIC";
            }
            broker_url = blobberPlugin.getBrokerURL();

            try {
                myLogger.info("Starting JMS connection mqEnabled=" + mqEnabled);
                factory = new ActiveMQConnectionFactory(broker_url);
                topicConnection = factory.createTopicConnection();
                topicConnection.start();
                topicSession = topicConnection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
                topic = topicSession.createTopic("tmc");
                topicPublisher = topicSession.createPublisher(topic);
            } catch (JMSException jme) {
                myLogger.severe("No connection could be established with JMS provider. Exception: " + jme.getMessage());
                mqEnabled = false;
            }
        } else {
            myLogger.info("activeMQ disabled");
        }

    }

    protected BlobberPlugin createBlobberPlugin() throws AcsJCouldntCreateObjectEx {
        try {
            Class<? extends BlobberPlugin> pluginClass = Class.forName("alma.acs.monitoring.blobber.BlobberPluginAlmaImpl").asSubclass(BlobberPlugin.class);
            Constructor<? extends BlobberPlugin> ctor = pluginClass.getConstructor(Logger.class);
            return ctor.newInstance(myLogger);
        } catch (Exception ex) {
            AcsJCouldntCreateObjectEx ex2 = new AcsJCouldntCreateObjectEx(ex);
            throw ex2;
        }
    }

    @Override
    public void run() {
        this.myLogger.info("Starting MQ connector thread.");
        Thread.currentThread().setName("BlobberMQConnectorThread");

	while(true){
	    BlobData tempBlobData = new BlobData();
	    try {
		tempBlobData = this.myBlobDataQueue.take();
	    } catch (InterruptedException ex) {
		ex.printStackTrace();
	    }
	    if (mqEnabled) {
		publishNewMonitorData(tempBlobData);
	    }

	    try {
		// this.myMonitorDAO.store(tempBlobData);
	    } catch (Exception ex) {
		ex.printStackTrace();
	    }
	}
    }

    /**
     * Sends data over JMS to the TMCDumper, as a workaround for currently existing problems with the oramon archive.
     * <p> 
     * Really needed are <code>inData.startTime</code>, <code>inData.componentName</code>, 
     * <code>inData.clob</code>, and <code>monitorCharacteristicIDs.getMonitorPointName()</code>.
     * <p>
     * @TODO Find a "cheap" way of getting the monitor point name, so that we do not need to create and pass 
     * the full MonitorCharacteristicIDs object with all autoconfiguration steps.
     * <p>
     * The receiver side code is in AIV/TMCS/TMCDumper/src/archive/tmcdb/monitoring/TMCDumper/TMCEventConsumer.java.
     */
    private void publishNewMonitorData(BlobData inData) {
        myLogger.fine("publishNewMonitorData Called: CompName:" + inData.componentName + ", propName: " + inData.propertyName + ", index: " + inData.index);
        try {
            message = topicSession.createMapMessage();
            message.setString("serialNumber", inData.serialNumber);
            message.setString("componentName", inData.componentName);
            message.setString("clob", inData.clob);
            message.setInt("index", inData.index);
            message.setString("propertyName", inData.propertyName);
            message.setLong("sampleSize", inData.sampleSize);
            message.setLong("startTime", inData.startTime);
            message.setLong("endTime", inData.stopTime);
            message.setLong("monitorPointId", inData.monitorPointId);
            if (inData.statistics != null) {
                message.setDouble("maxStat", inData.statistics.max.doubleValue());
                message.setDouble("minStat", inData.statistics.min.doubleValue());
                message.setDouble("meanStat", inData.statistics.mean.doubleValue());
                message.setDouble("stdDevStat", inData.statistics.stdDev.doubleValue());
            }
            message.setString("location", location);

            // unwind composed baci properties, to get the underlying monitor points (if any)
            message.setString("monitorPointName", inData.monitorPointName);

            topicPublisher.publish(message);
            myLogger.fine(">>>>>>>>>>>>>>>> message sent to JMS:" + message.getString("componentName") + "/" + message.getString("monitorPointName"));
        } catch (JMSException jme) {
            myLogger.severe("No data was published. An exception was caught:" + jme.getMessage());
        } catch (NullPointerException ex) {
            myLogger.severe("NULL POINTER EXCEPTION WHILE PREPARING MESSAGE TO MQ " + ex.getMessage());
            ex.printStackTrace();
        }
    }
}
