package cern.cmw.mom.test;



import java.awt.*;
import java.awt.event.*;

import java.applet.*;

import javax.swing.*;

import cern.cmw.mom.pubsub.*;
import cern.cmw.mom.util.*;


/**
 * Applet version of the Client test example.
 * @see Client
 */
public class AppletClient extends JApplet implements SubscriptionListener {

    boolean                     isStandalone      = false;
    JScrollPane                 jScrollPane1      = new JScrollPane();
    JTextArea                   jTextArea1        = new JTextArea();
    private final static String TOPIC             = "CMW.TEMP.CHAT";
    private Subscriber          s                 = null;
    private long                subscriptionToken = 0;
    JButton                     jButton1          = new JButton();

    //Get a parameter value

    /**
     * Method getParameter
     *
     *
     * @param key
     * @param def
     *
     * @return String
     *
     */
    public String getParameter(String key, String def) {

        return isStandalone
               ? System.getProperty(key, def)
               : ((getParameter(key) != null)
                  ? getParameter(key)
                  : def);
    }

    /*Construct the applet
    public AppletClient() {
    }
    */

    //Initialize the applet

    /**
     * Method init
     *
     *
     */
    public void init() {

        System.setProperty(cern.cmw.mom.util.MomConfig.KEEP_ALIVE_PROPERTY, "10");
        System.out.println("Initializing...");

        try {
            jbInit();
            System.out.println("Starting!");

            try {
                s = PubSubFactory.subscriber();
            } catch (MOMException me) {
                jTextArea1.append("MOMException raised while instantiating a Subscriber\n");
                me.printStackTrace();
            }

            go();
        } catch (Exception e) {
            System.out.println("Unable to initialize!!");
            e.printStackTrace();
        }
    }

    /**
     * Method go
     *
     *
     */
    public void go() {

        try {

            // Open the subscription to TOPIC
            subscriptionToken = s.subscribe(TOPIC, this, null);
        } catch (javax.jms.JMSException je) {
            jTextArea1.append("JMSException raised while subscribing to " + TOPIC + "\n");
            je.printStackTrace();
        } catch (javax.naming.NamingException je) {
            jTextArea1.append("JMSException raised while subscribing to " + TOPIC + "\n");
            je.printStackTrace();
        }
    }

    //Component initialization

    /**
     * Method jbInit
     *
     * @throws Exception
     */
    private void jbInit() throws Exception {

        jTextArea1.setToolTipText("");
        this.setSize(new Dimension(400, 300));
        jButton1.setToolTipText("");
        jButton1.setText("exit");
        jButton1.addActionListener(new java.awt.event.ActionListener() {

            public void actionPerformed(ActionEvent e) {
                bye(e);
            }
        });
        this.getContentPane().setLayout(new BorderLayout());
        this.getContentPane().add("Center", jScrollPane1);
        this.getContentPane().add("South", jButton1);

        //this.getContentPane().add(jScrollPane1, BorderLayout.CENTER);
        //this.getContentPane().add(jButton1, BorderLayout.SOUTH);
        jScrollPane1.getViewport().add(jTextArea1, null);
    }

    //Get Applet information

    /**
     * Method getAppletInfo
     *
     *
     * @return String
     *
     */
    public String getAppletInfo() {
        return "Applet Information";
    }

    //Get parameter info

    /**
     * Method getParameterInfo
     *
     *
     * @return String[][]
     *
     */
    public String[][] getParameterInfo() {
        return null;
    }

    /**
     * Method onMessage
     *
     *
     * @param message
     *
     */
    public void onMessage(javax.jms.Message message) {

        // Subscriber implementation of the listener
        try {
            javax.jms.TextMessage msg = (javax.jms.TextMessage) message;

            jTextArea1.append("Got message  : " + msg.getText() + "\n");

            if (msg.getText().equals("bye")) {
                jTextArea1.append("Time to close!\n");
                s.unSubscribe(subscriptionToken);
                s.close();
            }
        } catch (javax.jms.JMSException je) {
            jTextArea1.append("JMSException raised while processing message: \n" + message + "\n");
            je.printStackTrace();
        }
    }

    void bye(ActionEvent e) {

        try {

            // Close the subscription and the Subscriber object
            s.unSubscribe(subscriptionToken);
        } catch (javax.jms.JMSException je) {
            jTextArea1.append("JMSException raised while unsubscribing to " + TOPIC + "\n");
            je.printStackTrace();
        }

        s.close();
    }

    /*static initializer for setting look & feel
    static {
      try {
        //UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        //UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
      }
      catch(Exception e) {
      }
    }
    */
}


/*--- Formatted in Sun Java Convention Style on Mon, Sep 24, '01 ---*/


/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
