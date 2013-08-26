package cern.cmw.mom.test.chat;

import cern.cmw.mom.pubsub.MOMException;
import cern.cmw.mom.pubsub.PubSubFactory;
import cern.cmw.mom.pubsub.Publisher;
import cern.cmw.mom.pubsub.Subscriber;
import cern.cmw.mom.pubsub.SubscriptionListener;
import cern.cmw.mom.util.TopicAdminHelper;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.TextMessage;

import javax.naming.NamingException;

import javax.swing.UIManager;

import org.apache.log4j.Category;


/**
 * Chat application implemented via the CMW pubsub API.
 */
public class ChatClient implements SubscriptionListener {
    final static String CHAT_TOPIC = "CMW.TEMP.CHAT";
    final static Category cat = Category.getInstance(ChatClient.class.getName());
    boolean packFrame = false;
    ChatFrame1 frame = null;
    boolean connected = false;
    Subscriber sub = null;
    Publisher pub = null;
    long subscriptionToken = 0;

    //Construct the application
    public ChatClient() {
        frame = new ChatFrame1();

        //Validate frames that have preset sizes
        //Pack frames that have useful preferred size info, e.g. from their layout
        if (packFrame) {
            frame.pack();
        } else {
            frame.validate();
        }

        //Center the window
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension frameSize = frame.getSize();

        if (frameSize.height > screenSize.height) {
            frameSize.height = screenSize.height;
        }

        if (frameSize.width > screenSize.width) {
            frameSize.width = screenSize.width;
        }

        frame.setLocation((screenSize.width - frameSize.width) / 2,
            (screenSize.height - frameSize.height) / 2);
        frame.setVisible(true);

        frame.jButton1.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    connect(e);
                }
            });

        frame.jButton2.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    disconnect(e);
                }
            });

        frame.jButton3.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    post(e);
                }
            });
    }

    void connect(ActionEvent e) {
        if (!connected) {
            frame.statusBar.setText("Connecting, please wait...");

            try {
                pub = PubSubFactory.publisher();
                sub = PubSubFactory.subscriber();
                subscriptionToken = sub.subscribe(CHAT_TOPIC, this, null);
                connected = true;
                frame.statusBar.setText("Connected.");
                cat.info("connected");
                frame.jButton1.setEnabled(false);
                frame.jButton2.setEnabled(true);
                frame.jButton3.setEnabled(true);
                frame.jTextArea1.setText("Welcome to the CMW chat !!");
            } catch (MOMException me) {
                frame.statusBar.setText(me.getMessage());
                cat.error(me.getMessage());
                me.printStackTrace();
            } catch (JMSException je) {
                frame.statusBar.setText(je.getMessage());
                cat.error(je.getMessage());
                je.printStackTrace();
            } catch (NamingException ne) {
                frame.statusBar.setText(ne.getMessage());
                cat.error(ne.getMessage());
                ne.printStackTrace();
            }
        }
    }

    void disconnect(ActionEvent e) {
        if (connected) {
            frame.statusBar.setText("Disconnecting, please wait...");
            cat.info("Disconnecting...");
            pub.close();

            try {
                sub.unSubscribe(subscriptionToken);
            } catch (JMSException je) {
                cat.error(je.getMessage());
                je.printStackTrace();
            }

            sub.close();
            connected = false;
            frame.statusBar.setText("Not connected.");
            frame.jButton1.setEnabled(true);
            frame.jButton2.setEnabled(false);
            frame.jButton3.setEnabled(false);
        }
    }

    void post(ActionEvent e) {
        if (connected) {
            cat.info("Posting message " + frame.jTextField1.getText() + "...");

            try {
                TextMessage tm = pub.createTextMessage();
                tm.setText(frame.jTextField1.getText());
                pub.publish(CHAT_TOPIC, tm);
                frame.jTextField1.setText("");
            } catch (JMSException je) {
                frame.statusBar.setText(je.getMessage());
                cat.error(je.getMessage());
                je.printStackTrace();
            } catch (NamingException ne) {
                frame.statusBar.setText(ne.getMessage());
                cat.error(ne.getMessage());
                ne.printStackTrace();
            }
        }
    }

    public void onMessage(Message m) {
        TextMessage tm = (TextMessage) m;

        try {
            frame.jTextArea1.append("\n" + tm.getText());
        } catch (JMSException je) {
            je.printStackTrace();
        }
    }

    //Main method
    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) {
            cat.error(e.getMessage());
            e.printStackTrace();
        }

        new ChatClient();
    }
}
