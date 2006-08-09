/*
 * Created on Jun 26, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.jms;

import java.io.Serializable;

import javax.jms.JMSException;
import javax.jms.Topic;

/**
 * @author kzagar
 * 
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */

public class ACSJMSTopic implements Topic, Serializable {

    /**
     * 
     * @uml.property name="topicName"
     * @uml.associationEnd inverse=":java.lang.String" multiplicity="(0 1)"
     */
    private String topicName;

    public ACSJMSTopic(String topicName) {
        this.topicName = topicName;
    }

    /**
     * 
     * @uml.property name="topicName"
     */
    /* (non-Javadoc)
     * @see javax.jms.Topic#getTopicName()
     */
    public String getTopicName() throws JMSException {
        return this.topicName;
    }

}