/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
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