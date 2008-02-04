/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *                 and Cosylab
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *    MA 02111-1307  USA
 */
package alma.alarmsystem.core.mail;

import java.util.logging.Logger;

import alma.acs.logging.AcsLogLevel;

import cern.laser.business.pojo.MailAndSmsServerImpl;

/**
 * ACS class to send SMS and mail.
 * 
 * The class extends MailAndSmsServerImpl CERN pojo class because in such a class there are 
 * several hard-coded fields that are different from what we use in ACS/ALMA.
 * 
 * At the present the alarm system does not send emails neither SMS so this class defines
 * stub routines to be filled in when we'll decide we need them.
 * 
 * @see cern.laser.business.pojo.MailAndSmsServerImpl
 * 
 * @author acaproni
 *
 */
public class ACSMailAndSmsServer extends MailAndSmsServerImpl {
	
	// ACS loger
	private Logger logger;
	
	/**
	 * Constructor
	 */
	public ACSMailAndSmsServer(Logger log) {
		super();
		if (log==null) {
			throw new IllegalArgumentException("The logger can't be null");
		}
		logger=log;
	}
	
	/**
	 * Send and email.
	 * 
	 * @param address The address to send the email to
	 * @param subject The subject of the email
	 * @param text The text of the email
	 * 
	 * @see cern.laser.business.pojo.MailAndSmsServerImpl
	 */
	public void sendEmail(String address, String subject, String text) {
		logger.log(AcsLogLevel.WARNING, "Sending od EMAILS disabled: no email will be sent to "+address);
	}
	
	/**
	 * Send an SMS
	 * 
	 * @param number The gsm number to send the SMS to
	 * @param text The text of the SMS to send
	 * 
	 * @see cern.laser.business.pojo.MailAndSmsServerImpl
	 */
	public void sendSMS(String number, String text) {
		logger.log(AcsLogLevel.WARNING, "Sending of SMS disabled: no SMS will be sent to "+number);
	}

}
