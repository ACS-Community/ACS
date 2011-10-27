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

import java.util.Date;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;

import javax.mail.Address;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import cern.laser.business.pojo.MailAndSmsServerImpl;

import alma.acs.logging.AcsLogLevel;

/**
 * ACS class to send SMS and mail.
 * 
 * The class extends MailAndSmsServerImpl CERN pojo class because in such a class there are 
 * several hard-coded fields that are different from what we use in ACS/ALMA.
 * 
 * At the present the alarm system does not send SMS so this class defines
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
	
	private final Session session;
	
	/**
	 * Constructor
	 */
	public ACSMailAndSmsServer(Logger log) {
		super();
		if (log==null) {
			throw new IllegalArgumentException("The logger can't be null");
		}
		logger=log;
		
		Properties props=System.getProperties();
		if (!props.containsKey("mail.smtp.host")) {
			props.put("mail.smtp.host", "smtp.alma.cl");
		}
		if (!props.containsKey("mail.from")) {
			props.put("mail.from", "software@alma.cl");
		}
	    session = Session.getInstance(props, null);
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
	public void sendEmail(final String address, final String subject, final String text) {
		if (address==null || address.isEmpty()) {
			logger.log(AcsLogLevel.WARNING, "Sending EMAIL aborted: no valid dest. address");
		}
		logger.log(AcsLogLevel.DEBUG, "Sending EMAIL to "+address+", subject="+subject);
		Thread MailerThread = new Thread(new Runnable() {
			public void run() {
				try {
			        MimeMessage msg = new MimeMessage(session);
			        msg.setFrom();
			        if (address.contains(";")) {
			        	String[] addresses=address.split(";");
			        	Vector<Address> addressesVector = new Vector<Address>();
			        	for (String toAddress: addresses) {
			        		if (!toAddress.trim().isEmpty()) {
			        			addressesVector.add(new InternetAddress(toAddress.trim()));
			        		}
			        	}
			        	Address[] addrrs = new Address[addressesVector.size()];
			        	addressesVector.toArray(addrrs);
			        	msg.setRecipients(Message.RecipientType.TO,addrrs);
			        } else {
			        	msg.setRecipients(Message.RecipientType.TO,address);
			        }
			        msg.setSubject(subject);
			        msg.setSentDate(new Date());
			        msg.setText(text);
			        Transport.send(msg);
			        logger.log(AcsLogLevel.DEBUG, "Email sent");
			    } catch (MessagingException mex) {
			        logger.log(AcsLogLevel.ERROR,"Error sending email "+mex.toString());
			    }
			}
		},"MailerThread_"+System.currentTimeMillis());
		MailerThread.setDaemon(true);
		MailerThread.start();
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
