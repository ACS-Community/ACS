package cern.laser.business.pojo;

import java.util.Properties;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import cern.laser.business.LaserRuntimeException;

public class MailAndSmsServerImpl  {
  private Session session;
  private static final String CERN_SMTP_HOST = "smtp.cern.ch";
  private static final String FROM_LASER = "laser@cern.ch";

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  public MailAndSmsServerImpl() {
    Properties props = new Properties();
    // Setup mail server
    props.put("mail.smtp.host", CERN_SMTP_HOST);
    // Get session
    session = Session.getDefaultInstance(props, null);
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void sendEmail(String address, String subject, String text) {
    if (address == null) { throw new IllegalArgumentException("e-mail address is null"); }
    try {
      send(address, subject, text);
    } catch (Exception e) {
      throw new LaserRuntimeException("unable to send email to address " +address, e);
    }
  }

  public void sendSMS(String number, String text) {
    String address = buildSMSAddress(number);
    try {
      send(address, text, "");
    } catch (Exception e) {
      throw new LaserRuntimeException("unable to send sms to gsm number "+number, e);
    }
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //
  private void send(String address, String subject, String text) throws AddressException, MessagingException {
    MimeMessage message = new MimeMessage(session);
    // Set the from address
    message.setFrom(new InternetAddress(FROM_LASER));
    // Set the to address
    message.addRecipient(Message.RecipientType.TO, new InternetAddress(address));
    // Set the subject
    message.setSubject(subject);
    // Set the content
    message.setText(text);
    // Send message
    Transport.send(message);
  }

  private String buildSMSAddress(String number) {
    if ((number == null) || (number.length() != 6) || (!number.startsWith("16"))) { throw new IllegalArgumentException(
        "number is null or invalid"); }
    StringBuffer buffer = new StringBuffer();
    buffer.append("laser-gsm@");
    buffer.append(number);
    buffer.append(".gsm.cern.ch");

    return buffer.toString();
  }
}