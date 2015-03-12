/*
 * XMLMessageHelper.java
 *
 * Created on February 27, 2003, 3:01 PM
 */
package cern.laser.source.alarmsysteminterface.impl;

import java.io.BufferedReader;
import java.io.StringReader;
import java.io.StringWriter;

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;


/**
 * Helper class for marshaling/unmarshaling to/from ASIMessage instances and XML descriptions.
 * @author  fracalde
 */
public class XMLMessageHelper {
  /** Creates a new instance of XMLMessageHelper */
  private XMLMessageHelper() {
  }

  /**
   * DOCUMENT ME!
   *
   * @param asiMessage DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   *
   * @throws Exception DOCUMENT ME!
   */
  public static String marshal(ASIMessage asiMessage) throws Exception {
    StringWriter writer = new StringWriter();
    Marshaller marshaller = new Marshaller(writer);
    marshaller.setSuppressNamespaces(true);
    marshaller.marshal(asiMessage);

    return writer.toString();
  }

  /** Unmarshal the XML representation into an ASIMessage.
   * @param xmlMessage The XML description.
   * @return The ASIMessage instance
   * @throws Exception If unmarshaling fails.
   */
  public static ASIMessage unmarshal(String xmlMessage) throws Exception {
    ASIMessage asi_message = (ASIMessage) Unmarshaller.unmarshal(ASIMessage.class, new BufferedReader(new StringReader(xmlMessage)));

    return asi_message;
  }
}
