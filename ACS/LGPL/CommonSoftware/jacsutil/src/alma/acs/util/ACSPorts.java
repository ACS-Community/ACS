/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2002 Copyright by ESO (in the framework of the ALMA
 * collaboration), All rights reserved
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package alma.acs.util;

import java.net.InetAddress;
import java.util.Vector;

/**
 * Used to figure out the dynamic ports ACS is running under.
 * <p>
 * 
 * <b>Caution</b><br>
 * Be careful with the method names: <ul>
 * 
 * <li> getXXX() is static
 * <li> giveXXX() is instance
 * 
 * </ul></p>
 * 
 * @author dfugate October 15, 2003
 */
public class ACSPorts {
   //   /**
   //    * Testing purposes only!
   //    *
   //    * @param args <B>Not used!</B>
   //    */
   //   public static void main(String[] args) {
   //      System.out.println(getManagerPort());
   //      System.out.println(getCDBPort());
   //	 System.out.println(getIP());
   //      System.out.println("done...");
   //   }

   /**
    * Name of environment variable defining base port.
    */
   public static final String ACS_BASE_PORT_VARIABLE = "ACS.baseport";

   /**
    * Default base port (integer ranging from 0-9).
    */
   public static final String ACS_BASE_PORT_DEFAULT = "0";


   //
   // ======================== STATIC ============================
   //

   // -------- Public API ----------

   /**
    * @return the "ACS Base Port". This is just an integer ranging from 0-9.
    */
   public static int getBasePort() {
      return globalInstanceForSystemProperty().giveBasePort();
   }

   /**
    * @return the port manager is running on.
    */
   public static String getManagerPort() {
      return globalInstanceForSystemProperty().giveManagerPort();
   }

   /**
    * @return the port the CORBA Naming Service is running on.
    */
   public static String getNamingServicePort() {
      return globalInstanceForSystemProperty().giveNamingServicePort();
   }

   /**
    * @return the port the CORBA Notification Service is running on.
    */
   public static String getNotifyServicePort() {
      return globalInstanceForSystemProperty().giveNotifyServicePort();
   }

   /**
    * @return the port the CORBA Alarm Notification Service is running on.
    */
   public static String getAlarmNotifyServicePort() {
      return globalInstanceForSystemProperty().giveAlarmNotifyServicePort();
   }

   /**
    * @return the port the CORBA Logging Service is running on.
    */
   public static String getLoggingServicePort() {
      return globalInstanceForSystemProperty().giveLoggingServicePort();
   }

   /**
    * @return the port the CORBA Interface Repository is running on.
    */
   public static String getIRPort() {
      return globalInstanceForSystemProperty().giveIRPort();
   }

   /**
    * @return the port the ACS Logging Service is running on.
    */
   public static String getLogPort() {
      return globalInstanceForSystemProperty().giveLogPort();
   }

   /**
    * @return the port the ACS CDB is running on.
    */
   public static String getCDBPort() {
      return globalInstanceForSystemProperty().giveCDBPort();
   }
   
   /**
    * @return the port the ACS alarm service is running on.
    */
   public static String getAlarmServicePort() {
      return globalInstanceForSystemProperty().giveAlarmServicePort();
   }

   /**
    * @return the constant port (2970) the ACS Container daemon is running on.
    */
   public static String getContainerDaemonPort() {
      return globalInstanceForSystemProperty().giveContainerDaemonPort();
   }

   /**
    * @return the constant port (2980) the ACS Services daemon is running on.
    */
   public static String getServicesDaemonPort() {
      return globalInstanceForSystemProperty().giveServicesDaemonPort();
   }

    /**
     * @return the stringified IP or "localhost" if an error 
     * were to occur.
     */
    public static String getIP() {
	return globalInstanceForSystemProperty().giveIP();
    }

   private static Vector<ACSPorts> instances = new Vector<ACSPorts>();

   /**
    * Returns an instance of this class that is configured for the specified basePort.
    * <p>
    * In earlier versions of this class, one had to set a system property to
    * calculate the ports for different instances. </p>
    * <p>
    * Note that this will create a new instance only if necessary.</p>
    * 
    * @since v1.5
    */
   public static ACSPorts globalInstance(int basePort) {

      // could limit the allowed values by doing an "if (instanceIndex < 10)" here
      // but maybe we want 20 Acs instances at some point in the future
      if (basePort < 0)
         throw new IllegalArgumentException("invalid baseport: "+basePort);

      // fill up to needed size if necessary -
      // unfortunately Vector.ensureCapacity() doesn't do the job
      for (int i=instances.size(); i<basePort+1; i++)
         instances.add(null);

      ACSPorts instance = instances.get(basePort);
      if (instance == null) {
         instance = new ACSPorts(basePort);
         instances.set(basePort, instance);
      }

      return instance;
   }

   // -------- Internal ----------

   /**
    * @since v1.5
    */
   private static ACSPorts globalInstanceForSystemProperty() {

      int instanceIndex = 0;
      try {
         instanceIndex = Integer.parseInt(System.getProperty(ACS_BASE_PORT_VARIABLE, ACS_BASE_PORT_DEFAULT));
      } catch (NumberFormatException exc) {}

      return globalInstance(instanceIndex);
   }

   //
   // ======================== INSTANCE ============================
   //

   /**
    */
   private ACSPorts(int basePort) {
      this.basePort = basePort;
   }

   /**
    */
   int basePort;

   /**
    * @return the "ACS Base Port". This is just an integer ranging from 0-9.
    */
   public int giveBasePort() {
      return basePort;
   }

   /**
    * @return the port manager is running on.
    */
   public String giveManagerPort() {
      return String.valueOf(basePort * 100 + 3000 + 0);
   }

   /**
    * @return the port the CORBA Naming Service is running on.
    */
   public String giveNamingServicePort() {
      return String.valueOf(basePort * 100 + 3000 + 1);
   }

   /**
    * @return the port the CORBA Notification Service is running on.
    */
   public String giveNotifyServicePort() {
      return String.valueOf(basePort * 100 + 3000 + 2);
   }

   /**
    * @return the port the CORBA Logging Service is running on.
    */
   public String giveLoggingServicePort() {
      return String.valueOf(basePort * 100 + 3000 + 3);
   }

   /**
    * @return the port the CORBA Interface Repository is running on.
    */
   public String giveIRPort() {
      return String.valueOf(basePort * 100 + 3000 + 4);
   }

   /**
    * @return the port the CORBA Alarm Notification Service is running on.
    */
   public String giveAlarmNotifyServicePort() {
      return String.valueOf(basePort * 100 + 3000 + 7);
   }

   /**
    * @return the port the ACS Logging Service is running on.
    */
   public String giveLogPort() {
      return String.valueOf(basePort * 100 + 3000 + 11);
   }

   /**
    * @return the port the ACS CDB is running on.
    */
   public String giveCDBPort() {
      return String.valueOf(basePort * 100 + 3000 + 12);
   }
   
   /**
    * @return the port the ACS alarm service is running on.
    */
   public String giveAlarmServicePort() {
      return String.valueOf(basePort * 100 + 3000 + 13);
   }

   /**
    * @return the constant port (2970) the ACS Container daemon is running on.
    */
   public String giveContainerDaemonPort() {
      return String.valueOf(2970);
   }

   /**
    * @return the constant port (2980) the ACS Services daemon is running on.
    */
   public String giveServicesDaemonPort() {
      return String.valueOf(2980);
   }

   /**
     * @return this host's IP address
     */
    public String giveIP() {
	try
	    {
	    return InetAddress.getLocalHost().getHostAddress();
	    }
	catch(Exception e)
	    {
	    //on any failure just return localhost
	    return "localhost";
	    }
    }

    public String toString() {
    	return "ACSPorts@"+hashCode()+" [base "+basePort+"]";
    }

}
