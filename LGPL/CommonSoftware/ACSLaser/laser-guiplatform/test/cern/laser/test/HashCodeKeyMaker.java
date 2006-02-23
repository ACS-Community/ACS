/*
 * HashCodeKeyMaked.java
 *
 * Created on February 3, 2004, 5:35 PM
 */

package cern.laser.test;


import java.net.InetAddress;
import java.security.SecureRandom;

/**
 *
 * This class is used to generate primary keys as hash code
 * and to generate UUID for EJB
 *
 * @author  Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class HashCodeKeyMaker {
    
    //private SecureRandom seeder;
    //private String midValue;
  
    /** Creates a new instance of HashCodeKeyMaked */
    //public HashCodeKeyMaker() {
    //}
   
    /**
     * @param key
     */
    public static int makeKey(String key) {
       return key.hashCode();
    } 
   
    /**
     * Generates UUID for EJB's
     * 
     * @return UUID
     */
    public static int makeUUID() throws Exception {
        // init seeder nad midValue
        try {
            InetAddress inet = InetAddress.getLocalHost();
            byte [] bytes = inet.getAddress();
            String hexInetAddress = hexFormat(getInt(bytes),8);
            //String thisHashCode = hexFormat(System.iidentityHashCode(this),8);
            String thisHashCode = hexFormat(System.identityHashCode(new Object()),8);
            String midValue = hexInetAddress + thisHashCode;
            SecureRandom seeder = new SecureRandom();
            int node = seeder.nextInt();

            // generate UUID
            long timeNow = System.currentTimeMillis();
            // get int value as unsigned    
            int timeLow = (int) timeNow & 0xFFFFFFFF;
            // get next random value
            node = seeder.nextInt();
            String uuid = hexFormat(timeLow, 8) + midValue + hexFormat(node, 8);

            return uuid.hashCode();  
        } catch (java.net.UnknownHostException e) {
            throw new Exception(e);
        }
    }
     
    private static int getInt(byte bytes[]) {
        int i = 0;
        int j = 24;
        for (int k = 0; j >= 0; k++) {
          int l = bytes[k] & 0xff;
          i += l << j;
          j -= 8;
        }
        return i;
    }

    private static String hexFormat(int i, int j) {
        String s = Integer.toHexString(i);
        return padHex(s, j) + s;
    }
  
    private static String padHex(String s, int i) {
        StringBuffer tmpBuffer = new StringBuffer();
        if (s.length() < i) {
            for (int j = 0; j < i - s.length(); j++) {
                tmpBuffer.append('0');
            }
        }
        return tmpBuffer.toString();
    }
    
    
    public static void main(String args[]) throws Exception {
        System.out.println(HashCodeKeyMaker.makeKey("CERN"));  
        System.out.println(HashCodeKeyMaker.makeKey("CERN.SURVEILLANCE"));  
       
        System.out.println("UUID's:"); 
        System.out.println(HashCodeKeyMaker.makeUUID());  
        System.out.println(HashCodeKeyMaker.makeUUID());  
        System.out.println(HashCodeKeyMaker.makeUUID());  
        System.out.println(HashCodeKeyMaker.makeUUID());  
        System.out.println(HashCodeKeyMaker.makeUUID());  
        System.out.println(HashCodeKeyMaker.makeUUID());  
        
        
    }
    
}
