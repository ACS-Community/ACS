package cern.laser.util;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.SecureRandom;

public class UUIDGenerator  {
  private  SecureRandom seeder;
  private  String midValue;

  private static UUIDGenerator generator = null;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  private UUIDGenerator() throws UnknownHostException {
      InetAddress inet = InetAddress.getLocalHost();
      byte[] bytes = inet.getAddress();
      String hexInetAddress = hexFormat(getInt(bytes), 8);
      String thisHashCode = hexFormat(System.identityHashCode(this), 8);
      midValue = hexInetAddress + thisHashCode;
      seeder = new SecureRandom();
      int node = seeder.nextInt();
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //
  
  public static final UUIDGenerator getInstance() throws UnknownHostException {
    if (generator == null) {
      generator = new UUIDGenerator();
    }
    
    return generator;
  }

  public Integer getUUID() {
    long timeNow = System.currentTimeMillis();
    // get int value as unsigned
    int timeLow = (int) timeNow & 0xFFFFFFFF;
    // get next random value
    int node = seeder.nextInt();
    String uuid = hexFormat(timeLow, 8) + midValue + hexFormat(node, 8);

    return new Integer(uuid.hashCode());
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  private int getInt(byte bytes[]) {
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
}