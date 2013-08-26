package cern.cmw.mom.test;



class SerializableByteArray implements java.io.Serializable {

  private Byte aByteArray[] = null;

  /**
   * Constructor SerializableByteArray
   *
   *
   * @param objectSize
   *
   */
  public SerializableByteArray(int objectSize) {
    aByteArray = new Byte[objectSize];
  }
}


/*--- Formatted in Sun Java Convention Style on Mon, Feb 12, '01 ---*/


/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
