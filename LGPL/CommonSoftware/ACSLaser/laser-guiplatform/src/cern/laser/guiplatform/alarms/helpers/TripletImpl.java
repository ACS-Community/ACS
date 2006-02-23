package cern.laser.guiplatform.alarms.helpers;
import cern.laser.client.data.Triplet;

public class TripletImpl implements Triplet
{
  private String faultFamily;
  private String faultMember;
  private Integer faultCode;

  //public TripletImpl(cern.laser.business.data.Triplet triplet)
  public TripletImpl(String faultFamily, String faultMember, int faultCode)
  {
    this.faultFamily = faultFamily;
    this.faultMember = faultMember;
    this.faultCode = new Integer(faultCode);
    /*
    if (triplet == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    faultFamily = triplet.getFaultFamily();
    faultMember = triplet.getFaultMember();
    faultCode = triplet.getFaultCode();
    */
  }

  public TripletImpl(Triplet triplet)
  {
    if (triplet == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    faultFamily = new String(triplet.getFaultFamily());
    faultMember = new String(triplet.getFaultMember());
    faultCode = triplet.getFaultCode();
  }

  public String getFaultFamily()
  {
    return faultFamily;
  }


  public String getFaultMember()
  {
    return faultMember;
  }

  public Integer getFaultCode()
  {
    return faultCode;
  }

  public int hashCode() 
  {
    return this.toString().hashCode();
  }

  public Object clone() throws CloneNotSupportedException 
  {
    return new TripletImpl(this);
  }  
  
  public String toString() 
  {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("[");
    str_buf.append(getFaultFamily());
    str_buf.append("][");
    str_buf.append(getFaultMember());
    str_buf.append("][");
    str_buf.append(getFaultCode());
    str_buf.append("]");

    return str_buf.toString();
  }
}
