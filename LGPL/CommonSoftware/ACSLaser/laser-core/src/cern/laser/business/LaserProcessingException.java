package cern.laser.business;

public class LaserProcessingException extends Exception {
  public LaserProcessingException(String msg) {
    super(msg);
  }

  public LaserProcessingException(String msg, Throwable t) {
    super(msg, t);
  }
}