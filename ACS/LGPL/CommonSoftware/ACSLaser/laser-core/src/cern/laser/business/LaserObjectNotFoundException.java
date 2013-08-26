package cern.laser.business;

public class LaserObjectNotFoundException extends RuntimeException {
  public LaserObjectNotFoundException(String msg) {
    super(msg);
  }

  public LaserObjectNotFoundException(String msg, Throwable t) {
    super(msg, t);
  }
}