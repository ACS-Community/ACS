package cern.laser.business;

public class LaserCreateException extends RuntimeException {
  public LaserCreateException(String msg) {
    super(msg);
  }

  public LaserCreateException(String msg, Throwable t) {
    super(msg, t);
  }
}