package cern.laser.business;

public class LaserRuntimeException extends RuntimeException {
  public LaserRuntimeException(String msg) {
    super(msg);
  }

  public LaserRuntimeException(String msg, Throwable t) {
    super(msg, t);
  }
}