package alma.acs.nc;

public class CannotStartReceivingEventsException extends Exception {

	private static final long serialVersionUID = -5578631057986876356L;

	public CannotStartReceivingEventsException() {
		super();
	}

	public CannotStartReceivingEventsException(String message) {
		super(message);
	}

	public CannotStartReceivingEventsException(Throwable cause) {
		super(cause);
	}

	public CannotStartReceivingEventsException(String message, Throwable cause) {
		super(message, cause);
	}

}
