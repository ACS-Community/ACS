package alma.acs.nc;

public class CannotAddSubscriptionException extends Exception {

	private static final long serialVersionUID = -5578631057986876356L;

	public CannotAddSubscriptionException() {
		super();
	}

	public CannotAddSubscriptionException(String message) {
		super(message);
	}

	public CannotAddSubscriptionException(Throwable cause) {
		super(cause);
	}

	public CannotAddSubscriptionException(String message, Throwable cause) {
		super(message, cause);
	}

}
