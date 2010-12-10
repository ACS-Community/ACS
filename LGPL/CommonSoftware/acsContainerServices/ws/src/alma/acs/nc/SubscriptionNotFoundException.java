package alma.acs.nc;

public class SubscriptionNotFoundException extends Exception {

	private static final long serialVersionUID = 2150706790151735359L;

	public SubscriptionNotFoundException() {
		super();
	}

	public SubscriptionNotFoundException(String message) {
		super(message);
	}

	public SubscriptionNotFoundException(Throwable cause) {
		super(cause);
	}

	public SubscriptionNotFoundException(String message, Throwable cause) {
		super(message, cause);
	}

}
