package tmcdb.test.properties;

import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.DataAccessSupport;
import alma.ACSErr.CompletionHolder;
import alma.acs.exceptions.AcsJException;

public class MCtestDevIONoIncremental<T> extends DataAccessSupport<T> implements DataAccess<T> {

	private T value;
	private final long timestamp;
	
	public MCtestDevIONoIncremental(long timestamp, T value) {
		this.value = value;
		this.timestamp = timestamp;
	}

	@Override
	public T get(CompletionHolder completionHolder) throws AcsJException {
		return value;
	}

	@Override
	public boolean initializeValue() {
		// tnakamot: this method now returns false so that this.value won't
		//           be initialized by BACI properties which initializes the
		//           value with set(...) method, and this instance keeps the
		//           original value set in the constructor.
		return false;
	}

	@Override
	public void set(T value, CompletionHolder completion) throws AcsJException {
		this.value = value;
	}

}
