package tmcdb.test;

import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.DataAccessSupport;
import alma.ACSErr.CompletionHolder;
import alma.acs.exceptions.AcsJException;
import alma.acs.time.TimeHelper;

public class MCTestDataAccess<T extends Number> extends DataAccessSupport<T> implements DataAccess<T> {

	private long timestamp = 0L;
	private T value;
	
	public MCTestDataAccess (T value, long timstamp) {
		this.value = value;
		this.timestamp = timstamp;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public T get(CompletionHolder completionHolder) throws AcsJException {
		timestamp += 10000000; //increment 1 sec
		if (value instanceof Double) {
			value = (T)Double.valueOf(value.doubleValue() + 1);
		} else if (value instanceof Float) {
			value = (T)Float.valueOf(value.floatValue() + 1);
		} else if (value instanceof Long) {
			value = (T)Long.valueOf(value.longValue() + 1);
		} else if (value instanceof Integer) {
			value = (T)Integer.valueOf(value.intValue() + 1);
		}
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
	public void set(T value, CompletionHolder completion)
			throws AcsJException {
		timestamp = TimeHelper.getTimeStamp().value;
		this.value = value;
	}

}
