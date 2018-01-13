package tmcdb.test;

import alma.ACS.doubleSeqHolder;
import alma.ACS.longSeqHolder;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.DataAccessSupport;
import alma.ACSErr.CompletionHolder;
import alma.acs.exceptions.AcsJException;

public class MCTestDataAccessSeq<T> extends DataAccessSupport<T> implements
		DataAccess<T> {

	private T value;
	private long timestamp;
	
	
	public MCTestDataAccessSeq(T value, long timestamp) {
		this.value = value;
		this.timestamp = timestamp;
	}
	
	@Override
	public T get(CompletionHolder completionHolder) throws AcsJException {
		if (value instanceof double[]) {
			double[] double_array = (double[]) value;
			for (int i = 0; i < double_array.length; i++) {
				double_array[i]++;
			}
		} else if (value instanceof int[]) {
			int[] int_array = (int[]) value;
			for (int i = 0; i < int_array.length; i++) {
				int_array[i]++;
			}
		}
		timestamp += 10000000; //1 sec
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
