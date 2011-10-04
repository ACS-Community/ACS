package alma.acs.profiling.orb;

public class ProfilerMessage implements Comparable<ProfilerMessage>
{
	ProfilerMessage(Type type) {
		this.type = type;
	}
	enum Type {CONNECTION_POOL, REQUEST_QUEUE, THREAD_POOL, REQUEST_STARTED, REQUEST_FINISHED}
	final Type type;
	long timestamp;
	int requestId;
	String poaName;
	// todo fields for pool/queue sizes
	String operation;
	long timeElapsedMillis; // only for REQUEST_FINISHED
	
	
	/** 
	 * Currently not consistent with equals! 
	 */
	@Override
	public int compareTo(ProfilerMessage other) {
		if (this.timestamp < other.timestamp) return -1;
		if (this.timestamp > other.timestamp) return 1;
		
		// important to have REQUEST_STARTED before REQUEST_FINISHED, even with same timestamp
		int comp = this.type.compareTo(other.type);
		if (comp != 0) return comp;
		
		if (this.operation != null) {
			comp = this.operation.compareTo(other.operation);
			if (comp != 0) return comp;
		}
		
		if (this.requestId < other.requestId) return -1;
		if (this.requestId > other.requestId) return 1;
		
		if (this.poaName != null) {
			comp = this.poaName.compareTo(other.operation);
			if (comp != 0) return comp;
		}
		
		return 0;
	}

}
