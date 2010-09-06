package alma.acs.eventbrowser.views;

/**
 * $Author: jschwarz $
 * $Date: 2010/09/06 11:02:18 $
 * $Id: AbstractEventData.java,v 1.1 2010/09/06 11:02:18 jschwarz Exp $
 */

public abstract class AbstractEventData implements java.io.Serializable {

	private static final long serialVersionUID = -8199180183031190929L;

	protected Long timestamp;

	public AbstractEventData() {
		super();
	}

	public Long getTimestamp() {
		return timestamp;
	}
	

}