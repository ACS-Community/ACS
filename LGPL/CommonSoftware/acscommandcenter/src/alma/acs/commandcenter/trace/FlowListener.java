
package alma.acs.commandcenter.trace;

/**
 * Use this to implement a view (graphical or other) for a flow.
 */
public interface FlowListener {

	public void reset(Flow f, Object info);

	public void trying(Flow f, String step);

	public void success(Flow f, String name);
	
	public void failure(Flow f, String name, Object info);

	public void completion(Flow f);
   
}
