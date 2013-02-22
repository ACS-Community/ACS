package alma.acs.nc;

import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;

/**
 * This class helps cheating so that we both hide the now package-private NC Consumer from other code, 
 * and at the same time still allow jcontnc test classes to use it.
 * Later Consumer itself will move to the test package, see COMP-9051
 * 
 * @author hsommer
 */
public class OldConsumer extends Consumer
{

	public OldConsumer(String channelName, ContainerServicesBase services) throws AcsJException {
		super(channelName, services);
	}

}
