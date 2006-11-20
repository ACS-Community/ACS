package alma.nctest.clients;

import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.nc.SimpleSupplier;
import alma.simpleAcsTest.structWithMyString;

public class DemoSupplier {

	public static void main(String[] args) {
		structWithMyString ev = new structWithMyString("realString", "typedefHiddenString");

		SimpleSupplier supplier = null;
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) {
			System.out.println("Java property 'ACS.manager' must be "
					+ "set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}
		ComponentClient qlcl = null;
		try {
			qlcl = new ComponentClient(null, managerLoc, "QlClient");
			ContainerServices svc = qlcl.getContainerServices();
			supplier = new SimpleSupplier("qldisplayopNC", svc);
			supplier.publishEvent(ev);
			System.out.println("Done!");
		} catch (Exception e) {
			e.printStackTrace(System.err);
			System.err.println("Failed-------------");
		}
	}
}