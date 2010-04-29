package alma.acs.tmcdb;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import junit.framework.TestCase;

public class TestPojosInitialization extends TestCase {

	private static final String CONTNAME = "SUPERCONTAINER";

	public void setUp() {
		
	}

	public void testAdd() {

		Configuration conf = new Configuration();

		// Test simple add* method
		for(int i=0; i!= 10; i++) {
			Container cont = new Container();
			cont.setContainerName(CONTNAME);
			cont.setCallTimeout(0);
			cont.setConfiguration(conf);
			conf.addContainerToContainers(cont);
		}

		assertEquals(conf.getContainers().size(), 10);
		for(Container cont : conf.getContainers()) {
			assertEquals(cont.getContainerName(), CONTNAME);
			assertEquals(cont.getCallTimeout().intValue(), 0);
			assertEquals(cont.getConfiguration(), conf);
		}

		// Test addSeveral* method
		Set<Container> list = new HashSet<Container>();
		for (int i = 0; i != 10; i++) {
			Container cont = new Container();
			cont.setContainerName(CONTNAME);
			cont.setCallTimeout(0);
			cont.setConfiguration(conf);
			list.add(cont);
		}
		conf.addContainers(list);

		assertEquals(conf.getContainers().size(), 20);
		for(Container cont : conf.getContainers()) {
			assertEquals(cont.getContainerName(), CONTNAME);
			assertEquals(cont.getCallTimeout().intValue(), 0);
			assertEquals(cont.getConfiguration(), conf);
		}

		// Now testing with ManyToMany
		FaultFamily ff1 = new FaultFamily();
		ff1.setFamilyName("FF1");
		FaultFamily ff2 = new FaultFamily();
		ff2.setFamilyName("FF2");

		List<AlarmCategory> categories = new ArrayList<AlarmCategory>();
		for(int i = 0; i!= 10; i++)  {
			AlarmCategory ac = new AlarmCategory();
			ac.setConfiguration(conf);
			ac.setPath("MyPath");
			ac.addFaultFamilyToFaultFamilies(ff1);
			ac.addFaultFamilyToFaultFamilies(ff2);
			ff1.addAlarmCategoryToAlarmCategories(ac);
			ff2.addAlarmCategoryToAlarmCategories(ac);
			categories.add(ac);
		}

		assertEquals(ff1.getAlarmCategories().size(), 10);
		assertEquals(ff2.getAlarmCategories().size(), 10);
		for(AlarmCategory ac: categories) {
			assertEquals(ac.getFaultFamilies().size(), 2);
			for(FaultFamily ff: ac.getFaultFamilies()) {
				if( !ff.equals(ff1) )
					assertEquals(ff, ff2);
				if( !ff.equals(ff2) )
					assertEquals(ff, ff1);
			}
		}
	}

	public void tearDown() {
		
	}

}
