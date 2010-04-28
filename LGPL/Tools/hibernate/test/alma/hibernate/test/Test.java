package alma.hibernate.test;

import java.util.Map;
import java.util.TreeMap;

import org.hibernate.Session;
import org.hibernate.Transaction;

/**
 * This test needs module acsjlog to run 
 * (which means that it violates the rule that a module's test should run without first 
 * having to build later modules).
 * The reason for this is that the slf4j logging facade framework used by hibernate needs a mapping
 * to a concrete logging framework. In acsjlog we provide the mapping to the ACS logging framework.
 * To run this test independently of acsjlog, we would have to keep locally another slf4j binding, 
 * for example to JDK logging. This is dangerous though because there must be only one such mapping
 * in the system, and we don't want any confusion. Thus we rely on the ACS logging.
 */
public class Test extends TestCase {

	public void testCreate() {
		Session s = openSession();
		Transaction t = s.beginTransaction();

		TestEntity te = new TestEntity();
		
		SubEntity se = new SubEntity();
		se.setName("frodo");
		
		Map<String, SubEntity> sem = new TreeMap<String, SubEntity>();
		sem.put(se.getName(), se);
		s.persist(se);

		te.set_(sem);
		s.persist(te);
		
		
		t.commit();
		
		t = s.beginTransaction();

		s.createCriteria(TestEntity.class).list();
		
		t.commit();
		
		
		s.close();
	}
	
	@Override
	protected Class[] getMappings() {
		return new Class[] {
				TestEntity.class,
				SubEntity.class
		};
	}

	protected String[] getAnnotatedPackages() {
		return new String[] {
				/*"org.hibernate.test.annotations.any"*/
		};
	}
}
