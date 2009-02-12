package alma.hibernate.test;

import java.util.Map;
import java.util.TreeMap;

import org.hibernate.Session;
import org.hibernate.Transaction;

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
