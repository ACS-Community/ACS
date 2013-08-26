package alma.acs.testsupport;

public class ProcessUtilTestDummy
{

	/**
	 * Dummy class for testing {@link ProcessUtil}.
	 */
	public static void main(String[] args) {
		System.out.println(ProcessUtilTestDummy.class.getName() + " is running!");
		while (true) {
			try {
				// produce some output so that the streaming it to the JVM can be tested.
				System.out.println("All is well at " + System.currentTimeMillis());
				Thread.sleep(2000);
			} catch (InterruptedException ex) {
				ex.printStackTrace();
			}
		}
	}

}
