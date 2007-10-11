package com.cosylab.acs.maci.test;

import com.cosylab.acs.maci.ClientType;

/**
 * Client to be used for DoS attack.
 * Creating a huge number of clients will took to much time for test.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class TestAlwaysNotEqualClient extends TestClient
{

	/**
	 * Constructor for TestAlwaysNotEqualClient.
	 * @param name
	 */
	public TestAlwaysNotEqualClient(String name)
	{
		super(name);
	}

	/**
	 * Constructor for TestAlwaysNotEqualClient.
	 * @param name
	 * @param reply
	 */
	public TestAlwaysNotEqualClient(String name, ClientType type)
	{
		super(name, type);
	}

	/**
	 * @see java.lang.Object#equals(Object)
	 */
	public boolean equals(Object arg0)
	{
		return false;
	}

}
