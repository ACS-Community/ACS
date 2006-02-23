/*
 * Created on Aug 31, 2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package alma.acs.acsQoS;

import org.jacorb.orb.policies.*;

/**
 * @author bjeram
 *
 * TODO this has to be implemnted with JacORB 2.2
 */
public class Timeout 
{
	private int m_timeout;
	private org.omg.CORBA.Policy m_policy_list;
	//private org.jacorb.orb.policies.PolicyManager m_policy_manager;
	
	
	
protected void set()
{
}
	
public Timeout(int timeout)
{
	m_timeout = timeout;
}

public int get()
{
	return m_timeout;
}

public void set(int timeout)
{
	m_timeout = timeout;
	set();
}
}


