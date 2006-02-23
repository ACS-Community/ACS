/*
 * Created on Jun 27, 2003
 *
 * To change the template for this generated file go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
package alma.acs.util;

import java.io.IOException;
import java.io.OutputStream;
import java.io.StringWriter;

/**
 * Is for <code>OutputStream</code>s what <code>StringWriter</code>
 * is for <code>Writer</code>s.
 * 
 * @author hsommer
 *
 */
public class StringOutputStream extends OutputStream
{

	private StringWriter m_stringwriter;

	/**
	 * 
	 */
	public StringOutputStream()
	{
		super();
		m_stringwriter = new StringWriter();
	}


	/* (non-Javadoc)
	 * @see java.io.OutputStream#write(int)
	 */
	public void write(int b) throws IOException
	{
		m_stringwriter.write(b);
	}

	/**
	 * Return the buffer's current value as a string.
	 */
	public String toString() 
	{
		return m_stringwriter.toString();
	}

}
