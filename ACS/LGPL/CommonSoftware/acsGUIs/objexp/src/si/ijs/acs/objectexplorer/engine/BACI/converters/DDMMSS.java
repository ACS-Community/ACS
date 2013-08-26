/*
 * Created on Mar 11, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package si.ijs.acs.objectexplorer.engine.BACI.converters;


/**
 * dd:mm:ss holder class.
 */
public class DDMMSS
{
	/**
	 * Degrees.
	 */
	public int dd;
	
	/**
	 * Minutes.
	 */
	public int mm;
	
	/**
	 * Seconds.
	 */
	public double ss;

	/**
	 * Constructor.
	 * @param dd	degrees.
	 * @param mm	minutes.
	 * @param ss	seconds.
	 */
	public DDMMSS(int dd, int mm, double ss)
	{
		this.dd = dd; this.mm = mm; this.ss = ss;
	}
	
	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString()
	{
		StringBuffer buf = new StringBuffer(16);
		buf.append(String.valueOf(dd));
		buf.append(':');
		buf.append(String.valueOf(mm));
		buf.append(':');
		buf.append(String.valueOf(ss));
		return buf.toString();
	}
}