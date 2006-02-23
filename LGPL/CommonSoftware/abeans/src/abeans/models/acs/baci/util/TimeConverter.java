/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci.util;

/**
 * Converts ACS to Java time and vice versa.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public final class TimeConverter
{
	
	/**
 	 * Converts from OMG time ("long-epoch UTC", 100-ns since Oct 15, 1582)
	 * to Java time ("short-epoch UTC", ms since Jan 01, 1970).
	 * 
	 * @param omgTime	OMG time.
	 * @return			Java time.
	 */
	public static long toJava(long omgTime)
	{
		return ( omgTime - 122192928000000000L ) / 10000L;
	}

	/**
	 * Converts Java time ("short-epoch UTC", ms since Jan 01, 1970)
	 * to OMG time ("long-epoch UTC", 100-ns since Oct 15, 1582).
	 * 
	 * @param	jTime	Java time.
	 * @retrun			OMG time.
	 */
	public static final long toOMG(long jTime)
	{
		return ( jTime * 10000L + 122192928000000000L );
	}

}
