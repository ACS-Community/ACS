/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.time;

/**
 * Class that facilitates runtime profiling.
 * 
 * @author dfugate September 29, 2004
 */
public class Profiler extends alma.acs.util.StopWatch
{
    //private double m_totalTime;
    //private double m_minDuration;
    //private double m_maxDuration;
    private long m_totalTime;
    private long m_minDuration;
    private long m_maxDuration;
    private long m_totalNumStarts;
    private String m_extraDescrip;
    
    /**
     * Standard constructor.
     */
    public Profiler()
	{
	    super();
	    reset();
	}
    
    /**
     * Resets this objects values.
     */
    public void reset()
	{
	    m_totalTime = 0;
	    m_totalNumStarts = 0;
	    m_minDuration = 0x1FFFFFFF;
	    m_maxDuration = 0;
	    m_extraDescrip = "";
	}

    /**
     * Starts a timing operation.
     */
    public void start()
	{
	    super.reset();
	}
    
    /**
     * Stops a timing operation. Should only be called after a start invocation.
     */
    public void stop()
	{
	    long tTime = super.getLapTimeNanos();
	    
	    m_totalNumStarts++;

	    m_totalTime = m_totalTime + tTime;
	    
	    if (tTime < m_minDuration)
		{
		m_minDuration = tTime;
		}

	    if (tTime > m_maxDuration)
		{
		m_maxDuration = tTime;
		}
	}

    /**
     * Prints out a full description of all times that were saved along with
     * other relevant statistical data.
     * @param msg A message to be printed out with the data.
     */
    public void fullDescription(String msg)
	{
	    String out = "";
	    
	    out = out + "#ACS PROFILER# msg=" + msg;
            // multiply by 1E-6 to convert nanoseconds to milliseconds
	    out = out + ", avg=" + (double)(((double)m_totalTime /  (double)m_totalNumStarts) * 1E-6); 
	    out = out + ", runs=" + m_totalNumStarts; 
	    out = out + ", mindur=" + m_minDuration * 1E-6;
	    out = out + ", maxdur=" + m_maxDuration * 1E-6;
	    out = out + ", cpu=Unknown";
	    out = out + ", mem=Unknown"; 
	    out = out + ", date=" + TimeHelper.getUTCDate();
	    out = out + ", ip=" + alma.acs.util.ACSPorts.getIP();
	    out = out + ", lang=java";
	    out = out + ", units=ms";
	    out = out + m_extraDescrip;

	    System.out.println(out);
	}

    /**
     * Adds data to description output
     */
    public void addData(String key, String value)
	{
	    m_extraDescrip = m_extraDescrip + ", " + key + "=" + value;
	}

    /**
     *  For testing purposes only!
     */
    public static void main(String[] args)
	{
	    try
		{
		Profiler joe = new Profiler();
		System.out.println("*****************************************************"); 
		joe.start();
		Thread.sleep(0, 100);
		joe.stop();
		joe.fullDescription("Should only be one...");
		System.out.println("*****************************************************"); 
		joe.reset();
		joe.start();
		Thread.sleep(0, 200);
		joe.stop();
		joe.fullDescription("Should only be one...");
		System.out.println("*****************************************************"); 
		joe.reset();
		joe.start();
		Thread.sleep(0, 100);
		joe.stop();
		joe.start();
		Thread.sleep(0, 500);
		joe.stop();
		joe.start();
		Thread.sleep(0, 100);
		joe.stop();
		joe.fullDescription("Should be three...");
		System.out.println("*****************************************************"); 
		joe.reset();
		joe.start();
		Thread.sleep(5000);
		joe.stop();
		joe.start();
		Thread.sleep(3000);
		joe.stop();
		joe.fullDescription("Should be two with an average of 4 seconds...");
		System.out.println("*****************************************************");
		joe.reset();
		joe.start();
		Thread.sleep(1000);
		joe.stop();
		joe.addData("a key", "a value");
		joe.fullDescription("Should be one extra descrip.");
		joe.addData("somethingElse", "1.2345678");
		joe.fullDescription("Should be two extra descrips.");
		}
	    catch(Exception e)
		{
		e.printStackTrace();
		}
	}
}
