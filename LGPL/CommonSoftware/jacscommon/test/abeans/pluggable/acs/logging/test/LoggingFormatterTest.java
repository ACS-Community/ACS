/*
 * @@COPYRIGHT@@
 */
 
package abeans.pluggable.acs.logging.test;

import java.util.LinkedHashMap;
import java.util.Map;

import junit.framework.TestCase;
import junit.framework.TestSuite;

import abeans.core.AssertionFailed;
import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.defaults.MessageLogEntry;
import abeans.pluggable.acs.logging.LoggingFormatter;
import abeans.pluggable.acs.logging.LoggingLevel;


/**
 * Test for LoggingFormatter.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class LoggingFormatterTest extends TestCase implements Identifiable
{
	/**
	 * Identifier
	 */
	private Identifier id = null;


	public LoggingFormatterTest(String name)
	{
		super(name);
	}

	/**
	 * @see Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		class IdentifierImpl implements Identifier
		{
			private String name = null;
			private String shortName = null;
			private String qualifiedShortName = null;
			private String qualifiedLongName = null;
			private short type = Identifier.APPLICATION;
	
			public IdentifierImpl()
			{
				this.name = "LoggingFormatterTest";
				this.shortName = "LoggingFormatterTest";
				this.type = Identifier.APPLICATION;
				
			}
			public String getName()
			{
				return name;
			}
			public String getShortName()
			{
				return shortName;
			}
			public String getQualifiedLongName()
			{
				if (qualifiedLongName == null)
				{
						qualifiedLongName = "LoggingFormatterTest";
				}
				return qualifiedLongName;
			}
			public String getQualifiedShortName()
			{
				if (qualifiedShortName == null)
				{
					qualifiedShortName = "LoggingFormatterTest";
				}
				return qualifiedShortName;
			}
			public short getType()
			{
				return type;
			}
		}
		
		if (id == null) id = new IdentifierImpl();
		return id;
	}

	/**
	 */
	public static TestSuite suite()
	{
		return new TestSuite(LoggingFormatterTest.class);
	}

	/**
	 * Test LoggingFromatter.
	 */
	public void testFormatter()
	{ 
		LoggingFormatter formatter = new LoggingFormatter();

		MessageLogEntry record = new MessageLogEntry(this, "testFormatter", "INFO message", LoggingLevel.INFO);
		System.out.println(formatter.format(record));

		record = new MessageLogEntry(this, "testFormatter", "DEBUG message", LoggingLevel.DEBUG);
		System.out.println(formatter.format(record));

		// test additional properties
		Map map = new LinkedHashMap();
		map.put("Line", new Long(1208));
		map.put("StackId", "Stack ID");
		map.put("StackLevel", new Long(4));
		map.put("Priority", new Long(11));
		map.put("Context", "Testing...");
		map.put("ThreadName", "MyThread");
		map.put("Uri", "abeans-CDBDAL://csl01:5001/alma/gizmo/current/units?get");
		Map nv = new LinkedHashMap();
		nv.put("name1", "value1");
		nv.put("name2", "value2");
		map.put("Data", nv);

		Map nv2 = new LinkedHashMap();
		nv2.put("attribute1", "value1");
		nv2.put("attribute2", "value2");
		map.put("Attributes", nv2);
		
		record = new MessageLogEntry(this, "testFormatter", "ERROR message", LoggingLevel.ERROR);
		record.setParameters(new Object[] { map });
		System.out.println(formatter.format(record));
		
		record = new MessageLogEntry(this, "testFormatter", "EMERGENCY message", LoggingLevel.EMERGENCY);
		System.out.println(formatter.format(record));

		Exception ex = new IllegalArgumentException("source of all");
		ex = new AssertionFailed (this, "exception #1", ex);
		ex = new AssertionFailed(this, "exception #2", ex);
		record = new MessageLogEntry(this, "testFormatter", "ERROR message", ex, LoggingLevel.ERROR);
		System.out.println(formatter.format(record));
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return false;
	}

}
