package alma.acs.releasedoc;

import java.io.File;
import java.text.ParseException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class Cvs2clXmlParserTest extends TestCase
{
	private Cvs2clXmlParser parser;
	
	protected void setUp() throws Exception {
		parser = new Cvs2clXmlParser();
	}

	public void testIsWikiWord() {
		TWikiFormatter formatter = new TWikiFormatter();
		assertTrue(formatter.isWikiWord("LoggingConfigurableLevelTranslator"));
		assertTrue(formatter.isWikiWord("NamingContext"));
		assertTrue(formatter.isWikiWord("ACSCoreLevel"));
		assertTrue(formatter.isWikiWord("IOLogsHelper.java"));
		assertFalse(formatter.isWikiWord(""));
		assertFalse(formatter.isWikiWord("AAA"));
		assertFalse(formatter.isWikiWord("alllower"));
		assertFalse(formatter.isWikiWord("soStupid"));
		assertFalse(formatter.isWikiWord("ALL_UPPER"));
	}
	
	public void testMaskWikiWords() {
		TWikiFormatter formatter = new TWikiFormatter();
		String out = formatter.maskWikiWords("Made MyCollocatedDummy1 immortal (KeepAliveTime=-1) (some more text in parentheses)");
		assertEquals("Made !MyCollocatedDummy1 immortal (!KeepAliveTime=-1) (some more text in parentheses)", out);
	}
	
//	public void testFormatWikiMessage() {
//		TWikiFormatter formatter = new TWikiFormatter();
//		formatter.formatMessage("", message);
//	}
	
	public void __testParseXml() throws Exception {
		File xmlFile = new File("cvsLogs_603_700.xml");
		assertTrue("Fix user dir to contain " + xmlFile.getAbsolutePath(), xmlFile.exists());
		
		Document doc = parser.parseXml(xmlFile);
		assertNotNull(doc);
		
		Element docElem = doc.getDocumentElement();
		assertEquals("changelog", docElem.getNodeName());

		// list all elem names that occur
//		XPath xpath = XPathFactory.newInstance().newXPath();
//		Object xpathResult = xpath.evaluate("//*", docElem, XPathConstants.NODESET);
//		assertNotNull(xpathResult);		
//		NodeList nodes = (NodeList) xpathResult;
//		Set<String> elemNames = new HashSet<String>();
//		for (int i = 0; i < nodes.getLength(); i++) {
//			Node node = nodes.item(i);
//			if (node.getNodeType() == Node.ELEMENT_NODE) {
//				elemNames.add(node.getNodeName());
//			}
//		}
//		for (String elemName: elemNames) {
//			System.out.println(elemName);
//		}
		
		XPath xpath = XPathFactory.newInstance().newXPath();
		NodeList entryNodes = (NodeList) xpath.evaluate("//entry", docElem, XPathConstants.NODESET);
		for (int i = 0; i < entryNodes.getLength(); i++) {
			Element entryElem = (Element) entryNodes.item(i);
			Cvs2clXmlEntry entry = new Cvs2clXmlEntry(entryElem);
		}
	}
	
	public void testIntersection() throws Exception {
		File xmlFile1 = new File("cvsLogs_603_604.xml");
		assertTrue(xmlFile1.exists());
		File xmlFile2 = new File("cvsLogs_603_700.xml");
		assertTrue(xmlFile2.exists());
		
		Document doc1 = parser.parseXml(xmlFile1);
		assertNotNull(doc1);
		Set<Cvs2clXmlEntry> entries1 = new HashSet<Cvs2clXmlEntry>();
		storeEntries(doc1, entries1);

		Document doc2 = parser.parseXml(xmlFile2);
		assertNotNull(doc2);
		Set<Cvs2clXmlEntry> entries2 = new HashSet<Cvs2clXmlEntry>();
		storeEntries(doc2, entries2);
				
		System.out.println("Set 1 size = " + entries1.size());
		System.out.println("Set 2 size = " + entries2.size());
		
		entries2.removeAll(entries1);
		System.out.println("Set 2 minus Set 1 size = " + entries2.size());
		
		TWikiFormatter formatter = new TWikiFormatter();
		List<Cvs2clXmlEntry> sortedEntries = formatter.sortByDate(entries2);
		formatter.printTwiki(sortedEntries);
	}

	private void storeEntries(Document doc, Set<Cvs2clXmlEntry> entries) throws XPathExpressionException, ParseException {
		XPath xpath = XPathFactory.newInstance().newXPath();
		NodeList entryNodes = (NodeList) xpath.evaluate("//entry", doc, XPathConstants.NODESET);
		System.out.println("Number of entries: " + entryNodes.getLength());
		for (int i = 0; i < entryNodes.getLength(); i++) {
			Element entryElem = (Element) entryNodes.item(i);
			Cvs2clXmlEntry entry = new Cvs2clXmlEntry(entryElem);
			boolean wasNewEntry = entries.add(entry);
			if (!wasNewEntry) {
				System.out.println("**** Multiple entry " + entry.getDate() + " author = " + entry.getAuthor());
			}
		}
	}
}
