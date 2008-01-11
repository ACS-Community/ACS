/**
 * 
 */
package alma.acs.releasedoc;

import java.io.File;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import alma.acs.util.CmdLineArgs;
import alma.acs.util.CmdLineRegisteredOption;

/**
 * This class bundles the other classes of this package and provides a command line tool.
 * @author jschwarz, hsommer
 */
public class ProcessCvs2clOutput 
{	
	public static enum SortBy {
		time,
		user,
		path
	}

	private SortBy sortBy;
	private List<File> subtractFiles = new ArrayList<File>();


	public static void main(String[] args) {
		try {
			(new ProcessCvs2clOutput()).run(args);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	
	void run(String[] args) throws Exception {
		
		if (args.length < 1) {
			throw new IllegalArgumentException("At least the xml file name must be given.");
		}		
		File xmlFile1 = new File(args[args.length-1]);
		
		if (!xmlFile1.exists()) {
			System.err.println("File "+xmlFile1.getAbsolutePath()+" does not exist.");
		}

		setOptions(args);
		
		Cvs2clXmlParser parser = new Cvs2clXmlParser();
		
		Document doc = parser.parseXml(xmlFile1);
		Set<Cvs2clXmlEntry> entries1 = new HashSet<Cvs2clXmlEntry>();
		storeEntries(doc, entries1);

		// optionally subtract entry sets
		for (File subtractFile : subtractFiles) {
			Document doc2 = parser.parseXml(subtractFile);
			Set<Cvs2clXmlEntry> entries2 = new HashSet<Cvs2clXmlEntry>();
			storeEntries(doc2, entries2);
			entries1.removeAll(entries2);
			System.out.println("Subtracted cvs entries from file " + subtractFile.getAbsolutePath());
		}
		
		TWikiFormatter formatter = new TWikiFormatter();
		
		// sorting
		List<Cvs2clXmlEntry> entries2 = null;
		switch (sortBy) {
		case time:
			entries2 = formatter.sortByDate(entries1);			
			break;
		case user:
			entries2 = formatter.sortByAuthor(entries1);			
			break;
			
		// @TODO: other sorting options!!
		default:
			entries2 = new ArrayList<Cvs2clXmlEntry>(entries1);
			break;
		}
		
		// formatting
		formatter.printTwiki(entries2);
	}
	
	
	void storeEntries(Document doc, Set<Cvs2clXmlEntry> entries) throws XPathExpressionException, ParseException {
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


	void setOptions(String[] args) {
		// -- prepare arg parser
		CmdLineArgs cmdArgs = new CmdLineArgs();
		
		// optionally subtract other sets of cvs logs
		CmdLineRegisteredOption optSubtract = new CmdLineRegisteredOption("-subtract", 1);
		cmdArgs.registerOption(optSubtract);

		// sort by time or by path (module, file,...) and time
		CmdLineRegisteredOption optSortBy = new CmdLineRegisteredOption("-sortBy", 1);
		cmdArgs.registerOption(optSortBy);
		
		// -- parse and set args
		cmdArgs.parseArgs(args);		

		// -- subtract
		subtractFiles.clear();
		if (cmdArgs.isSpecified(optSubtract)) {
			String subtractFileNames = cmdArgs.getValues(optSubtract)[0].trim();
			for (String fileName : subtractFileNames.split(File.pathSeparator)) {
				File file = new File(fileName);
				if (file.exists() && file.isFile()) {
					subtractFiles.add(file);
				}
				else {
					System.err.println("Will ignore non-existing subtract file " + file.getAbsolutePath());
				}
			}
		}
		
		// -- sort by
		if (cmdArgs.isSpecified(optSortBy)) {
			String sortByString = cmdArgs.getValues(optSortBy)[0].trim();
			sortBy = SortBy.valueOf(sortByString);
		}
		else {
			sortBy = SortBy.time;
		}
	}

}
