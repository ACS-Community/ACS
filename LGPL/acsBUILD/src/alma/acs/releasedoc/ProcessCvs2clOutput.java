/**
 * 
 */
package alma.acs.releasedoc;

import java.io.File;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import alma.acs.releasedoc.Cvs2clXmlEntry.EntryFile;
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
		file
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
			// formatting
			formatter.printTwiki(entries2);
			break;
			
		case user:
			entries2 = formatter.sortByAuthor(entries1);
			TWikiFormatter.HeadingInserter hi = new TWikiFormatter.HeadingInserter() {
				boolean needsHeading(Cvs2clXmlEntry entry) {
					return (lastEntry == null || !lastEntry.getAuthor().equals(entry.getAuthor()));
				}
				String headingText(Cvs2clXmlEntry entry) {
					return entry.getAuthor();
				}
			};
			formatter.setHeadingInserter(hi);
			// formatting
			formatter.printTwiki(entries2);
			break;
			
		case file:
			// A separate Cvs2clXmlEntry gets created for every file, and all entries for the same file are put in a map.
			
			Map<String, List<Cvs2clXmlEntry>> entriesByFileName = new HashMap<String, List<Cvs2clXmlEntry>>();
			
			// Iterate over all entries, some of which represent a related change on many files which needs to be broken up
			for (Cvs2clXmlEntry compositeEntry : entries1) {
				// Iterate over all files of this entry. We don't care about the common dir, because every file is given with the full path anyway.
				for (Cvs2clXmlEntry.EntryFile file : compositeEntry.getFiles()) {
					
					String pathName = file.getPathName();
					List<Cvs2clXmlEntry> currentFileEntries = null;
					if (entriesByFileName.containsKey(pathName)) {
						// reuse entry list, add Cvs2clXmlEntry
						currentFileEntries = entriesByFileName.get(pathName);
					}
					else {
						currentFileEntries = new ArrayList<Cvs2clXmlEntry>();
						entriesByFileName.put(pathName, currentFileEntries);
					}
					
					// create a separate Cvs2clXmlEntry for this file
					Cvs2clXmlEntry singleFileEntry = new Cvs2clXmlEntry(compositeEntry);
					List<EntryFile> singleFileList = new ArrayList<EntryFile>(1);
					singleFileList.add(file);
					singleFileEntry.setFiles(singleFileList);
					currentFileEntries.add(singleFileEntry);
				}
			}
			
			List<String> fileNames = new ArrayList<String>(entriesByFileName.keySet());
			Collections.sort(fileNames);
			
			// @TODO Optionally keep only fileNames that also occur in an externally provided file
			
			hi = new TWikiFormatter.HeadingInserter() {
				boolean needsHeading(Cvs2clXmlEntry entry) {
					if (lastEntry == null) {
						return true;
					}
					String nameThis = getRelevantFileName(entry.getFiles().get(0).getPathName());
					String nameLast = getRelevantFileName(lastEntry.getFiles().get(0).getPathName());
					return !nameThis.equals(nameLast);
				}
				String headingText(Cvs2clXmlEntry entry) {
					return getRelevantFileName(entry.getFiles().get(0).getPathName());
				}
				private String getRelevantFileName(String pathName) {
					int relevantLevels = ( pathName.startsWith("LGPL/CommonSoftware/") ? 3 : 1 );
					String[] nameParts = pathName.split("/");
					String ret = "";
					for (int i = 0; i < relevantLevels; i++) {
						ret += nameParts[i] + "/";
					}
					return ret;
				}
			};
			formatter.setHeadingInserter(hi);
			
			for (String fileName : fileNames) {
				List<Cvs2clXmlEntry> currentFileEntries = formatter.sortByDate(entriesByFileName.get(fileName));
				
				// @TODO construct a fixed-ordered map first and then pass everything in one call to the formatter
				formatter.printTwikiByFile(fileName, currentFileEntries);
			}
			break;

		default: // should never happen, unless someone adds new enum literals
			throw new IllegalStateException("Need to fix this switch statement!");
		}
		
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
			try {
				sortBy = SortBy.valueOf(sortByString);
			} catch (IllegalArgumentException ex) {
				System.err.println("Will ignore illegal sort option '" + sortByString + "'.");
			}
		}
		else {
			sortBy = SortBy.time;
		}
	}

}
