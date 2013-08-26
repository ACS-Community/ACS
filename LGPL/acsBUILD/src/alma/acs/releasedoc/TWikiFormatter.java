package alma.acs.releasedoc;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import alma.acs.releasedoc.Cvs2clXmlEntry.EntryFile;
import alma.acs.util.XmlNormalizer;


/**
 * Produces output in twiki format which can be pasted to the release notes ChangeLog pages,
 * such as http://almasw.hq.eso.org/almasw/bin/view/ACS/ChangeLog-7_0_2.
 * @author hsommer
 */
public class TWikiFormatter
{
	public static final String LINE_SEPARATOR = System.getProperty("line.separator");
	
	private DateFormat dateFormat = SimpleDateFormat.getDateTimeInstance(); 
		
	private HeadingInserter headingInserter;
	
	List<Cvs2clXmlEntry> sortByDate(Collection<Cvs2clXmlEntry> entries) {
		return sort(entries, new Comparator<Cvs2clXmlEntry>() {
			public int compare(Cvs2clXmlEntry entry1, Cvs2clXmlEntry entry2) {
				return entry1.getDate().compareTo(entry2.getDate());
			}
		});
	}
	
	List<Cvs2clXmlEntry> sortByAuthor(Collection<Cvs2clXmlEntry> entries) {
		return sort(entries, new Comparator<Cvs2clXmlEntry>() {
			public int compare(Cvs2clXmlEntry entry1, Cvs2clXmlEntry entry2) {
				if (!entry1.getAuthor().equals(entry2.getAuthor())) {
					return entry1.getAuthor().compareTo(entry2.getAuthor());
				}
				// fallback to sort by date
				return entry1.getDate().compareTo(entry2.getDate());
			}
		} );
	}
	
	
	/**
	 * Returns the {@code entries} as a sorted list with the order given by {@code comp}, 
	 * without modifying the original collection.
	 */
	List<Cvs2clXmlEntry> sort(Collection<Cvs2clXmlEntry> entries, Comparator<Cvs2clXmlEntry> comp) {
		List<Cvs2clXmlEntry> list = new ArrayList<Cvs2clXmlEntry>(entries);
		Collections.sort(list, comp);
		return list;
	}


	void printTwiki(List<Cvs2clXmlEntry> entryList) {
		for (Cvs2clXmlEntry entry : entryList) {
			printTwikiByCheckin(entry);
		}
	}
	
	void printTwikiByCheckin(Cvs2clXmlEntry entry) {
		
		if (headingInserter != null) {
			headingInserter.processRecord(entry);
		}
		String output = dateFormat.format(entry.getDate());
		output += "  ";
		output += entry.getAuthor();
		output += LINE_SEPARATOR;
		String wikiFileIndent = getWikiIndentBullet1();
		if (entry.getCommonDir() != null) {
			output += "   * " + entry.getCommonDir();
			output += ": " + LINE_SEPARATOR;
			wikiFileIndent = "   " + wikiFileIndent;
		}
		Iterator<EntryFile> fileIter = entry.getFiles().iterator();
		while (fileIter.hasNext()) {
			EntryFile file = fileIter.next();
			output += wikiFileIndent;
			if (file.getCvsstate().equals("dead")) {
				output += "<strike>" + file.getPathName() + "</strike>";
			}
			else {
				output += "=" + file.getPathName() + "=";
			}
			output += LINE_SEPARATOR;
		}
		// @TODO perhaps use <blockquote> around the message for indenting, instead of the silly bullet.
		output += "   * " + formatMessage("     ", entry.getMessage()) + LINE_SEPARATOR;
		System.out.println(output);
	}

	void printTwikiByFile(String fileName, List<Cvs2clXmlEntry> entries) {
		if (entries == null || entries.size() < 1) {
			throw new IllegalArgumentException("empty entries arg");
		}
		Cvs2clXmlEntry firstEntry = entries.get(0);
		Cvs2clXmlEntry lastEntry = entries.get(entries.size() - 1);

		if (headingInserter != null) {
			headingInserter.processRecord(firstEntry);
		}
		
		String output = lastEntry.getFiles().get(0).getCvsstate().equals("dead") 
				? "<strike>" + fileName + "</strike>"
				: fileName;
		output += LINE_SEPARATOR;
		for (Cvs2clXmlEntry entry : entries) {
			output += getWikiIndentBullet1() + dateFormat.format(entry.getDate()) + "  " + entry.getAuthor() + "<br>" + LINE_SEPARATOR;
			// @TODO perhaps use <blockquote> around the message for indenting, instead of the silly bullet.
			output += getWikiIndentBullet1_subseqLines() + formatMessage(getWikiIndentBullet1_subseqLines(), entry.getMessage()) + LINE_SEPARATOR;
		}
		System.out.println(output);
	}
	
	protected String formatMessage(String indentAfterFirstLine, String message) {
		String xmlMaskedMessage = XmlNormalizer.normalize(message);
		StringTokenizer tok = new StringTokenizer(xmlMaskedMessage, "\n\r\f");
		String ret = maskWikiWords(tok.nextToken());
		while (tok.hasMoreTokens()) {
			ret += " <br>" + LINE_SEPARATOR;
			ret += indentAfterFirstLine;
			ret += maskWikiWords(tok.nextToken());
		}
		return ret;
	}
	
	/**
	 * @TODO: try to use <code>&lt;noautolink&gt;</code> pair around our text and stop masking individual words
	 */
	protected String maskWikiWords(String msgLine) {
		StringTokenizer wordTok = new StringTokenizer(msgLine, " (", true);
		String ret = "";
		while (wordTok.hasMoreTokens()) {
			String word = wordTok.nextToken();
			if (isWikiWord(word)) {
				ret += "!";
			}
			ret += word;
		}
		return ret;
	}
	
	/**
	 * @param word
	 */
	protected boolean isWikiWord(String word) {
		if (word == null || word.length() <= 2) {
			return false;
		}
		
		// skip leading '(' and uppercase chars
		boolean hasLeadingUppercase = false;
		int pos = 0;
		for (int i = 0; i < word.length(); i++) {
			char c = word.charAt(i);
			if (Character.isUpperCase(word.charAt(i))) {
				hasLeadingUppercase = true;
			}
			else {
				break;
			}
			pos++;
		}

		if (hasLeadingUppercase && pos < word.length() && Character.isLowerCase(word.charAt(pos))) {
			for (int i = 2; i < word.length(); i++) {
				if (Character.isUpperCase(word.charAt(i))) {
					// got a wiki word
					return true;
				}
			}
		}
		// for some reason "CDB" is considered a wiki word
		if (word.equals("CDB")) {
			return true;
		}
		return false;
	}
	
	String getWikiIndentBullet1() {
		return "   * ";
	}
	
	String getWikiIndentBullet1_subseqLines() {
		return "     ";
	}
	
	abstract static class HeadingInserter {
		protected Cvs2clXmlEntry lastEntry;
		abstract boolean needsHeading(Cvs2clXmlEntry entry);
		abstract String headingText(Cvs2clXmlEntry entry);
		void processRecord(Cvs2clXmlEntry entry) {
			if (needsHeading(entry)) {
				System.out.println();
				System.out.println("---++ " + headingText(entry));
			}
			lastEntry = entry;
		}
	}
	
	protected void setHeadingInserter(HeadingInserter hi) {
		headingInserter = hi;
	}
}
