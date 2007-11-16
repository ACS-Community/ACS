package alma.acs.releasedoc;

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

//2007-08-03 09:45  hsommer
//
//* Makefile: added logLevelGUI
//
//2007-08-03 09:41  hsommer
//
//* LGPL/CommonSoftware/logLevelGUI/src/:
//  alma/acs/gui/loglevel/LogLevelFrame.java,
//  alma/acs/gui/loglevel/LogLevelPanel.java,
//  alma/acs/gui/loglevel/LogLevelWinAdapter.java,
//  alma/acs/gui/loglevel/leveldlg/LogLevelDlg.java,
//  alma/acs/gui/loglevel/leveldlg/LogLevelModel.java,
//  alma/acs/gui/loglevel/leveldlg/LogLevelTable.java,
//  alma/acs/gui/loglevel/leveldlg/LogTypeCellRenderer.java,
//  alma/acs/gui/loglevel/tree/AdministratorClient.java,
//  alma/acs/gui/loglevel/tree/LogLvlTree.java,
//  alma/acs/gui/loglevel/tree/LogLvlTreeModel.java,
//  alma/acs/gui/loglevel/tree/ManagerBusyDlg.java,
//  alma/acs/gui/loglevel/tree/TreeMouseListener.java,
//  alma/acs/gui/loglevel/tree/TreePopupMenu.java, logLevelPanel:
//  merged from GUI2-2007-06-B
//  
  
public class TWikiFormatter
{
	public static final String LINE_SEPARATOR = System.getProperty("line.separator");
	
	List<Cvs2clXmlEntry> sortByDate(Collection<Cvs2clXmlEntry> entries) {
		List<Cvs2clXmlEntry> list = new ArrayList<Cvs2clXmlEntry>(entries);
		
		Comparator<Cvs2clXmlEntry> comp = new Comparator<Cvs2clXmlEntry>() {
			public int compare(Cvs2clXmlEntry entry1, Cvs2clXmlEntry entry2) {
				return entry1.getDate().compareTo(entry2.getDate());
			}
		};
		
		Collections.sort(list, comp);
		return list;
	}
	
	void printTwiki(List<Cvs2clXmlEntry> entryList) {
		for (Cvs2clXmlEntry entry : entryList) {
			printTwiki(entry);
		}
	}
	
	void printTwiki(Cvs2clXmlEntry entry) {
		
		String output = SimpleDateFormat.getDateTimeInstance().format(entry.getDate());
		output += "  ";
		output += entry.getAuthor();
		output += " \n";
		String wikiFileIndent = "   * ";
		if (entry.getCommonDir() != null) {
			output += "   * " + entry.getCommonDir();
			output += ": \n";
			wikiFileIndent = "      * ";
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
			output += " \n";
		}
		output += "   * " + formatMessage("     ", entry.getMessage()) + " \n";
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
	
	protected String maskWikiWords(String msgLine) {
		StringTokenizer wordTok = new StringTokenizer(msgLine);
		String ret = "";
		while (wordTok.hasMoreTokens()) {
			String word = wordTok.nextToken();
			if (isWikiWord(word)) {
				ret += "!";
			}
			ret += word + " ";
		}		
		return ret;
	}
	
	protected boolean isWikiWord(String word) {
		if (word.length() > 2 && Character.isUpperCase(word.charAt(0)) && Character.isLowerCase(word.charAt(1))) {				
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
	
}
