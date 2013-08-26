package alma.acs.releasedoc;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

//changelog

//tagdate
//  tagisodate	
//  tagdatetag
//entry
//  date
//  weekday
//  time
//  isoDate
//  author
//  file
//    name
//    cvsstate
//    revision
//    tag
//  utag
//  commondir 
//  msg

public class Cvs2clXmlEntry
{
	private SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");

	private Date date;
	private String author;
	private List<EntryFile> files = new ArrayList<EntryFile>();
	private String commondir; // null if we have only one file 
	private String msg;
	
	public Cvs2clXmlEntry(Element domElem) throws ParseException {
		NodeList childNodes = domElem.getChildNodes();
		for (int i = 0; i < childNodes.getLength(); i++) {
			Node childNode = childNodes.item(i);
			if (childNode.getNodeType() == Node.ELEMENT_NODE) {
				if (childNode.getNodeName().equals("isoDate")) {
					String dateString = childNode.getTextContent();
					date = parseDate(dateString);
				}
				else if (childNode.getNodeName().equals("author")) {
					author = childNode.getTextContent();
				}
				else if (childNode.getNodeName().equals("commondir")) {
					commondir = childNode.getTextContent();
				}
				else if (childNode.getNodeName().equals("file")) {
					files.add(new EntryFile((Element)childNode));
				}
				else if (childNode.getNodeName().equals("msg")) {
					msg = childNode.getTextContent().trim();
				}
			}
		}
		if (date == null || author == null) {
			// todo Give hint as to which element is lacking what data..
			throw new IllegalArgumentException("Missing required data in entry element.");
		}
		
//		System.out.println("data = " + date.toString() + " and author = " + author + " msg = " + msg);
	}
	

	/**
	 * Special ctor to be used when copying an entry, 
	 * e.g. to flatten files that each should have their own Cvs2clXmlEntry object.
	 */
	public Cvs2clXmlEntry(Cvs2clXmlEntry other) {
		this.date = other.date;
		this.author = other.author;
		this.commondir = other.commondir;
		this.msg = other.msg;
		
		
		this.files = new ArrayList<EntryFile>(other.files);
		for (EntryFile otherfile : other.files) {
			this.files.add(new EntryFile(otherfile));
		}
	}
	
	
	Date getDate() {
		return date;
	}
	
	String getAuthor() {
		return author;
	}
	
	String getMessage() {
		return msg;
	}
	

	String getCommonDir() {
		return this.commondir;
	}
	
	List<EntryFile> getFiles() {
		return files;
	}
	
	/**
	 * Only to be used when inverting the check-in based structure to a file-based structure etc.
	 */
	void setFiles(List<EntryFile> newFileList) {
		files = newFileList;
	}
	
	Date parseDate(String dateString) throws ParseException {
		String parseableDate = ( dateString.endsWith("Z") ? dateString.substring(0, dateString.length()) : dateString ) ;
		return dateFormat.parse(parseableDate);
	}
	
	
	public boolean equals(Object obj) {
		return (
				obj instanceof Cvs2clXmlEntry && 
				this.date.equals(((Cvs2clXmlEntry)obj).date) &&
				this.author.equals(((Cvs2clXmlEntry)obj).author) &&  
				this.msg.equals(((Cvs2clXmlEntry)obj).msg)  
			);
	}

	public int hashCode() {
		return date.hashCode();
	}
	

	public static class EntryFile {
		private String pathName;
		private String cvsstate;
		private String revision;
		private String tag; // may be null
			
		EntryFile(Element domElem) {
			NodeList childNodes = domElem.getChildNodes();
			for (int i = 0; i < childNodes.getLength(); i++) {
				Node childNode = childNodes.item(i);
				if (childNode.getNodeType() == Node.ELEMENT_NODE) {
					if (childNode.getNodeName().equals("name")) {
						pathName = childNode.getTextContent();
					}
					else if (childNode.getNodeName().equals("cvsstate")) {
						cvsstate = childNode.getTextContent();
					}
					else if (childNode.getNodeName().equals("revision")) {
						revision = childNode.getTextContent();
					}
					else if (childNode.getNodeName().equals("tag")) {
						tag = childNode.getTextContent();
					}
				}
			}
			if (pathName == null || cvsstate == null || revision == null) {
				// todo Give hint as to which element is lacking what data..
				throw new IllegalArgumentException("Missing required data in file element.");
			}
//			System.out.println("file = " + pathName); 
		}

		EntryFile(EntryFile other) {
			this.pathName = other.pathName;
			this.cvsstate = other.cvsstate;
			this.revision = other.revision;
			this.tag = other.tag;
		}
		
		
		String getPathName() {
			return pathName;
		}

		String getCvsstate() {
			return cvsstate;
		}

		String getRevision() {
			return revision;
		}

		String getTag() {
			return tag;
		}
	}


}
