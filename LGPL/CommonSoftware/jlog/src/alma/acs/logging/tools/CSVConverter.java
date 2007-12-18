package alma.acs.logging.tools;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;
import java.util.regex.Pattern;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.Field;

/**
 * Objects of this class produce a CSV string from a given log.
 * The CSV adhere to the definition in RFC4180.
 * 
 *  It is possible to select the column of the log to export and their
 *  position in the string.
 *  
 *  A method generate the header in CSV format
 *  
 *  All the fields appear inside double quotes.
 *  If a field contains double quotes they are escaped by double quotes
 *  
 *  It is possible to define a different separator instead of a come, like
 *  for example the TAB.
 *  
 *  It is also possible not to enclose the fields by double quotes but in that 
 *  case the content of the field is changed because some character are not
 *  allowed.
 *  
 * @author acaproni
 *
 */
public class CSVConverter {
	
	// The string describing the position and the fields of a log to export
	// The index of each col is the same (indexes 0-15 i.e. 0-f in the string) 
	// plus the data (index 16 or g in the string)
	// The same col can be repeated many times
	//
	// FIELD_TIMESTAMP =  0 (0)
	// FIELD_ENTRYTYPE =  1 (1)
    // FIELD_SOURCEOBJECT=2 (2)
	// FIELD_FILE =       3 (3)
	// FIELD_LINE =       4 (4)
	// FIELD_ROUTINE =    5 (5)
	// FIELD_HOST =       6 (6)
	// FIELD_PROCESS =    7 (7)
	// FIELD_CONTEXT =    8 (8)
	// FIELD_THREAD =     9 (9)
	// FIELD_LOGID =      a (10)
	// FIELD_PRIORITY =   b (11)
	// FIELD_URI =        c (12)
	// FIELD_STACKID =    d (13)
	// FIELD_STACKLEVEL = e (14)
	// FIELD_LOGMESSAGE=  f (15)
	// ADDITIONAL DATA=   g (16)
	//
	// For example if cols==0f1 the the output will contain entries with like that: 
	// timestamp, message, log type
	private String colIndex="01234567890abcdefg";
	
	// The separator, usually a ','
	private char separator=',';
	
	// If true each field is enclosed in double quotes
	// If false the double quotes do not enclose the fields but
	// some character in the fields will be replaced tbecause 
	// are not allowed (like the double quotes for example)
	private boolean useDoubleQuotes = true;
	
	/**
	 * Constructor 
	 * 
	 * @param cols A string describing the field of the log and their
	 *             position in the output
	 */
	public CSVConverter(String cols) {
		if (cols!=null) {
			setCols(cols);
		}
	}
	
	/**
	 * Constructor
	 * 
	 * @param cols A string describing the field of the log and their
	 *             position in the output
	 * @param separator A character to use as fields separator
	 * @param doubleQuotes If true the fields are enclosed by double quotes
	 */
	public CSVConverter(String cols, char separator, boolean doubleQuotes) {
		if (cols!=null) {
			setCols(cols);
		}
		useDoubleQuotes=doubleQuotes;
		this.separator=separator;
	}
	
	/**
	 * Set the fields of the log and their position in the output
	 * 
	 * @param cols
	 */
	public void setCols(String cols) {
		if (!Pattern.matches("[0-9a-g]+",cols)) {
			throw new IllegalArgumentException("Wrong format for columns: [0-9a-g]+");
		}
		colIndex=cols;
	}
	
	/**
	 * Set the separator
	 * 
	 * @param sep The new separator char
	 */
	public void setSeparator(char sep) {
		separator=sep;
	}
	
	/**
	 * Put or remove the double quotes around the fields
	 * 
	 * @param enclose If true the fields are enclosed by double quotes
	 */
	public void encloseByDoubleQuotes(boolean enclose) {
		useDoubleQuotes=enclose;
	}
	
	/**
	 * Generate the header for the CSV file
	 * (it is optional and can appear in the first line of the file)
	 * 
	 * @return The CSV string representing the header in CSV format
	 *         with CR/LF at the end of the line
	 */
	public String getHeader() {
		StringBuilder str = new StringBuilder();
		for (int t=0; t<colIndex.length(); t++) {
			Character c= Character.toUpperCase(colIndex.charAt(t));
			int index;
			if ((c>='0' && c<='9') || (c>='A' && c<='F')) {
				index=Integer.parseInt(c.toString(),16);
			} else {
				index=16; // DATA
			}
			if (t>0) {
				str.append(separator);
			}
			appendField(Field.values()[index].getName(),str);
		}
		str.append(0xD); // CR
		str.append(0xA); // LF
		return str.toString();
	}
	
	/**
	 * Convert a log in a CSV string 
	 * 
	 * @param log The log to convert
	 * @return The CSV string representing the log
	 */
	public String convert(ILogEntry log) {
		StringBuilder str = new StringBuilder();
		SimpleDateFormat df = new SimpleDateFormat(ILogEntry.TIME_FORMAT);
		for (int t=0; t<colIndex.length(); t++) {
			if (t>0) {
				str.append(separator);
			}
			Character c= Character.toUpperCase(colIndex.charAt(t));
			int index;
			if ((c>='0' && c<='9') || (c>='A' && c<='F')) {
				index=Integer.parseInt(c.toString(),16);
				Object obj = log.getField(Field.values()[index]);
				if (obj==null) {
					appendField(null,str);
				} else  if (index==Field.TIMESTAMP.ordinal()) {
					// Write the date in the right format
					Date dt = (Date)obj;
					StringBuffer dateSB = new StringBuffer();
					java.text.FieldPosition pos = new java.text.FieldPosition(0);
					df.format(dt,dateSB,pos);
					appendField(dateSB.toString(),str);
				} else if (index==Field.ENTRYTYPE.ordinal()) {
					appendField(LogTypeHelper.values()[Integer.parseInt(obj.toString())].logEntryType,str);
				} else {
					appendField(obj.toString(),str);
				}
			} else {
				// DATA
				if (log.hasDatas()) {
					appendField(formatData(log.getAdditionalData()),str);
				} else {
					appendField(null,str);
				}
			}
		}
		str.append('\n');//0xD); // CR
		//str.append(0xA); // LF
		return str.toString();
	}
	
	/**
	 * Append a field to the string (str)
	 * The field is into double quotes.
	 * If double quotes exist into the string, they are escaped.
	 * 
	 * @param fld The field to append
	 * @param str The string builder where the field is appended
	 */
	private void appendField(String fld, StringBuilder str) {
		if (useDoubleQuotes) {
			if (fld==null || fld.length()==0) {
				str.append('"');
				str.append('"');
				return;
			}
			str.append('"');
			str.append(fld.replaceAll("\"","\"\""));
			str.append('"');
		} else {
			if (fld==null || fld.length()==0) {
				return;
			}
			String temp = fld.replace('"','\'');
			temp=temp.replace(",","_");
			str.append(temp.replace('\n',' '));
		}
	}
	
	/**
	 * Format the additional data in a string to be
	 * appended in the CSV.
	 * The produced string is not converted in CSV but contains all
	 * the entries of the additional data
	 * 
	 * The format is the following:
	 * [name1 ==> val1] [name2 ==> val2] ....
	 * 
	 * @param datas The additional data of a log
	 * @return A string with the additional data
	 */
	private String formatData(Vector<ILogEntry.AdditionalData> datas) {
		StringBuilder temp = new StringBuilder();
		boolean first=true;
		for (ILogEntry.AdditionalData data: datas) {
			if (!first) {
				temp.append(' ');
			} else {
				first=false;
			}
			temp.append('[');
			temp.append(data.getName());
			temp.append(" ==> ");
			temp.append(data.getValue());
			temp.append(']');
		}
		return temp.toString();
	}
	
}
