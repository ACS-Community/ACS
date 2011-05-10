/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarmsanalyzer.save;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Vector;
import java.util.Date;

import alma.acs.alarmsanalyzer.document.AnnunciatedContainer;
import alma.acs.alarmsanalyzer.document.ChatteringAlarmsContainer;
import alma.acs.alarmsanalyzer.document.LostSourcesContainer;
import alma.acs.alarmsanalyzer.document.MostFrequantAlarmsContainer;
import alma.acs.alarmsanalyzer.document.StaleAlarmsContainer;
import alma.acs.alarmsanalyzer.document.StatisticsContainer;
import alma.acs.alarmsanalyzer.document.SuppressedContainer;
import alma.acs.alarmsanalyzer.document.flood.FloodContainer;

/**
 * Object of this class, collects the data to be saved on file.
 * 
 * @author acaproni
 *
 */
public class SaveData {
	
	/**
	 * The types of saved file
	 * 
	 * @author acaproni
	 *
	 */
	public enum FileContentType {
		TXT,
		WIKI
	}
	
	/**
	 * The type of the file to save
	 */
	public final FileContentType saveType;
	
	/**
	 * The tables to write on file
	 */
	private final Vector<TableData> tables = new Vector<TableData>();
	
	/**
	 * Constructor
	 * 
	 * @param type The type of text to write in the file
	 */
	public SaveData(FileContentType type, File file) throws IOException {
		if (type==null) {
			throw new IllegalArgumentException("Invalid null file type descriptor");
		}
		if (file==null) {
			throw new IllegalArgumentException("the file can't be null");
		}
		if (file.exists() && !file.canWrite()) {
			throw new IOException("Can't write file "+file.getAbsolutePath());
		}
		saveType=type;
		save(file);
	}
	
	/**
	 * Save the data in the passed file
	 * 
	 * @param file The file to write data into
	 */
	private void save(File file) throws IOException {
		String content=buildContent();
		
		if (content!=null) {
			FileWriter writer = new FileWriter(file);
			writer.write(content,0,content.length());
			writer.flush();
			writer.close();
		}
		System.out.println("Written "+content.length()+" bytes in "+file.getAbsolutePath());
	}
	
	/**
	 * Get the table out of the containers
	 */
	private String buildContent() {
		tables.add(MostFrequantAlarmsContainer.getInstance().getDataToSave());
		tables.add(StaleAlarmsContainer.getInstance().getDataToSave());
		tables.add(ChatteringAlarmsContainer.getInstance().getDataToSave());
		tables.add(StatisticsContainer.getInstance().getDataToSave());
		tables.add(AnnunciatedContainer.getInstance().getDataToSave());
		tables.add(SuppressedContainer.getInstance().getDataToSave());
		tables.add(LostSourcesContainer.getInstance().getDataToSave());
		tables.add(FloodContainer.getInstance().getDataToSave());
		
		switch (saveType) {
		case TXT: return toTxtString();
		case WIKI: return toWikiString();
		default: return "Unsupported file type: "+saveType;
		}
	}
	
	/**
	 * Build the content of the file in TWiki format
	 * 
	 * @return The content of the file
	 */
	private String toWikiString() {
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd 'at' HH:mm:ss");
		StringBuffer ret = new StringBuffer("---+ Alarm profiling ");
		ret.append(dateFormat.format(new Date()));
		ret.append("\n%TOC%\n");
		
		for (TableData tData: tables) {
			// Append the title
			ret.append("---++ ");
			ret.append(tData.tableTile);
			ret.append('\n');
			
			// Append the header of the table
			for (String colName: tData.header) {
				ret.append("| *");
				ret.append(colName);
				ret.append("* ");
			}
			ret.append("|\n");
			
			// Append the rows
			for (int r=0; r<tData.getRowsNumber(); r++) {
				String[] row=tData.getRow(r);
				for (String val: row) {
					ret.append("| ");
					ret.append(val);
					ret.append(" ");
				}
				ret.append("|\n");
			}
		}
		return ret.toString();
	}
	
	/**
	 * Build the content of the file in TXT format
	 * 
	 * @return The content of the file
	 */
	private String toTxtString() {
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd 'at' HH:mm:ss");
		StringBuffer ret = new StringBuffer("Alarm statistics ");
		ret.append(dateFormat.format(new Date()));
		ret.append("\n\n");
		
		for (TableData tData: tables) {
			// Append the title
			ret.append(tData.tableTile);
			ret.append('\n');
			
			// Append the header of the table
			for (String colName: tData.header) {
				ret.append('\t');
				ret.append(colName);
			}
			ret.append('\n');
			
			// Append the rows
			for (int r=0; r<tData.getRowsNumber(); r++) {
				String[] row=tData.getRow(r);
				for (String val: row) {
					ret.append('\t');
					ret.append(val);
				}
				ret.append('\n');
			}
			ret.append('\n');
		}
		return ret.toString();
	}
}
