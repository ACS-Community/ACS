/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/*
 * Created on Jan 14, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.cosylab.logging.engine;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.input.DOMBuilder;
import org.jdom.adapters.JAXPDOMAdapter;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;

/**
 * @author acaproni
 *
 * This class stores all the filters defined by the user.
 * It supports all the functionalities needed by the filters like
 * load and save, the check if a log is compatible with the
 * filters and so on
 * 
 * FiltersList is a Vector. It uses another vector to
 * store the indexes of the active filters
 */
public class FiltersVector extends Vector<Filter> {
	// The vector of the active filters
	// It contains the indexes (int) of the active filters
	private Vector<Integer> activeFilters= new Vector<Integer>();

	/**
	 * Constructor 
	 */
	public FiltersVector() {
		super();
	}

	/**
	 * Add a filter to the vector. 
	 * If it is active, its index is added to the vector of the
	 * active filters
	 * 
	 * @param f The filter to add
	 * @param active true if the filter is active
	 */
	public void addFilter(Filter f, boolean active) {
		if (f==null) {
			throw new IllegalArgumentException("Invalid null filter");
		}
		add(f);
		if (active) activeFilters.add(new Integer(size()-1));
	}
	
	/**
	 * Set the filters in the vector deleting any other
	 * filter. The activeFilters vector is also updated
	 * 
	 * @param filters The array of filters to add
	 * @param active The array of active filters
	 */
	public void setFilters(Filter[] f, boolean[] active) {
		if (f.length !=  active.length) {
			throw new IllegalArgumentException("The size of filters and active differ");
		}
		clear();
		activeFilters.clear();
		for (int i = 0; i < f.length; i++) {
			add(f[i]);
			if (active[i]) {
				activeFilters.add(new Integer(i));
			} 
		}
	}
	
	/**
	 * Set the filters in this vector to be the same of the 
	 * passed vector
	 * 
	 * @param flts The vector of filters
	 */
	public void setFilters(FiltersVector flts) {
		if (flts==null) {
			throw new IllegalArgumentException("Invalid null filters vector");
		}
		clear();
		activeFilters.clear();
		// Get the indexes of the active filters
		for (Filter f: flts) {
			if (f==null) {
				throw new IllegalStateException("A filter in the vector is null");
			}
			add(f);
		}
		int[] activesIdx=flts.getAppliedFiltersIndexes();
		if (activesIdx==null) {
			return;
		}
		for (int t=0; t<activesIdx.length; t++) {
			activeFilters.add(activesIdx[t]);
		}
	}

	/**
	 * Return the indexes of the active filters
	 * 
	 * @return The array of the indexes of the active filters
	 */
	public int[] getAppliedFiltersIndexes() {
		if (activeFilters.size()==0) {
			// No active filters
			return null;
		} 
		int[] temp = new int[activeFilters.size()];
		for (int t=0; t<activeFilters.size(); t++) {
			temp[t]=(activeFilters.get(t)).intValue();
		}
		return temp;
	}

	/**
	 * Check if there are active filters
	 * 
	 * @return true if there are active filters
	 */
	public boolean hasActiveFilters() {
		return activeFilters.size()>0;
	}
	
	/**
	 * Return true if the filter is active
	 * 
	 * @param n The index of the filters
	 * @return true if the filter is active
	 */
	public boolean isActive(int n) {
		return activeFilters.contains(new Integer(n));
	}
	
	/**
	 * Apply the (active) filters to a log
	 * 
	 * @param log The log to check 
	 * @return true if the log pass all the active filters check
	 */
	public boolean applyFilters(ILogEntry log) {
		boolean testPassed=activeFilters.size()>=0;
		
		// Check the log against all the active Filters (if any)
		for (int t=0; t<activeFilters.size() && testPassed; t++) {
			testPassed=(get(activeFilters.get(t).intValue())).applyTo(log, false);
		}
		return testPassed;
	}
	
	/**
	 * Returns the filter(s) applied as a string.
	 * 
	 * @return The string with the applied filters
	 */
	public String getFilterString() {
		if (activeFilters.size() == 0) {
			return "Not filtered";
		}
		
		StringBuffer returnValue = new StringBuffer();
		for (int t = 0; t<activeFilters.size(); t++) {
			int pos = activeFilters.get(t).intValue();
			if (t>0) returnValue.append(", ");
			returnValue.append(((Filter)elementAt(pos)).field.getName());
		}
		return "Filtered by: "+returnValue.toString();
	}

	/**
	 * Delete all the filters (and the active vector)
	 */
	public void clear() {
		super.clear();
		activeFilters.clear();
	}
	
	/**
	 * Load filters 
	 * In case of errors an exception is thrown
	 * 
	 * @param f The xml file to parse (java.io.File) 
	 * @param eraseFilters If true existing filters will be deleted before loading
	 * @param fileName Is the name of the file (it is usually null and the name is
	 *                 read from the parameter f. However when this method is called
	 *                 recursively we need to pass the name because the file we're reading
	 *                 is a temporary file (generated by converting the original file)
	 * @throws <code>Exception</code> In case of error loading the filters
	 */
	public void loadFilters(File f, boolean eraseFilters, String fileName) throws Exception {
		Document doc;
		FileInputStream fStream;
		fStream = new FileInputStream(f);
		JAXPDOMAdapter adapter = new JAXPDOMAdapter(); 
		DOMBuilder builder = new DOMBuilder();
		doc = builder.build(adapter.getDocument(fStream,false));
        Element root = doc.getRootElement();
        if (root.getName().compareToIgnoreCase("FILTER_LIST")!=0 && 
        	root.getName().compareToIgnoreCase("FILTERS")!=0) {
        	// The root is not FILTER_LIST neither FILTERS so the file we are 
        	// parsing is not of the right type
        	// We show a message to the user and abort
        	throw new Exception("Wrong xml file: the root is "+root.getName()+" instead of FILTER_LIST");
        } else if (root.getName().compareToIgnoreCase("FILTER_LIST")==0) {
        	// The file is an old version (show a message to the user to explain what's
        	// happening and how to avoid this message appears again in future
        	System.err.println("The format of this filter file is deprecated. \nSave the filters to avoid this message appears in future.");
    		// Convert the format of the file
    		File newFormatFile = convertOldFilterFile(f);
    		if (newFormatFile==null) {
    			// Something went wrong while converting: abort
    			throw new Exception("Error converting the file to the new format");
    		} else {
    			// Recursively call loadFilters but now the file has
    			// the new format
    			loadFilters(newFormatFile,eraseFilters,f.getAbsolutePath());
    			return;
    		}
        } 
        
        Element filtersElement = root.getChild("FILTER_LIST");
        if (filtersElement==null) return; // No filters defined
        
        List children = filtersElement.getChildren("FILTER");
        // Check if the vector has elements
        if (children==null || children.size()==0) {
        	return;
        }
        
        if (eraseFilters) {
        	// The user whish to substitute the existing filters
        	// with those he's loading
        	clear();
        }

        // Read all the filters from the file
        Iterator it = children.iterator();
        while (it.hasNext()) {
        	// Temporary Strings to store values read from the file
        	String lethalStr = null;
        	String notStr = null;
        	String minStr = null;
        	String maxStr = null;
        	String exactStr = null;
        	String wcharStr = null;
        	String fieldStr=null;
        	Boolean enabled=null;
        	Element element = (Element) it.next();
        	String type = element.getAttributeValue("type");
        	Element lethalElement = element.getChild("LETHAL");
        	if (lethalElement!=null) {
        		lethalStr=lethalElement.getText();
        	}
        	Element notElement = element.getChild("APPLYNOT");
        	if (notElement!=null) {
        		notStr=notElement.getText();
        	}
        	Element minElement = element.getChild("MIN");
        	String minType=null;
        	if (minElement!=null) {
        		minStr=minElement.getText();
        		minType = minElement.getAttributeValue("class");
        	}
        	Element maxElement = element.getChild("MAX");
        	String maxType=null;
        	if (maxElement!=null) {
        		maxStr=maxElement.getText();
        		maxType = maxElement.getAttributeValue("class");
        	}
        	Element exactElement = element.getChild("EXACT");
        	String exactType=null;
        	if (exactElement!=null) {
        		exactStr=exactElement.getText();
        		exactType=exactElement.getAttributeValue("class");
        	}
        	Element wcharElement = element.getChild("WILDCHAR");
        	if (wcharElement!=null) {
        		wcharStr=wcharElement.getText();
        	}
        	Element fieldElement = element.getChild("FIELD");
        	if (fieldElement!=null) {
        		fieldStr=fieldElement.getText();
        	}
        	// Build the Field.
        	LogField field = LogField.fromName(fieldStr);
        	if (field==null) {
        		// Ooops the field has not been found
        		// Check if this String contains an Integer representing the
        		// position of this field in the enum LogField (it was in the old format)
        		Integer i = Integer.parseInt(fieldStr);
        		field = LogField.values()[i];
        	}
        	
        	
        	Element enabledElement = element.getChild("ENABLED");
        	if (enabledElement!=null) {
       			enabled= new Boolean(enabledElement.getText());
        	} else {
        		// Tag not found: enable the filter per default
        		enabled=Boolean.TRUE;
        	}
        	
        	// Build the filter
        	Filter filter = Filter.buildFilter(
        			field,
        			lethalStr,
					notStr,
					minStr,
					minType,
					maxStr,
					maxType,
					exactStr,
					exactType,
					wcharStr);
        	
        	// bulidFilter throws an exception but ner return a null filter
        	if (filter==null) {
        		throw new IllegalStateException("The filter should not be null");
        	} 
        	addFilter(filter,enabled);
        }
	}
	
	/**
	 * Convert a filter file from the old to the new format
	 * The difference between the old stile and this new format is the presence
	 * of the history as well as the document type.
	 * At the present the history is not needed anymore but I keep this
	 * method for converting the xml type.
	 * 
	 * NOTE: This method will be removed
	 * 
	 * @param f The file to convert
	 * 
	 * @return The file with new format
	 * @throws <code>Exception</code> In case of error converting the file
	 */
	private File convertOldFilterFile(File oldFile) throws Exception {
		if (oldFile==null) {
			throw new IllegalArgumentException("The file can't be null");
		}
		// Check if the file is readable and writable
		if (!oldFile.canRead() || !oldFile.canWrite()) {
			throw new IllegalArgumentException("The file must have read and write permissions");
		}
		//Create a new temporary file 
		try {
		File tempFile = File.createTempFile("jlog_",null);
		
		FileOutputStream outStream = new FileOutputStream(tempFile);
		DataOutputStream dataOutStream = new DataOutputStream(outStream);
		
		// Write the header of the new file
		dataOutStream.writeBytes("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>\n");
		dataOutStream.writeBytes("<FILTERS>\n");
		// Write the list of filters as it is in the old file
		FileInputStream inStream = new FileInputStream(oldFile);
		byte[] buffer = new byte[1024];
		// strBuffer will contain the whole old file
		StringBuffer strBuffer = new StringBuffer();
		// The number of bytes read
		int nBytes;
		do {
			nBytes=inStream.read(buffer);
			if (nBytes>0) {
				strBuffer.append(new String(buffer,0,nBytes));
			}
		} while (nBytes!=-1); //EOF
		
		String startTagStr = new String("<FILTER_LIST>");
		String endTagStr = new String("</FILTER_LIST>");
		int startTagPos = strBuffer.indexOf(startTagStr);
		int endTagPos   = strBuffer.indexOf(endTagStr);
		
		if (startTagPos>0 && endTagPos>0) {
			dataOutStream.writeBytes(strBuffer.substring(startTagPos,endTagPos+endTagStr.length()));
		} else {
			// Strange.. it seems I read no filters definition...
			dataOutStream.writeBytes(startTagStr);
			dataOutStream.writeBytes(endTagStr);
		}
		
		// Close the file with the termination tags
		dataOutStream.writeBytes("</FILTERS>");

		return tempFile;
		
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * Save the filters on a file
	 * 
	 * @param f The xml file to store the filters in (java.io.File)
	 */
	public void saveFilters(File f) throws IOException {
		if (size()==0) return; // No filters to save
		if (f.exists()) f.delete();
		f.createNewFile();
		FileOutputStream outStream = new FileOutputStream(f);
		DataOutputStream dataOutStream = new DataOutputStream(outStream);
		dataOutStream.writeBytes("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>\n");
		dataOutStream.writeBytes("<FILTERS>\n");
		dataOutStream.writeBytes("<FILTER_LIST>\n");
		for (int t=0; t<size(); t++) {
			StringBuilder xmlString=new StringBuilder(get(t).toXMLString());
			int pos=xmlString.indexOf("</FILTER>");
			if (pos>=0) {
				StringBuilder enabledStr = new StringBuilder("\t<ENABLED>");
				enabledStr.append(isActive(t));
				enabledStr.append("</ENABLED>\n\t");
				xmlString.insert(pos, enabledStr);
			}
			dataOutStream.writeBytes(xmlString.toString());
		}
		dataOutStream.writeBytes("</FILTER_LIST>\n");
		dataOutStream.writeBytes("</FILTERS>\n");
	}
	
	/**
	 * Remove an element from the FiltersVector
	 * (We need to override this method because we need to keep the
	 * activeFilters aligned) 
	 */
	public Filter remove(int index) {
		if (index<0 || index>=this.size()) {
			throw new IndexOutOfBoundsException("Invalid index");
		}
		// Get and remove the element from the Vector
		Filter f = super.remove(index);
		// The removed caused that all the filters following the removed one 
		// have been moved back by the remove 
		// @see java.util.Vector.remove for further details
		// To keep up to date the activeFilters vector we have to:
		// 1 remove index if present in the vector
		// 2 decrease all the indexes in activeFilters, greater then index
		int pos=-1; // pos of index in activeFilters
		for (int t=0; t<activeFilters.size(); t++) {
			int idx = activeFilters.get(t).intValue();
			if (idx==index) {
				pos=t;
			} else if (idx>index) {
				activeFilters.set(t,new Integer(idx-1));
			}
		}
		if (pos!=-1) {
			activeFilters.remove(pos);
		}
		return f;
	}
	
	/**
	 * Activate/deactivate a filter
	 * 
	 * @param f The filter to activate/deactivate
	 * @param active If true, activate the filter
	 */
	public void activateFilter(Filter f, boolean active) {
		if (f==null) {
			throw new IllegalArgumentException("Invalid null filter");
		}
		Integer pos = indexOf(f);
		if (pos==-1) {
			throw new IllegalArgumentException("The filter is not in the vector");
		}
		if (active) {
			if (!activeFilters.contains(pos)) {
				activeFilters.add(pos);
			}
		} else {
			activeFilters.remove(pos);
		}
	}

}

