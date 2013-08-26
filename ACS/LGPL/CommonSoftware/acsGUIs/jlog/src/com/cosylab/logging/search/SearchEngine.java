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
 * Created on Jul 8, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.cosylab.logging.search;

import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;

import alma.acs.logging.table.LogEntryTable;
import alma.acs.logging.table.LogTableDataModel;
import alma.acs.util.IsoDateFormat;

/**
 * @author acaproni
 *
 * Objects of this class search the logs for a string
 * The method that activates the search is find (overloaded)
 */
public class SearchEngine {
    /**
     * The reference to the table data model 
     */
    private LogTableDataModel logTableDataModel;
    
    /**
     * The JTable of the logs
     */
    private LogEntryTable logEntryTable;
    
    /**
     * The constructor
     * 
     * @param ltdm The LogTableDataModel of the main window
     */
    public SearchEngine(LogEntryTable let) {
        this.logEntryTable=let;
        this.logTableDataModel=let.getLCModel();
    }

    /**
     * Search the logs for a string
     * 
     * @param searchString The string to look for in the logs
     * @param caseSensitive If true performs a CaseSensitive search
     * @param wholeWord If true look for the whole word int the column
     * @param forwardSearch If true search forward otherwise backward
     * @param cols The columns of each log tool for the string
     * @return -1 if no log is found otherwise the row containing the log 
     */
    public int find(
            String searchString, 
            boolean caseSensitive, 
            boolean wholeWord, 
            boolean forwardSearch,
            boolean[] cols) {
        
        return find(null,searchString,caseSensitive,wholeWord,forwardSearch,cols);
    }
 
    /**
     * Search the log for a regular expression
     * 
     * @param regExp The regular expression to look for in the logs
     * @param forwardSearch If true search forward otherwise backward
     * @param cols The columns of each log tool for the string
     * @return -1 if no log is found otherwise the row containing the log
     */
    public int find (
            Pattern regExp,
            boolean forwardSearch,
            boolean[] cols) {
        return find(regExp,null,true,true,forwardSearch,cols);
    }
    
    /**
     * The method executes the search for both the public overloaded 
     * find methods.
     * 
     * @param regExp The regular expression to look for in the logs
     *               null if the method search for a string 
     * @param searchString The string to look for in the logs
     *                     null if the method search for a reg exp
     * @param caseSensitive If true performs a CaseSensitive search
     *                      Ignored for reg exp searchs (it is coded
     *                      in the Pattern)
     * @param wholeWord If true look for the whole word int the column
     *                  Ignored for reg exp searchs
     * @param forwardSearch If true search forward otherwise backward
     * @param cols The columns of each log tool for the string
     * @return -1 if no log is found otherwise the row containing the string/reg exp
     */
    private int find(
            Pattern regExp,
            String searchString, 
            boolean caseSensitive, 
            boolean wholeWord,
            boolean forwardSearch,
            boolean[] cols) {
        // I want only one loop for both forward and backward searches
        // For that I calculate the interval of valid rows to scan
        // [a,b] where a<=b i.e. this interval is independedent from the
        // direction of the search
        // Then I use a cursor to navigate the rows, decreasing or 
        // increasing its value depending of the direction of the search
        
        // Get the starting and the ending row for the search
        int startingRow=getStartingRow(forwardSearch);
        int endRow = (forwardSearch)?logEntryTable.getRowCount()-1:0;
        // The position where the SimpleDate must be written
        FieldPosition pos = new java.text.FieldPosition(0);
        // A temporary buffer
        StringBuffer tempSB = new StringBuffer();
        // The variable used to browse the rows
        int cursor = startingRow;
        // Order end and start rown in growing order 
        if (endRow<startingRow) {
            int temp=startingRow;
            startingRow=endRow;
            endRow=temp;
        }
        
        int foundRow=-1;
        
        while (cursor>=startingRow && cursor<=endRow && foundRow==-1) {
        	// cols contains one entry for each field of a log entry
        	// plus one entry for the additional data
        	ILogEntry log = logTableDataModel.getVisibleLogEntry(logEntryTable.convertRowIndexToModel(cursor));
        	String string=null; // The value of the field
        	for (int t=0; t<cols.length-1; t++) {
        		Object obj = log.getField(LogField.values()[t]);
        		if (obj==null) {
        			continue;
        		}
        		if (cols[t]) {
        			switch (LogField.values()[t]) {
        				case TIMESTAMP: {
        					SimpleDateFormat df = new IsoDateFormat();
        					Date dt = new Date((Long)obj);
        					tempSB.delete(0,tempSB.length());
        					df.format(dt,tempSB,pos);
        					string=tempSB.toString();
        					break;
        				} 
        				case ENTRYTYPE:        					
        				case LINE: 
        				case PRIORITY: 
        				case STACKLEVEL: {
        					string=obj.toString();
        					break;
        				}
        				default: {
        					string = obj.toString();
        					break;
        				}
        			}
        			if (matches(string,regExp,searchString,caseSensitive,wholeWord) ) {
                        if ((forwardSearch && cursor!=startingRow) || (!forwardSearch && cursor!=endRow)) { 
                            foundRow=cursor;
                            if (forwardSearch) {
                                cursor++;
                            } else {
                                cursor--;
                            }
                            return foundRow;
                        }
                    }
        		}
        	}
        	// Look into the additional data: here we have to search for each key and value
        	// (we can't build a big striung with everything inside because it would fail
        	// searching for regular espressions
            if (cols[cols.length-1] && log.hasDatas()) {
            	Vector<ILogEntry.AdditionalData> addData = log.getAdditionalData();
            	for (int t=0; t<addData.size(); t++) {
            		ILogEntry.AdditionalData data = addData.elementAt(t);
            		string = data.name;
            		if (matches(string,regExp,searchString,caseSensitive,wholeWord) ) {
                        if ((forwardSearch && cursor!=startingRow) || (!forwardSearch && cursor!=endRow)) { 
                            foundRow=cursor;
                            if (forwardSearch) {
                                cursor++;
                            } else {
                                cursor--;
                            }
                            return foundRow;
                        }
                    }
            		string = data.value;
            		if (matches(string,regExp,searchString,caseSensitive,wholeWord) ) {
                        if ((forwardSearch && cursor!=startingRow) || (!forwardSearch && cursor!=endRow)) { 
                            foundRow=cursor;
                            if (forwardSearch) {
                                cursor++;
                            } else {
                                cursor--;
                            }
                            return foundRow;
                        }
                    }
            	}
            }
        	
            if (forwardSearch) {
                cursor++;
            } else {
                cursor--;
            }
        }
        return foundRow;
    }
    
    /**
     * Check if the given object matches the search criteria
     * 
     * @param obj The object to check (it should be a string or a way to convert
     *            the obj to a string must be known)
     * @param regExp The regular expression to look for in the logs
     *               null if the method search for a string 
     * @param searchString The string to look for in the logs
     *                     null if the method search for a reg exp
     * @param caseSensitive If true performs a CaseSensitive search
     *                      Ignored for reg exp searchs (it is coded
     *                      in the Pattern)
     * @param wholeWord If true look for the whole word int the column
     *                  Ignored for reg exp searchs
     * @return true if the object matches the serach criteria
     */
    private boolean matches(Object obj,
            Pattern regExp,
            String searchString, 
            boolean caseSensitive, 
            boolean wholeWord) {
        if (obj==null) {
            return false;
        }
        
        // Convert the object to a String
        String str;
        try {
            str = obj.toString();
        } catch (Exception e) {
            // This should never happen but...
            System.err.println("Impossible to convert this object to a String: "+obj);
            return false;
        }
        
        if (regExp==null) {
            // Check the strings
            if (wholeWord) {
                if (caseSensitive) {
                    return str.compareTo(searchString)==0;
                } else {
                    return str.compareToIgnoreCase(searchString)==0;
                }
            } else {
                if (caseSensitive) {
                    return str.indexOf(searchString)!=-1;
                } else {
                    String upperStr=str.toUpperCase();
                    String searchStrUpper=searchString.toUpperCase();
                    return upperStr.indexOf(searchStrUpper)!=-1;
                }
            }
        } else {
            // Check the regular expression
            Matcher matcher = regExp.matcher(str);
            return matcher.matches();
        }
    }
    
    /**
     * Get the starting row number for a search.
     * 
     * @return The selected row in the table of the main window or 
     *         the first/last row if no row is selected by the user 
     *         (depending if the search is backward or forward)
     */
    private int getStartingRow(boolean forward) {
        int ret = logEntryTable.getSelectedRow();
        if (ret==-1) {
            if (forward) {
                ret=0;
            } else {
                ret=logEntryTable.getRowCount()-1;
            }
        }
        return ret;
    }
}
