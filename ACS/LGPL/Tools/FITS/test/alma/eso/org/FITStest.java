/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
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
 * Created on Apr 14, 2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package alma.eso.org;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.Enumeration;

import org.eso.fits.*;

/**
 * @author acaproni
 *
 * The class to test the jfits library
 * 
 * It has the same functionalities of the python program to test the python
 * FITS library
 * 
 */
public class FITStest {
	/**
	 * The stream for stdin
	 */
	private BufferedReader inStream;
	
	/**
	 * The name of the fits file (read from the command line)
	 */
	private String imageName;
	
	/**
	 * The FITS file
	 */
	FitsFile file;
	
	/**
	 * The starting point of the project
	 * 
	 * @param args Command line args
	 */
	public static void main(String[] args) {
		if (args.length!=1) {
			System.out.println("USAGE:");
			System.out.println("FITStest <img>");
			System.exit(-1);
		}
		
		new FITStest(args[0]);
	}
	
	/**
	 * Constructor
	 *
	 * @param imgName The name of the FITS (read from command line)
	 */
	FITStest(String imgName) {
		// Join the stdin
		inStream = new BufferedReader(new InputStreamReader(System.in));
		// Open the FITS file
		try {
			file = new FitsFile(imgName);
		} catch (FitsException fe) {
			System.err.println("Error opening "+imgName);
			System.err.println("FITS exception: "+fe.toString());
			System.exit(-1);
		} catch (IOException ioe) {
			System.err.println("Error opening "+imgName);
			System.err.println("IO exception: "+ioe.toString());
			System.exit(-1);
		}
		// Print some info about the headers
		int noHDU = file.getNoHDUnits();
	    System.out.println("FITS file has " + noHDU + " headers");
	    for (int t=0; t<noHDU; t++) {
	    	FitsHDUnit hdu = file.getHDUnit(t);
			FitsHeader hdr = hdu.getHeader();
			System.out.print("Header "+t+": Name = "+hdr.getName()+"; ");
			System.out.print("Type = "+hdr.getType()+"; Data size = "+hdr.getDataSize());
			System.out.println("; Num of keys = "+hdr.getNoKeywords());
	    }
	    // Print some info about the data
	    printImageInfo();
		// Enters the loop
		commandLoop();
	}
	
	/**
	 * Print keywords and values of the specified header
	 * 
	 * @param hdrNum The number of the header
	 */
	private void printHeader(int hdrNum) {

		FitsHDUnit hdu = file.getHDUnit(hdrNum);
		FitsHeader hdr = hdu.getHeader();
		int noKw = hdr.getNoKeywords();
		int type = hdr.getType();
		int size = (int) hdr.getDataSize();
		System.out.println("HEADER " + hdrNum);
		System.out.println("\theader name " + hdr.getName()); 
		System.out.println("\theader type " + Fits.getType(type));
		System.out.println("\tnumber of keywords in the header: " + noKw);
		System.out.println("\tsyze of the header: " + size + " bytes");
		System.out.println("   Keywords:");
		Enumeration enum1 = hdr.getKeywords();
		while (enum1.hasMoreElements()) {
		    FitsKeyword kw = (FitsKeyword) enum1.nextElement();
		    System.out.print("     " + kw.getName());
		    switch (kw.getType()) {
		    	case FitsKeyword.COMMENT:
		    		System.out.print("(C) " + kw.getComment());
		    	break;
		    	case FitsKeyword.STRING:
		    		System.out.print("(S)= '" + kw.getString() + "'");
		    	break;
		    	case FitsKeyword.BOOLEAN:
		    		System.out.print("(B)= " + kw.getBool());
		    	break;
		    	case FitsKeyword.INTEGER:
		    		System.out.print("(I)= " + kw.getInt());
		    	break;
		    	case FitsKeyword.REAL:
		    		System.out.print("(R)= " + kw.getReal());
		    	break;
		    	case FitsKeyword.DATE:
		    		System.out.print("(D)= " + kw.getString());
		    	break;
		    	default:
		    	}
		    if (0<kw.getComment().length() && (kw.getType()!=FitsKeyword.COMMENT)) {
		    	System.out.print(" / " + kw.getComment());
		    }
		    System.out.println();
		}

		if (type == Fits.IMAGE) {
		    System.out.println("\n  Check data matrix " + "- compute mean and rms");
		    FitsMatrix dm = (FitsMatrix) hdu.getData();
		    int naxis[] = dm.getNaxis();
		    double crval[] = dm.getCrval();
		    double crpix[] = dm.getCrpix();
		    double cdelt[] = dm.getCdelt();

		    System.out.println("  Dimension of matrix: "+ naxis.length);
		    for (int n=0; n<naxis.length; n++) {
		    	System.out.println("   Axis " + n + ": " + naxis[n]
					   + ",  " + crpix[n] + ",  "
					   + crval[n] + ",  " + cdelt[n]);
		    }
		    System.out.println("\n");

		    int nv, off, npix;
		    int nval = dm.getNoValues();
		    if (0<nval) {
			int ncol = naxis[0];
			int nrow = nval/ncol;
			System.out.println(" Npixel,row,col: " + nval+ ", " + nrow + ", " + ncol);
			float data[] = new float[ncol];
			double mean, rms, val;

			off = nv = npix = 0 ;
			mean = rms = 0.0;
			long time = System.currentTimeMillis();
			for (int nr=0; nr<nrow; nr++) {
			    try {
			    	dm.getFloatValues(off, ncol, data);
			    	for (int n = 0; n<ncol; n++) {
			    		val = data[n];
			    		npix++;
			    		mean += val;
			    		rms  += val*val;
			    	}
			    } catch (FitsException e) {
			    }
			
			    off += ncol;
			}
			mean = mean/npix;
			rms  = rms/npix - mean*mean;
			rms = ((0.0<rms) ? Math.sqrt(rms) : 0.0);
			float dtime =
			    (float) (1000.0*(System.currentTimeMillis()-time)/
				     ((double) nval));
			System.out.println("  Mean: " + (float)mean +
					   ", rms: " + (float)rms +
					   ", Time: " + dtime
					   + " S/Mp, Pixels: " + npix);
		    }
		} else if (type==Fits.BTABLE || type==Fits.ATABLE) {
		    System.out.println("\n  Check table data - list columns");
		    FitsTable dm = (FitsTable) hdu.getData();
		    int nrow = dm.getNoRows();
		    int ncol = dm.getNoColumns();
		    FitsColumn col[] = new FitsColumn[ncol];
		    System.out.println("  Columns: " + ncol 
				       + ", Rows: " + nrow);
		    for (int n=0; n<ncol; n++) {
		    	col[n] = dm.getColumn(n);
		    	System.out.print("  " + n + " >"
					 + col[n].getLabel() + "<, ");
		    	System.out.print(col[n].getRepeat() + " ");
		    	System.out.print(col[n].getDataType() + ", >");
		    	System.out.print(col[n].getDisplay() + "<, >");
		    	System.out.println(col[n].getUnit() + "<");

		    	if (col[n].getDataType() == 'F'
		    		|| col[n].getDataType() == 'E'
		    			|| col[n].getDataType() == 'D') {
		    		int npix = 0;
		    		double mean, rms, val;
		    		mean = rms = 0.0;
		    		long time = System.currentTimeMillis();
		    		for (int nr=0; nr<nrow; nr++) {
		    			val = col[n].getReal(nr);
		    			if (Double.isNaN(val)) continue;
		    			npix++;
		    			mean += val;
		    			rms  += val*val;
		    		}
		    		float dtime =
		    			(float) (1000.0*(System.currentTimeMillis()-time)/((double) nrow));
		    		mean = mean/npix;
		    		rms  = rms/npix - mean*mean;
		    		rms = ((0.0<rms) ? Math.sqrt(rms) : 0.0);
		    		System.out.println("      no,mean,rms: " + npix
					       + ", " + (float)mean + ", "
					       + (float)rms + "; "
					       + dtime + " S/Mp");
		    	} else if (col[n].getDataType() == 'I'
				   || col[n].getDataType() == 'J'
				   || col[n].getDataType() == 'B') {
		    		int npix = 0;
		    		double mean, rms, val;
		    		mean = rms = 0.0;
		    		long time = System.currentTimeMillis();
		    		for (int nr=0; nr<nrow; nr++) {
		    			val = col[n].getInt(nr);
		    			if (val == Long.MIN_VALUE) continue;
		    			npix++;
		    			mean += val;
		    			rms  += val*val;
		    		}
		    		float dtime =
		    			(float) (1000.0*(System.currentTimeMillis()-time)/((double) nrow));
		    		mean = mean/npix;
		    		rms  = rms/npix - mean*mean;
		    		rms = ((0.0<rms) ? Math.sqrt(rms) : 0.0);
		    		System.out.println("      no,mean,rms: " + npix
					       + ", " + (float)mean + ", "
					       + (float)rms + "; "
					       + dtime + " S/Mp");
		    	}
		   }
		}
	}
	
	/**
	 * Print keywords and values of all the headers
	 *
	 */
	private void printHeaders() {
		int noHDU = file.getNoHDUnits();
	    System.out.println("FITS file has " + noHDU + " HDUnits");

	    for (int i=0; i<noHDU; i++) {
	    	printHeader(i);
	    }
	}
	
	/**
	 * Print some info about DATA
	 *
	 */
	private void printImageInfo() {
		int noHDU = file.getNoHDUnits();
	    for (int i=0; i<noHDU; i++) {
	    	FitsHDUnit hdu = file.getHDUnit(i);
	    	FitsData data=hdu.getData();
	    	if (data!=null) {
	    		System.out.print("DATA for hdr "+i+": num. of axes:"+data.getNoAxes());
	    		if (data.getNoAxes()>0) {
	    			System.out.print(' ');
	    			// Get the dimensions of the axes
	    			int dimAxes[]=data.getNaxis();
	    			System.out.print('[');
	    			for (int j=0; j<data.getNoAxes(); j++) {
	    				if (j>0) {
	    					System.out.print('x');
	    				}
	    				System.out.print(""+dimAxes[j]);
	    			}
	    			System.out.print(']');
	    		}
	    		System.out.println(" type: "+data.getType());
	    	}
	    }
	}
	
	/** 
	 * Get the value of a keyword
	 * 
	 * @param name The name of the keyword
	 * @return null if the keyword is not found, otherwise a String with 
	 * 		its value and comment 
	 */
	private String getKeyword(String name) {
		// The value of the keyword (if any)
		String value=null;
		// The comment of the keyword (if any)
		String comment=null;
		
		// Looks for the keyword in all the headers
		for (int i=0; i<file.getNoHDUnits(); i++) {
			FitsHDUnit hdu = file.getHDUnit(i);
			FitsHeader hdr = hdu.getHeader();
			FitsKeyword kw=hdr.getKeyword(name.toUpperCase());
			if (kw!=null) {
				// Keyword found!
				value=kw.getString();
				comment=kw.getComment();
				System.out.println("Key "+name.toUpperCase()+" found on hdr "+i);
				break;
			}
		}
		
		if (value==null && comment==null) {
			return null;
		} else {
			if (value==null) value="";
			if (comment==null) comment="";
			return value+" / "+comment;
		}
	}
	
	/**
	 * Delete a keyword
	 * It deletes the first occurrence of the keyword it founds starting
	 * from FITS header 0
	 *  
	 * @param name The name of the keyword
	 * @return true if the keyword is found and deleted
	 */
	private boolean deleteKeyword(String name) {
		// Scans the headers to find the keyword
		for (int hdrNum=0; hdrNum<file.getNoHDUnits(); hdrNum++) {
			int pos = getKeywordPosition(hdrNum,name);
			if (pos!=-1) { // Keyword found at pos position
				FitsHDUnit hdu = file.getHDUnit(hdrNum);
				FitsHeader hdr = hdu.getHeader();
				hdr.removeKeywordAt(pos);
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Update a keyword
	 * If the keyword doesn't exist then adds the key to header 0
	 * If more then one key exists it updates the first one
	 * 
	 * @param key The keyword
	 * @param value The value of the keyword
	 * @param comment The comment of the keyword
	 */
	private void updateKeyword(String key, String value, String comment) {
		// Build the FitsKeyword
		FitsKeyword kw = new FitsKeyword(key,value,comment);
		updateKeyword(kw);
	}
	
	/**
	 * Update a keyword
	 * If the keyword doesn't exist then adds the key to header 0
	 * If more then one key exists it updates the first one
	 * 
	 * @param key The FitsKeyword
	 * 
	 */
	private void updateKeyword(FitsKeyword key) {
		// Try to find the keyword in a header
		for (int hdrNum=0; hdrNum<file.getNoHDUnits(); hdrNum++) {
			int pos = getKeywordPosition(hdrNum,key.getName());
			if (pos!=-1) { // Keyword found at pos position
				FitsHDUnit hdu = file.getHDUnit(hdrNum);
				FitsHeader hdr = hdu.getHeader();
				hdr.removeKeywordAt(pos);
				hdr.insertKeywordAt(key,pos);
				return;
			}
		}
		// The key doesnt exist: add the keyword to header 0
		FitsHDUnit hdu = file.getHDUnit(0);
		FitsHeader hdr = hdu.getHeader();
		hdr.addKeyword(key);
	}
	
	/**
	 * Return the position of a keyword
	 * 
	 * @param header The header to search for the keyword
	 * @param name The name of the keyword
	 * @return The position of the keyword or -1 if the keyword doesn't exist
	 */
	private int getKeywordPosition(int header, String name){
		FitsHDUnit hdu = file.getHDUnit(header);
		FitsHeader hdr = hdu.getHeader();
		// Check if the keyword exists
		FitsKeyword kw=hdr.getKeyword(name.toUpperCase());
		if (kw==null) return -1;
		// Scans the list of the keyword
		Enumeration enum1 = hdr.getKeywords();
		int pos=-1;
		while (enum1.hasMoreElements()) {
			pos++;
		    kw = (FitsKeyword) enum1.nextElement();
		    if (kw.getName().trim().compareToIgnoreCase(name.trim())==0) {
		    	// Keyword found
		    	return pos;
		    }
		}
		return -1;
	}
	
	/**
	 * Process the command. 
	 * Valid commands are:
	 * 	read keyword
	 * 	delete keyword
	 * 	readall
	 * 	update keyword = va;ue / comment
	 * 	save filename
	 * 
	 * @param cmd The command read from stdin
	 */
	private void processCommand(String cmd) {
		String words[]=cmd.trim().split("\\s+");
		if (words[0].compareToIgnoreCase("READ")==0) {
			if (words.length!=2) {
				System.err.println("Invalid command: "+cmd);
				System.err.println("Use: READ <keyword>");
				return;
			}
			String valueAndComment=getKeyword(words[1]);
			if (valueAndComment==null) {
				System.out.println("Keyword "+words[1]+" NOT found");
			} else {
				System.out.println(words[1].toUpperCase()+" = "+valueAndComment.toUpperCase());
			}
			return;
		} else if (words[0].compareToIgnoreCase("DELETE")==0) {
			if (words.length!=2) {
				System.err.println("Invalid command: "+cmd);
				System.err.println("Use: DELETE <keyword>");
				return;
			}
			if (deleteKeyword(words[1])) {
				System.out.println(words[1].trim().toUpperCase()+" deleted");
			} else {
				System.out.println(words[1].trim().toUpperCase()+" NOT found");
			}
			return;
		} else if (words[0].compareToIgnoreCase("READALL")==0) {
			if (words.length!=1) {
				System.err.println("Invalid command: "+cmd);
				System.err.println("Use: READALL");
				return;
			}
			printHeaders();
			return;
		} else if (words[0].compareToIgnoreCase("UPDATE")==0) {
			String myCmd=cmd.toUpperCase().replaceFirst("UPDATE","").trim();
			String keyName="";
			String value="";
			String comment="";
			// Extract name, value and comment of the keyword from cmd
			int equalPos=myCmd.indexOf("=");
			if (equalPos!=-1) {
				keyName=myCmd.substring(0,equalPos).trim();
			} else {
				System.err.println("Invalid command: "+cmd);
				System.err.println("Use: UPDATE KEY = VALUE / COMMENT");
				return;
			}
			String valueAndComment=myCmd.substring(equalPos+1);
			int separatorPos=valueAndComment.indexOf('/');
			if (separatorPos!=-1) {
				value = valueAndComment.substring(0,separatorPos).trim();
				comment=valueAndComment.substring(separatorPos+1).trim();
			} else {
				System.err.println("Invalid command: "+cmd);
				System.err.println("Use: UPDATE KEY = VALUE / COMMENT");
				return;
			}
			updateKeyword(keyName,value,comment);
			return;
		} else if (words[0].compareToIgnoreCase("SAVE")==0) {
			if (words.length!=2) {
				System.err.println("Invalid command: "+cmd);
				System.err.println("Use: SAVE <filename>");
				return;
			}
			try {
				file.writeFile(words[1]);
			} catch (FitsException fe) {
				System.err.println("Error saving "+words[1]);
				System.err.println("\tFitsException: "+fe.getMessage());
			} catch (IOException ioe) {
				System.err.println("Error saving "+words[1]);
				System.err.println("\tIOException: "+ioe.getMessage());
			}
			return;
		} else if (words[0].compareToIgnoreCase("HELP")==0 || words[0].compareTo("?")==0) {
			System.out.println("Available commands commands:");
			System.out.println("\tREAD KEY");
			System.out.println("\tREADALL");
			System.out.println("\tUPDATE KEY = VALUE / COMMENT");
			System.out.println("\tDELETE KEY");
			System.out.println("\tSAVE FILENAME");
			System.out.println("\tHELP");
			System.out.println("\t?");
			System.out.println("\tQUIT");
			System.out.println("Use EOF or quit to terminate");
			return;
		} else if (words[0].compareToIgnoreCase("QUIT")==0) {
		} else {
			System.err.println("? Unrecognized command: "+words[0]+" ?");
			System.err.println("Use help or ? for help");
			return;
		}			
	}
	
	/**
	 * The command loop: read and executes command
	 * The commands are read from inStream (stdin) and executed
	 * on the FITS file passed on the command line 
	 *
	 */
	private void commandLoop() {
		String line="";
		do {
			System.out.print("> ");
			try {
				line = inStream.readLine();
			} catch (Exception e) {
				System.err.println("Error:\n"+e.toString());
				System.exit(-1);
			}
			// Execute the command
			processCommand(line.trim());			
		} while (line.trim().compareToIgnoreCase("QUIT")!=0);
	}
}
