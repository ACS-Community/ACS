/*
 * Created on Mar 4, 2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package antMountGUI;

import java.util.Vector;

/**
 *  A curve stroke
 * 
 * @author acaproni
 *
 * @version 1.0
 */
public class CurveStroke extends Stroke {
	
	/**
	 * Build a curve stroke
	 * 
	 * @param time The time
	 * @param orientation The orientation
	 * @param theArray The array
	 * 
	 * @pre theArray!=null
	 */
	public CurveStroke(double time, double orientation, double[][] theArray) {
		super (time,orientation);
		this.array=theArray;
	}
	
	/**
	 * Set the array
	 * 
	 * @param newArray The new array
	 * 
	 * @pre newArray!=null
	 */
	public void setArray(double[][] newArray) {
		array=newArray;
	}
	
	/**
	 * Create the array with the values stored inthe parameter (useful link to the DefaultTableModel
	 * used in the GUI
	 * 
	 * @param values The vector of vector of strings to store into the array
	 * 
	 * @pre values!=null && values.$forall(Vector v; v!=null)
	 * 
	 * @post array.length==values.size()
	 * @post (array[0].length)==((Vector)values.elementAt(0)).size() 		
	 */
	public void setArray(Vector values) {
		/** @assert values!=null */
		int t;
		Vector rowVector;
		array = new double[values.size()][3];
		for (t=0; t<values.size(); t++) {
			/** @assert values.elementAt(t)!=null */
			rowVector=(Vector)values.elementAt(t);
			setRow(t,rowVector);
		}
	}
	
	/**
	 * Fill a row decoding the vector of strings (useful link to the DefaultTableModel
	 * used in the GUI
	 *  
	 * @param row The index of the row
	 * @param values The vector of (string) values to insert into the row
	 * 
	 * &pre values!=null && values.$forall(String s; s!=null)
	 * 
	 */
	public void setRow(int row, Vector values) {
		int t=0;
		for (t=0; t<3; t++) {
			array[row][t]=Double.parseDouble((String)values.elementAt(t));
		}
	}
		
	/** 
	 * Set an item of the array in position [row,col] decoding the string
	 * 
	 * @param row The row 
	 * @param col The column
	 * @param newVal The stringfied representation of the double
	 * 
	 * @pre	row>=0 && row<array.length
	 * @pre col>=0 && col<array[0].length
	 * @pre	newVal!=null
	 */
	public void setArrayItem(int row, int col, String newVal) {
		array[row][col]=Double.parseDouble(newVal);
	}
	
	/** 
	 * Set an item of the array in position [row,col]
	 * 
	 * @param row The row 
	 * @param col The column
	 * @param newVal The new value
	 * 
	 * @pre	row>=0 && row<array.length
	 * @pre	col>=0 && col<array[0].length
	 *  
	 */
	public void setArrayItem(int row, int col, double newVal) {
		array[row][col]=newVal;
	}
	
	/**
	 * Return the value of an item of the array 
	 * 
	 * @param row The row of the item
	 * @param col The column of the item
	 * 
	 * @return The value of an item of the array
	 * 
	 * @pre	row>=0 && row<array.length
	 * @pre col>=0 && col<array[0].length
	 */
	public double getArrayItem(int row, int col) {
		return array[row][col];
	}
	
	/**
	 * 
	 * @return The array
	 */
	public double[][] getArray() {
			return array;
	}
	
	/**
	 * 
	 * @return The array as array of String objects (useful for the DefaultTableModel used by the GUI)
	 * 
	 * @post $result.length==array.length
	 * @post $result[0].length==array[0].length
	 */
	public String[][] getArrayAsStrings() {
		String[][] temp=new String[array.length][array[0].length];
		int r,c;
		for (r=0; r<array.length; r++) {
			for (c=0; c<array[0].length; c++)
				temp[r][c]=new String(Double.toString(array[r][c]));
		}
		return temp;
	}
	
	/**
	 *  The array (xArray,yArray,time)
	 */
	private double[][] array=null;
}
