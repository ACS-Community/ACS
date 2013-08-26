package com.cosylab.gui.components.r2.chart.accessories;

import java.lang.Math;
/**
 * This type is to round double type. It cut off a few less significant digits.
 */
public class RoundDoubleToDouble {
/**
 * RoundDoubleToDouble constructor comment.
 */
public RoundDoubleToDouble() {
	super();
}
/**
 * This method round <code>number</code> in this manner that digits that represent
 * the same order as <code>formatNumberForRounding</code> are rounded as method
 * roundToDigits(double, int) do it.

 * <code>roundToDigits(123456,123,1) return 123500</code>
 * <code>roundToDigits(123416,123,1) return 123400</code>
 
 * @return double
 * @param number double
 * @param numberOfDigits int
 * @param formatNumberForRounding double
 */
public static double roundToDigits(double number, double formatNumberForRounding, int numberOfDigits) throws IllegalArgumentException {
	if (number==0) return 0;
	boolean positiveNumber;
	if (number<0) {
		number*=-1;
		positiveNumber=false;
	} else positiveNumber=true;	
	int n;
	if (number>=1) n=(int)(Math.log(number)/Math.log(10));
	else n=-(int)Math.ceil(-Math.log(number)/Math.log(10));
	int n1;
	if (formatNumberForRounding>=1) n1=(int)(Math.log(formatNumberForRounding)/Math.log(10));
	else n1=-(int)Math.ceil(-Math.log(formatNumberForRounding)/Math.log(10));
	
	try {
		if ((n=numberOfDigits+n-n1)<=0) {
			if (number< Math.pow(10,n1-numberOfDigits+1)/2) return 0;
			else if (positiveNumber) return Math.pow(10,n1-numberOfDigits+1);
				else return -Math.pow(10,n1-numberOfDigits+1);
		}
		
		if (positiveNumber) {
			return roundToDigits(number,n);
		} else return roundToDigits(-number,n);
	} catch (IllegalStateException e) {
		throw e;
	}
}
/**
 * This method not just cut off parameter specified number of less significant digits.
 * It adds 1 to last not-cut digit if most significant cut digits was 5 or greater.
 * @return double
 * @param number double
 * @param numberOfDigits int
 */
public static double roundToDigits(double number, int numberOfDigits) throws IllegalArgumentException {
	if (number==0 || numberOfDigits==0) return 0;
	boolean positiveNumber;
	if (number<0) {
		number*=-1;
		positiveNumber=false;
	} else positiveNumber=true;	
	if (ValidDigitTester.doubleDigitTest(number)<=numberOfDigits) {
		if (positiveNumber) return number;
		else return -number;
	}
	if (numberOfDigits<0) throw new IllegalArgumentException("Number of digits to round must be positive integer");
	int pow=(int)Math.ceil(Math.log(number)/Math.log(10));
	double num=number/Math.pow(10,pow);
	num*=Math.pow(10,numberOfDigits);
	pow-=numberOfDigits;	
	if (positiveNumber) return Math.pow(10,pow)*Math.rint(num);
	else return -Math.pow(10,pow)*Math.rint(num);
}
/**
 * This method round <code>number</code> in this manner that digits that represent
 * the same order as <code>formatNumberForRounding</code> are rounded as method
 * roundUpToDigits(double, int) do it.

 * <code>roundToDigits(123456,123,1) return 123500</code>
 * <code>roundToDigits(123416,123,1) return 123500</code>
  
 * @return double
 * @param number double
 * @param numberOfDigits int
 * @param formatNumberForRounding double
 */
public static double roundUpToDigits(double number, double formatNumberForRounding, int numberOfDigits) throws IllegalArgumentException {
	if (number==0) return 0;
	boolean positiveNumber;
	if (number<0) {
		number*=-1;
		positiveNumber=false;
	} else positiveNumber=true;	
	int n;
	if (number>=1) n=(int)(Math.log(number)/Math.log(10));
	else n=-(int)Math.ceil(-Math.log(number)/Math.log(10));
	int n1;
	if (formatNumberForRounding>=1) n1=(int)(Math.log(formatNumberForRounding)/Math.log(10));
	else n1=-(int)Math.ceil(-Math.log(formatNumberForRounding)/Math.log(10));
	
	try {
		if ((n=numberOfDigits+n-n1)<=0) if (positiveNumber) return Math.pow(10,n1-numberOfDigits+1);
										else return 0;
		
		if (positiveNumber) {
			return roundUpToDigits(number,n);
		} else return roundUpToDigits(-number,n);
	} catch (IllegalArgumentException e) {
		throw e;
	}
}
/**
 * This method not just cut off parameter specified number of less significant digits.
 * Rounded value is equall or greather than old value.
 * @return double
 * @param number double
 * @param numberOfDigits int
 */
public static double roundUpToDigits(double number, int numberOfDigits) throws IllegalArgumentException {
	if (number==0 || numberOfDigits==0) return 0;
	boolean positiveNumber;
	if (number<0) {
		number*=-1;
		positiveNumber=false;
	} else positiveNumber=true;	
	if (ValidDigitTester.doubleDigitTest(number)<=numberOfDigits) {
		if (positiveNumber) return number;
		else return -number;
	}
	if (numberOfDigits<0) throw new IllegalArgumentException("Number of digits to round must be positive integer");
	int pow=(int)Math.ceil(Math.log(number)/Math.log(10));
	double num=number/Math.pow(10,pow);
	num*=Math.pow(10,numberOfDigits);
	pow-=numberOfDigits;	
	if (positiveNumber) return Math.pow(10,pow)*Math.ceil(num);
	else return -Math.pow(10,pow)*((int)(num));
}
/**
 * This method round parameter specified number to power of ten, which
 * is first, greather than it. 
 */
public static double roundUpToTenth(double number) {
	boolean negative=false;
	if (number<0) {
		number*=-1;
		negative=true;
	}
	if (number>1) {
		int pow=(int)(Math.ceil(Math.log(number)/2.3025850929940456840));
		if (negative) return -Math.pow(10,pow);
		else return Math.pow(10,pow);
	}
	else {
		int pow=(int)(Math.log(number)/2.3025850929940456840);
		if (negative) return -Math.pow(10,pow);
		else return Math.pow(10,pow);
	}
}
/**
 * This method round <code>number</code> in this manner that digits that represent
 * the same order as <code>formatNumberForRounding</code> are rounded as method
 * roundUpToDigits(double, int) do it.

 * <code>roundToDigits(123456,123,1) return 123400</code>
 * <code>roundToDigits(123416,123,1) return 123400</code>
  
 * @return double
 * @param number double
 * @param numberOfDigits int
 * @param formatNumberForRounding double
 */
public static double truncToDigits(double number, double formatNumberForRounding, int numberOfDigits) throws IllegalArgumentException {
	if (number==0) return 0;
	boolean positiveNumber;
	if (number<0) {
		number*=-1;
		positiveNumber=false;
	} else positiveNumber=true;	
	int n;
	if (number>=1) n=(int)(Math.log(number)/Math.log(10));
	else n=-(int)Math.ceil(-Math.log(number)/Math.log(10));
	int n1;
	if (formatNumberForRounding>=1) n1=(int)(Math.log(formatNumberForRounding)/Math.log(10));
	else n1=-(int)Math.ceil(-Math.log(formatNumberForRounding)/Math.log(10));
	
	try {
		if ((n=numberOfDigits+n-n1)<=0) if (!positiveNumber) return -Math.pow(10,n1-numberOfDigits+1);
										else return 0;
		
		if (positiveNumber) {
			return truncToDigits(number,n);
		} else return truncToDigits(-number,n);
	} catch (IllegalArgumentException e) {
		throw e;
	}
}
/**
 * This method just cut off parameter specified number of less significant digits.
 * @return double
 * @param number double
 * @param numberOfDigits int
 */
public static double truncToDigits(double number, int numberOfDigits) throws IllegalArgumentException{
	if (number==0 || numberOfDigits==0) return 0;
	boolean positiveNumber;
	if (number<0) {
		number*=-1;
		positiveNumber=false;
	} else positiveNumber=true;	
	if (ValidDigitTester.doubleDigitTest(number)<=numberOfDigits) {
		if (positiveNumber) return number;
		else return -number;
	}
	if (numberOfDigits<0) throw new IllegalArgumentException("Number of digits to round must be positive integer");
	int pow=(int)Math.ceil(Math.log(number)/Math.log(10));
	double num=number/Math.pow(10,pow);
	num*=Math.pow(10,numberOfDigits);
	pow-=numberOfDigits;	
	if (positiveNumber) return Math.pow(10,pow)*((int)(num));
	else return -Math.pow(10,pow)*Math.ceil(num);
}
/**
 * This method round parameter specified number to power of ten, which
 * is first, lower than it. 
 */
public static double truncToTenth(double number) {
	boolean negative=false;
	if (number<0) {
		number*=-1;
		negative=true;
	}
	int pow=(int)(Math.floor(Math.log(number)/2.3025850929940456840));
	if (negative) return -Math.pow(10,pow);
	else return Math.pow(10,pow);
}
}
