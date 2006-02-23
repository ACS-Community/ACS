package com.cosylab.gui.components.r2.chart.accessories;

/**
 * This class is to test how many valid digits number has.
 */
public class ValidDigitTester {
/**
 * ValidDigitTester constructor comment.
 */
public ValidDigitTester() {
	super();
}
/**
 * This method return number of valid digits in tested double. 
 */
public static int doubleDigitTest(double number) {
//	System.out.println("Testing "+number);
	if (number==0) return 0;
	if (number<0) number*=-1;
	int pow=(int)Math.ceil(Math.log(number)/Math.log(10));
	double num=number/Math.pow(10,pow);
	if (num>=1) {
		num/=10;
		pow+=1;
	}
	int i, j=1, zeros=0;
	boolean end=false;
	long test;
//	double a;
//	double b;
	for (i = 1; !end ; i++){
//		a=num*Math.pow(10,i);
//		b=num*Math.pow(10,i-1);

//		System.out.println(i+" > "+a+" - "+b+" = "+(a-10.0*b)+" ("+((long)a-10*(long)b)+")");

		test= (long)(num*Math.pow(10,i))-10*(long)(Math.pow(10,i-1));
		
		if (test==1) {
			j=i;
			zeros=0;
		} else if (test==0) {
			zeros++;
		} else end=true;
		if (zeros>=6) end=true;			
	}
//	System.out.println("Result "+j);
	return j;
}
/**
 * This method return number of valid digits in tested int. 
 */
public static int integerDigitTest(int number) {
	if (number==0) return 0;
	if (number<0) number*=-1;
	int pow=(int)Math.ceil(Math.log(number)/Math.log(10));
	double num=number/Math.pow(10,pow);
	if (num>=1) {
		num/=10;
		pow+=1;
	}
	int i, j=1, zeros=0;
	boolean end=false;
	long test;
	for (i = 1; !end ; i++){
		test= (long)(num*Math.pow(10,i))-10*(long)(num*Math.pow(10,i-1));
		if (test==1) {
			j=i;
			zeros=0;
		} else if (test==0) {
			zeros++;
		} else end=true;
		if (zeros>=6) end=true;			
	}
	return j;
}
}
