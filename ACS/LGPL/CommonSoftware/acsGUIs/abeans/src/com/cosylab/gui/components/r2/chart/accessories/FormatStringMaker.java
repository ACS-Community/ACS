package com.cosylab.gui.components.r2.chart.accessories;

/**
 * This type is useful for outputing numbers as a strings. 
 */
public class FormatStringMaker {
/**
 * FormatStringMaker constructor comment.
 */
public FormatStringMaker() {
	super();
}
/**
 * This method return string (e.g. "0.000") and it has as many zeros in mantissa
 * as parameter specified double has valid numbers in mantissa.
 * @return java.lang.String
 * @param number double
 */
public static String getFormatString(double number) {
	boolean end=false;
	int ref=(int)number;
	String s="0";
	int watchdog=0;
	while (!end&&watchdog<5) {
		if (ref<number) {
			if (s.length()==1) s+=".";
			s+="0";
			number*=10;
			ref=(int)number;
			watchdog++;
		}
		else end=true;
	}
	return s;
}
}
