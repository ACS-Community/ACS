package com.cosylab.gui.components.r2.chart.accessories;

import java.lang.Math;
/**
 * This class is for making right numbers for intervals 
 * at graphs between two number signatures. 
 */
public class IntervalNumber {
/**
 * IntervalNumber constructor comment.
 */
public IntervalNumber() {
	super();
}
/**
 * This method return suitable interval that is greather than parameer specified.
 */
public static double getRightInterval(double interv) {
	int exp;
	if (Math.log(interv)/Math.log(10)>0) {
		exp=(int)(Math.log(interv)/Math.log(10));
	} else exp=-(int)Math.ceil(-Math.log(interv)/Math.log(10));

	double osn=interv/Math.pow(10,exp);
	
	if (osn<=2) {
		return (2*Math.pow(10,exp));
	} else {
		if (osn<=3) {
			return (3*Math.pow(10,exp));
		} else {
			if (osn<=5) {
				return (5*Math.pow(10,exp));
			} else {
				return (Math.pow(10,exp+1));
			}
		}
	}
}
/**
 * This method return interval that is used for determing central number, around which all
 * numbers on axis are disposed
 * @see ImprovedChartXAxis.drawAxis()
 */
public static double getRoundedInterval(double interv) {
	int exp;
	if (Math.log(interv)/Math.log(10)>0) {
		exp=(int)(Math.log(interv)/Math.log(10));
	} else exp=-(int)Math.ceil(-Math.log(interv)/Math.log(10));

	return (Math.pow(10,exp+1));
}
/**
 * This method takes interval as a parameter and return suitable subinterval.
 */
public static double getSubInterval(double interv) {
	int exp;
	if (Math.log(interv)/Math.log(10)>0) {
		exp=(int)(Math.log(interv)/Math.log(10));
	} else exp=-(int)Math.ceil(-Math.log(interv)/Math.log(10));

	double osn=interv/Math.pow(10,exp);
	
	if (osn<=2) {
		return (Math.pow(10,exp));
	} else {
		if (osn<=3) {
			return (3*Math.pow(10,exp));
		} else {
			if (osn<=5) {
				return (5*Math.pow(10,exp));
			} else {
				return (5*Math.pow(10,exp));
			}
		}
	}
}
/**
 * This method takes interval as a parameter and return suitable subsubinterval.
 */
public static double getSubSubInterval(double interv) {
	int exp;
	if (Math.log(interv)/Math.log(10)>0) {
		exp=(int)(Math.log(interv)/Math.log(10));
	} else exp=-(int)Math.ceil(-Math.log(interv)/Math.log(10));

	double osn=interv/Math.pow(10,exp);
	
	if (osn<=2) {
		return (2*Math.pow(10,exp-1));
	} else {
		if (osn<=3) {
			return (Math.pow(10,exp));
		} else {
			if (osn<=5) {
				return (Math.pow(10,exp));
			} else {
				return (Math.pow(10,exp));
			}
		}
	}
}
}
