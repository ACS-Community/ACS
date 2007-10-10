package dartboard;

/**
 * Insert the type's description here.
 * Creation date: (11/7/00 9:02:16 PM)
 * @author: 
 */
public abstract class Planet extends Dart {
	private double rightAscension;
	private double declination;
/**
 * Planet constructor comment.
 * @param x int
 * @param y int
 */
public Planet() {
	super();
}
/**
 * Insert the method's description here.
 * Creation date: (11/9/00 5:35:00 PM)
 * @return double
 */
public double getDeclination() {
	return declination;
}
/**
 * Insert the method's description here.
 * Creation date: (11/9/00 5:34:35 PM)
 * @return double
 */
public double getRightAscension() {
	return rightAscension;
}
/**
 * Insert the method's description here.
 * Creation date: (11/9/00 5:35:00 PM)
 * @param newDeclination double
 */
public void setDeclination(double newDeclination) {
	declination = newDeclination;
}
/**
 * Insert the method's description here.
 * Creation date: (11/9/00 5:34:35 PM)
 * @param newRightAscension double
 */
public void setRightAscension(double newRightAscension) {
	rightAscension = newRightAscension;
}
}
