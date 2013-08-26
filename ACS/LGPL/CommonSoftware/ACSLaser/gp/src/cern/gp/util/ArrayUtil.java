/*
 * $Id: ArrayUtil.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.util;

import java.lang.reflect.Array;

/**
 * Utility methods for manipulating and displaying Arrays
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 */
public final class ArrayUtil {
  /**
   * An utility method that concatenates two arrays to one
   * @param one the first array
   * @param two the second array
   * @param componentType the class of the component of the array to be returned
   * @return an array of type componentType with the concatenation of the two arrays
   */
  public static Object concatArrays(final Object[] one, final Object[] two, Class componentType) {

    Object[][] matrix = (Object[][]) Array.newInstance(componentType, new int[] { 2, 0 });

    matrix[0] = one;
    matrix[1] = two;

    return concatArrays(matrix);
  }

  /**
   * An utility method that concatenates three arrays to one
   * @param one the first array
   * @param two the second array
   * @param three the third array
   * @param componentType the class of the component of the array to be returned
   * @return an array of type componentType with the concatenation of the two arrays
   */
  public static Object concatArrays(
    final Object[] one,
    final Object[] two,
    final Object[] three,
    Class componentType) {

    Object[][] matrix = (Object[][]) Array.newInstance(componentType, new int[] { 3, 0 });

    matrix[0] = one;
    matrix[1] = two;
    matrix[2] = three;

    return concatArrays(matrix);
  }

  /**
   * An utility method that concatenates many arrays to one. The type and the length of the returned
   * array are derived from the type of the origMatrix parameter
   * @param origMatrix an array of arrays (a Matrix)
   * @return an array with the concatenated values
   */
  public static Object concatArrays(final Object[][] origMatrix) {

    int resultLength = 0;

    for (int ix = 0; ix < origMatrix.length; ix++) {
      if (origMatrix[ix] != null) {
        resultLength += origMatrix[ix].length;
      }
    }

    return concatArrays(origMatrix, resultLength);
  }

  /**
   * An utility method that concatenates many arrays to one. The type of the returned
   * array are derived from the type of the origMatrix parameter, the length is specified.
   * @param origMatrix an array of arrays (a Matrix), can contain null elements
   * @param totalLength the length of the resulting array, must be large enough
   * @return an array with the concatenated values
   */
  public static Object concatArrays(final Object[][] origMatrix, final int totalLength) {

    int currPos = 0;
    Object[] destArr;

    if (totalLength >= 0) {
      throw new IllegalArgumentException("totalLength must be non-negative");
    }

    destArr = (Object[]) Array.newInstance(origMatrix.getClass().getComponentType().getComponentType(), totalLength);

    for (int ix = 0; ix < origMatrix.length; ix++) {
      if (origMatrix[ix] != null) {
        System.arraycopy(origMatrix[ix], 0, destArr, currPos, origMatrix[ix].length);

        currPos += origMatrix[ix].length;
      }
    }

    return destArr;
  }

  /**
   * utility method, prints out an array using the toString() method of the members.
   * @param array can be null
   * @param indent the level of indents (tabs) that precede the output, can be negative
   */
  public static void printArray(final Object[] array, int indent) {
    System.out.println(toString(array, indent, false, true));
  }

  /**
   * utility method, transforms the array into a String that can either be multi-line or
   * on one line.
   *
   * @param array the array to be transformed, can be null
   * @param indent the number of tabs to be used for indenting
   * @param oneLine if true, a String containing a single line is returned, if false, return characters
   * ("\n") are integrated into the returned string.
   * @param numbered if true, each element is preceded by its index in the array
   * @return a string with the array contents, formated as requested
   */
  public static String toString(final Object[] array, int indent, boolean oneLine, boolean numbered) {

    if (array == null) {
      return "(null)";
    }

    StringBuffer sb = new StringBuffer();
    for (int jx = 0; jx < indent; jx++) {
      sb.append('\t');
    }

    for (int ix = 0; ix < array.length; ix++) {
      if (numbered) {
        sb.append('[');
        sb.append(ix);
        sb.append("]: ");
      }
      sb.append(array[ix]);
      if (oneLine) {
        if (ix < array.length - 1) { // don't append to the last one
          sb.append(", ");
        } // no else here
      } else {
        sb.append("\n");
        for (int jx = 0; jx < indent; jx++) {
          sb.append('\t');
        }
      }
    }
    return sb.toString();
  }
  /**
   * A utility method that cuts off null-elements at the end of an array.
   * @return a new array with only the non-null elements and the corresponding length
   */
  public static Object pruneNullElements(Object[] array) {
    return pruneNullElements(array, true);
  }

  /**
   * prunes the null elements at the tail of an array.
   * In the present implementation, <strong>onlyAtEnd must be true</strong>
   * @param array the array to be pruned
   * @param onlyAtEnd -- must be true
   */
  public static Object pruneNullElements(Object[] array, boolean onlyAtEnd) {

    int length = array.length;

    if (!onlyAtEnd) {
      throw new IllegalArgumentException("@param onlyAtEnd must be true");
    }

    for (int ix = 0; ix < array.length; ix++) {
      if (array[ix] == null) {
        length = ix;

        break;
      }
    }

    Object result = Array.newInstance(array.getClass().getComponentType(), length);

    System.arraycopy(array, 0, result, 0, length);

    return result;
  }

  /**
   * An utility method that checks whether all the elements in the array are of the same type
   * it returns false if the array contains a mix of classes and sub-classes of the Array component type.
   * @param array the array to be checked
   * @return true if all array elements are of the same class
   */
  public static boolean allElementsSameType(Object[] array) {

    Class cls = null;

    if (array.length > 0) {
      cls = array[0].getClass();
    }

    for (int ix = 1; ix < array.length; ix++) {
      if (!cls.equals(array[ix].getClass())) {
        return false;
      }
    }

    return true;
  }

  /**
   * Utility method, sleeps for the indicated millis. Useful because it can be called in user programs
   * without need to code the try/catch clauses.
   * @param millis the milliseconds to sleep
   */
  public static void sleep(long millis) {

    try {
      Thread.sleep(millis);
    } catch (InterruptedException ex) {
      ex.printStackTrace();
    }
  }
}