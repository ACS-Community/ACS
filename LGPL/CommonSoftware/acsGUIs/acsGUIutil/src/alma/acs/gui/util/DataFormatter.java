/*
 Created on April 25, 2014 by msekoranja

 ALMA - Atacama Large Millimiter Array
 (c) European Southern Observatory, 2011

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */
package alma.acs.gui.util;

import java.lang.reflect.Modifier;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

import alma.acs.util.UTCUtility;

/**
 * Introspection based data formatter.
 */
public class DataFormatter {
	public static int MAX_RECURSION_LEVEL = 25;
	private static final SimpleDateFormat df = new SimpleDateFormat("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'SSS");
	private static final Object[] NULL_OBJECT_ARRAY = new Object[] { null };

	public static String arrayToString(Object array) {
		return arrayToString(array, "");
	}

	public static String arrayToString(Object array, String lineStart) {
		if (array.getClass().isArray()) {
			Object[] fixedArray;
			StringBuffer result = new StringBuffer();

			result.append(lineStart);
			result.append("Array of class: ");
			result.append(array.getClass().getComponentType().getName());
			if (array.getClass().getComponentType().isPrimitive())
				fixedArray = convertPrimitiveArray(array);
			else
				fixedArray = (Object[]) array;
			result.append(" length: ");
			result.append(fixedArray.length);
			result.append("\n");

			for (int i = 0; i < fixedArray.length; i++) {
				result.append(lineStart);
				result.append(" [");
				result.append(i);
				result.append("] ");
				result.append(fixedArray[i].toString());
				result.append("\n");
			}
			return result.toString();
		} else
			throw new IllegalArgumentException(
					"array parameter is not an array");
	}

	public static Object[] convertPrimitiveArray(Object value) {
		if (!(value.getClass().isArray() && value.getClass().getComponentType()
				.isPrimitive()))
			throw new IllegalArgumentException("value is not a primitive Array");
		try {
			Class<?> type = value.getClass().getComponentType();
			Object[] retVal = null;
			if (type.toString().equals("int")) {
				int[] tempVal = (int[]) value;
				retVal = new Integer[tempVal.length];
				for (int i = 0; i < tempVal.length; i++)
					retVal[i] = new Integer(tempVal[i]);
			} else if (type.toString().equals("double")) {
				double[] tempVal = (double[]) value;
				retVal = new Double[tempVal.length];
				for (int i = 0; i < tempVal.length; i++)
					retVal[i] = new Double(tempVal[i]);
			} else if (type.toString().equals("long")) {
				long[] tempVal = (long[]) value;
				retVal = new Long[tempVal.length];
				for (int i = 0; i < tempVal.length; i++)
					retVal[i] = new Long(tempVal[i]);
			} else if (type.toString().equals("short")) {
				short[] tempVal = (short[]) value;
				retVal = new Short[tempVal.length];
				for (int i = 0; i < tempVal.length; i++)
					retVal[i] = new Short(tempVal[i]);
			} else if (type.toString().equals("byte")) {
				byte[] tempVal = (byte[]) value;
				retVal = new Byte[tempVal.length];
				for (int i = 0; i < tempVal.length; i++)
					retVal[i] = new Byte(tempVal[i]);
			} else if (type.toString().equals("char")) {
				char[] tempVal = (char[]) value;
				retVal = new Character[tempVal.length];
				for (int i = 0; i < tempVal.length; i++)
					retVal[i] = new Character(tempVal[i]);
			} else if (type.toString().equals("float")) {
				float[] tempVal = (float[]) value;
				retVal = new Float[tempVal.length];
				for (int i = 0; i < tempVal.length; i++)
					retVal[i] = new Float(tempVal[i]);
			} else if (type.toString().equals("boolean")) {
				boolean[] tempVal = (boolean[]) value;
				retVal = new Boolean[tempVal.length];
				for (int i = 0; i < tempVal.length; i++)
					retVal[i] = new Boolean(tempVal[i]);
			}
			return retVal;
		} catch (Exception e) {
			throw new IllegalArgumentException("value could not be converted");
		}
	}

	public static String cropFromEnd(String string, int number_of_lines) {
		int start = getLineStart(string, getLineCount(string) - number_of_lines);
		if (start < 0)
			return string;
		
		String retVal = string.substring(
				start,
				string.length() - 1);
		return retVal;
	}

	public static int getLineCount(String lines) {
		int count = 0;
		int pos = -1;
		int maxPos = lines.length();

		do {
			pos = lines.indexOf(10, pos + 1);
			count++;
		} while ((pos > -1) && (pos < maxPos));

		return count;
	}

	public static int getLineStart(String string, int lineNo) {
		int count = 0;
		int pos = -1;
		int maxPos = string.length();

		do {
			pos = string.indexOf(10, pos + 1);
			count++;
		} while ((pos > -1) && (pos < maxPos) && (count < lineNo));
		return pos;
	}

	public static String[] splitString(String stringToSplit,
			String charToSplitAt) {
		int end = -1;
		int start = 0;
		int maxPos = stringToSplit.length();
		ArrayList<String> list = new ArrayList<String>();
		do {
			end = stringToSplit.indexOf(charToSplitAt, start);
			list.add(stringToSplit.substring(start, end));
			start = end + 1;
		} while ((end > -1) && (end < maxPos));
		String[] retVal = new String[list.size()];
		list.toArray(retVal);
		return retVal;
	}

	public static String[] splitStringByLines(String stringToSplit) {
		int end = -1;
		int start = 0;
		int maxPos = stringToSplit.length();
		ArrayList<String> list = new ArrayList<String>();
		do {
			end = stringToSplit.indexOf(10, start);
			if (end == -1)
				end = maxPos - 1;
			if ((end >= 0) && (end < maxPos) && (start >= 0)
					&& (start < maxPos))
				list.add(stringToSplit.substring(start, end));
			start = end + 1;
		} while ((end > -1) && (end < maxPos - 1));
		String[] retVal = new String[list.size()];
		list.toArray(retVal);
		return retVal;
	}

	public static String unpackArray(Object array) {
		return unpackArray(array, "", 0, false);
	}

	public static String unpackArray(Object array, String lineStart) {
		return unpackArray(array, lineStart, 0, false);
	}

	public static String unpackArray(Object array, String lineStart, int level,
			boolean expand) {
		StringBuffer result = new StringBuffer();
		Class<?> type = array.getClass().getComponentType();
		result.append("[Array of " + type + "], length = "
				+ java.lang.reflect.Array.getLength(array));
		Object[] list = null;
		if (type.isPrimitive()) {
			list = convertPrimitiveArray(array);
		} else
			list = (Object[]) array;
		for (int i = 0; i < list.length; i++)
			result.append("\n"
					+ lineStart
					+ " ("
					+ i
					+ ") "
					+ unpackReturnValue(list[i], lineStart + "  ", level + 1,
							expand));
		return result.toString();
	}

	/**
	 * Uses introspection to unpack any type of return value operations might
	 * return. It does that recursively to the level of max_recursion_level. 
	 * It omits methods, such as hashCode, clone etc.
	 */
	public static String unpackReturnValue(Object value, String start, int level) {
		return unpackReturnValue(value, start, level, false);
	}

	/**
	 * Uses introspection to unpack any type of return value operations might
	 * return. It does that recursively to the level of max_recursion_level. 
	 * It omits methods, such as hashCode, clone etc.
	 */
	public static String unpackReturnValue(Object value, String start,
			int level, boolean expand) {
		StringBuffer result = new StringBuffer(500);
		try {
			if (value != null) {
				Class<?> type = value.getClass();
				if (type.isArray()) {
					result.append(unpackArray(value, start, level, expand));
				} else {
					if ((value instanceof java.lang.String)
							|| (value instanceof java.lang.Number)
							|| (value instanceof java.lang.Boolean)) {
						result.append(value.toString());
					} else {
						if (level > MAX_RECURSION_LEVEL)
							return (value.getClass() + " (Recursion level exceeded)");
						else {
							result.append(value.toString());
							if (expand) {
								result.append('\n');
								result.append(start);
								result.append("  (" + type.getName() + ")");
								java.lang.reflect.Field[] fields = type
										.getFields();
								for (int j = 0; j < fields.length; j++) {
									if (!Modifier.isStatic(fields[j]
											.getModifiers())
											&& Modifier.isPublic(fields[j]
													.getModifiers())) {
										java.lang.reflect.Field curField = fields[j];
										result.append('\n');
										result.append(start);
										result.append(curField.getName());
										result.append(": ");
										// msekoran - ACS timestamp support (this is really not clean solution)
										if (curField.getName().equals(
												"timeStamp")
												&& curField
														.getDeclaringClass()
														.getName()
														.equals("alma.ACSErr.Completion")) {
											long javaTime = UTCUtility
													.utcOmgToJava(curField
															.getLong(value));
											result.append(df.format(new Date(
													javaTime)));
										} else
											result.append(unpackReturnValue(
													curField.get(value), start
															+ " ", level + 1,
													expand));
									}
								}

								// do not show Java Throwable method (e.g. getStackTrace)
								boolean isThrowable = value instanceof Throwable;
								if (!isThrowable) {
									java.lang.reflect.Method[] methods = type
											.getMethods();
									for (int j = 0; j < methods.length; j++) {
										if (java.lang.reflect.Modifier
												.isPublic(methods[j]
														.getModifiers())) {
											java.lang.reflect.Method curMet = methods[j];
											if ((curMet.getParameterTypes().length == 0)
													&& ((curMet.getReturnType()
															.isPrimitive()) || (level < 1))
													&& (!curMet.getReturnType()
															.toString()
															.equals("void"))
													&& (!curMet.getName()
															.startsWith("_"))
													&& (!curMet.getName()
															.equals("hashCode"))
													&& (!curMet.getName()
															.equals("clone"))
													&& (!curMet.getName()
															.equals("getClass"))
													&& (!curMet.getName()
															.equals("toString")))
												result.append("\n"
														+ start
														+ "  |"
														+ curMet.getName()
														+ ": "
														+ unpackReturnValue(
																curMet.invoke(
																		value,
																		NULL_OBJECT_ARRAY),
																start + "  |",
																level + 1,
																expand));
										}
									}
								}
							}
						}
					}
				}
				return result.toString();
			} else
				return ("null");
		} catch (Exception e) {
			return (result + "\n" + start + "N/A");
		}
	}

	public static String unpackValues(String[] names, Object[] values) {
		return unpackValues(names, values, ":");
	}

	public static String unpackValues(String[] names, Object[] values,
			String delimiter) {
		return unpackValues(names, values, delimiter, false);
	}

	public static String unpackValues(String[] names, Object[] values,
			String delimiter, boolean expand) {
		StringBuffer result = new StringBuffer();
		for (int i = 0; i < values.length; i++) {
			result.append("    ");
			result.append(names[i]);
			result.append(delimiter);
			result.append(" ");
			result.append(unpackReturnValue(values[i], "      ", 0, expand));
			result.append("\n");
		}
		return result.toString();
	}
}
