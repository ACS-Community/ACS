/*
 * $Id: FilterImpl.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.services.selection;

import java.util.Arrays;

import cern.laser.client.LaserException;
import cern.laser.client.services.selection.Filter;

public class FilterImpl extends Filter {
  private String property;
  private String operator;
  private String value;

  /** Operator. */
  private static final String GREATER_THAN_OPERATOR = ">";
  /** Operator. */
  private static final String LESS_THAN_OPERATOR = "<";
  /** Operator. */
  private static final String GREATER_EQUAL_THAN_OPERATOR = ">=";
  /** Operator. */
  private static final String LESS_EQUAL_THAN_OPERATOR = "<=";
  /** Operator. */
  private static final String EQUAL_OPERATOR = "=";
  /** Operator. */
  private static final String NOT_EQUAL_OPERATOR = "<>";
  /** Operator. */
  private static final String LIKE_OPERATOR = "LIKE";
  /** The predefined privateOperators array. */
  private static final String privateOperators[] = { GREATER_THAN_OPERATOR, LESS_THAN_OPERATOR,
      GREATER_EQUAL_THAN_OPERATOR, LESS_EQUAL_THAN_OPERATOR, EQUAL_OPERATOR, NOT_EQUAL_OPERATOR, LIKE_OPERATOR};

  /** Predefined numeric property. */
  private static final String ALARM_ID_PROPERTY = "ALARM_ID";
  /** Predefined literal property. */
  private static final String FAULT_FAMILY_PROPERTY = "FAULT_FAMILY";
  /** Predefined literal property. */
  private static final String FAULT_MEMBER_PROPERTY = "FAULT_MEMBER";
  /** Predefined numeric property. */
  private static final String FAULT_CODE_PROPERTY = "FAULT_CODE";
  /** Predefined literal property. */
  private static final String PROBLEM_DESCRIPTION_PROPERTY = "PROBLEM_DESCRIPTION";
  /** Predefined literal property. */
  private static final String SITE_PROPERTY = "SITE";
  /** Predefined numeric property. */
  private static final String BUILDING_PROPERTY = "BUILDING";
  /** Predefined numeric property. */
  private static final String PRIORITY_PROPERTY = "PRIORITY";
  /** Predefined literal property. */
  private static final String SOURCE_NAME_PROPERTY = "SOURCE_NAME";
  /** Predefined numeric property. */
  private static final String SAFETY_ZONE_PROPERTY = "SAFETY_ZONE";
  /** Predefined literal property. */
  private static final String RESPONSIBLE_PERSON_PROPERTY = "RESPONSIBLE_PERSON";
  /** Predefined literal property. */
  private static final String SYSTEM_NAME_PROPERTY = "SYSTEM_NAME";
  /** Predefined literal property. */
  private static final String IDENTIFIER_PROPERTY = "IDENTIFIER";
  /** The predefined privateProperties array. */
  private static final String privateProperties[] = { ALARM_ID_PROPERTY, FAULT_FAMILY_PROPERTY, FAULT_MEMBER_PROPERTY,
      FAULT_CODE_PROPERTY, PROBLEM_DESCRIPTION_PROPERTY, SITE_PROPERTY, BUILDING_PROPERTY, PRIORITY_PROPERTY,
      SOURCE_NAME_PROPERTY, SAFETY_ZONE_PROPERTY, RESPONSIBLE_PERSON_PROPERTY, SYSTEM_NAME_PROPERTY,
      IDENTIFIER_PROPERTY};

  private static final String BOOLEAN_SUFFIX = "_BOOL";
  private static final String INTEGER_SUFFIX = "_INT";
  private static final String FLOAT_SUFFIX = "_FLOAT";
  private static final String DOUBLE_SUFFIX = "_DOUBLE";
  private static final String SHORT_SUFFIX = "_SHORT";
  private static final String LONG_SUFFIX = "_LONG";
  private static final String STRING_SUFFIX = "_STRING";
  private static final String BYTE_SUFFIX = "_BYTE";

  private static final String integerProperties[] = { ALARM_ID_PROPERTY, FAULT_CODE_PROPERTY, PRIORITY_PROPERTY,
      SAFETY_ZONE_PROPERTY};
  private static final String stringProperties[] = { FAULT_FAMILY_PROPERTY, FAULT_MEMBER_PROPERTY, BUILDING_PROPERTY,
      IDENTIFIER_PROPERTY, PROBLEM_DESCRIPTION_PROPERTY, RESPONSIBLE_PERSON_PROPERTY, SITE_PROPERTY,
      SOURCE_NAME_PROPERTY, SYSTEM_NAME_PROPERTY};
  /** The predefined numeric privateOperators array. */
  private static final String numericOperators[] = { GREATER_THAN_OPERATOR, LESS_THAN_OPERATOR,
      GREATER_EQUAL_THAN_OPERATOR, LESS_EQUAL_THAN_OPERATOR, EQUAL_OPERATOR, NOT_EQUAL_OPERATOR};
  /** The predefined literal privateOperators array. */
  private static final String literalOperators[] = { EQUAL_OPERATOR, NOT_EQUAL_OPERATOR, LIKE_OPERATOR};
  /** The predefined boolean privateOperators array. */
  private static final String booleanOperators[] = { EQUAL_OPERATOR, NOT_EQUAL_OPERATOR,};
  /** The predefined byte privateOperators array. */
  private static final String byteOperators[] = { EQUAL_OPERATOR, NOT_EQUAL_OPERATOR,};

  private boolean multiplePercentAllowed;

  public static final String[] operators() {
    return privateOperators;
  }

  public static final String[] properties() {
    return privateProperties;
  }

  public FilterImpl() {
    this(null, null, null, false);
  }

  public FilterImpl(String newProperty, String newOperator, String newValue) {
    this(newProperty, newOperator, newValue, false);
  }

  public FilterImpl(String newProperty, String newOperator, String newValue, boolean multiplePercentAllowed) {
    property = newProperty;
    operator = newOperator;
    value = newValue;
    this.multiplePercentAllowed = multiplePercentAllowed;
  }

  public String getProperty() {
    return property;
  }

  public void setProperty(String newProperty) {

    property = newProperty;
  }

  public String getOperator() {
    return operator;
  }

  public void setOperator(String newOperator) {
    operator = newOperator;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String newValue) {
    value = newValue;
  }

  public boolean equals(Object ref) {
    if (ref == null) { return false; }
    if (!(ref instanceof Filter)) { return false; }
    Filter filter = (Filter) ref;

    return filter.toString().equals(toString());
  }

  public int hashCode() {
    return toString().hashCode();
  }

  public String toString() {
    StringBuffer buffer = new StringBuffer();
    buffer.append(property);
    buffer.append(operator);
    buffer.append(value);

    return buffer.toString();
  }

  public boolean isBoolean() {
    return (property == null ? false : property.toUpperCase().endsWith(BOOLEAN_SUFFIX));
  }

  public boolean isInteger() {
    return (property == null ? false : Arrays.asList(integerProperties).contains(property)
        || property.toUpperCase().endsWith(INTEGER_SUFFIX));
  }

  public boolean isDouble() {
    return (property == null ? false : property.toUpperCase().endsWith(DOUBLE_SUFFIX));
  }

  public boolean isLong() {
    return (property == null ? false : property.toUpperCase().endsWith(LONG_SUFFIX));
  }

  public boolean isFloat() {
    return (property == null ? false : property.toUpperCase().endsWith(FLOAT_SUFFIX));
  }

  public boolean isShort() {
    return (property == null ? false : property.toUpperCase().endsWith(SHORT_SUFFIX));
  }

  public boolean isNumeric() {
    return (property == null ? false : isInteger() || isShort() || isLong() || isFloat() || isDouble());
  }

  public boolean isString() {
    return (property == null ? false : !(isNumeric() || isByte() || isBoolean()));
  }

  public boolean isByte() {
    return (property == null ? false : property.toUpperCase().endsWith(BYTE_SUFFIX));
  }

  public void validate() throws LaserException {
    if (property == null) { throw new LaserException("property is null"); }
    if (operator == null) { throw new LaserException("operator is null"); }
    if (value == null) { throw new LaserException("value is null"); }
    if (!(Arrays.asList(privateOperators).contains(operator))) { throw new LaserException("operator expected, found : "
        + operator); }
    if (isNumeric()) {
      if (!(Arrays.asList(numericOperators).contains(operator))) { throw new LaserException(
          "numeric operator expected, found : " + operator); }
      if (isInteger()) {
        try {
          Integer.valueOf(value);
        } catch (Exception e) {
          throw new LaserException("integer value expected, found : " + value);
        }
      }
      if (isLong()) {
        try {
          Long.valueOf(value);
        } catch (Exception e) {
          throw new LaserException("long value expected, found : " + value);
        }
      }
      if (isFloat()) {
        try {
          Float.valueOf(value);
        } catch (Exception e) {
          throw new LaserException("float value expected, found : " + value);
        }
      }
      if (isDouble()) {
        try {
          Double.valueOf(value);
        } catch (Exception e) {
          throw new LaserException("double value expected, found : " + value);
        }
      }
      if (isShort()) {
        try {
          Short.valueOf(value);
        } catch (Exception e) {
          throw new LaserException("short value expected, found : " + value);
        }
      }
    } else if (isBoolean()) {
      if ((!value.equalsIgnoreCase("TRUE")) && (!value.equalsIgnoreCase("FALSE"))) { throw new LaserException(
          "boolean value expected, found : " + value); }
    } else if (isByte()) {
      if (!(Arrays.asList(byteOperators).contains(operator))) { throw new LaserException(
          "byte operator expected, found : " + operator); }
      try {
        Byte.valueOf(value);
      } catch (Exception e) {
        throw new LaserException("byte value expected, found : " + value);
      }
    } else if (!(Arrays.asList(literalOperators).contains(operator))) { throw new LaserException(
        "literal operator expected, found : " + operator); }
    if (operator.equals(LIKE_OPERATOR)) {
      int index = value.indexOf("%");
      if (index == -1) { throw new LaserException("% expected"); }
      int last_index = value.lastIndexOf("%");
      if ( !multiplePercentAllowed && index != last_index) { throw new LaserException("only one occourence of % allowed"); }
    }
  }
}