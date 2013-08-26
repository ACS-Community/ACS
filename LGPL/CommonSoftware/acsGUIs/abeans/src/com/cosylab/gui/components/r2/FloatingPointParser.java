package com.cosylab.gui.components.r2;

/**
 * Simple, robust floating point number parser.<p>
 * The parser is derived from the following regular expression:<p>
 * <code>[+-]?[0-9]*(\.[0-9]*)?((e[+-]?)[0-9]*)?</code><p>
 * This expression allows maximum flexibility in description of floating
 * point numbers, while still generating expected output. Every state of
 * this parser is final and therefore suitable for parsing of sequential user
 * input, where intermediate states are not valid values.<p>
 * Creation date: (12/16/2001 16:46:23)
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public final class FloatingPointParser {

    // FP number segments  a.bEc
    private String a = "";
    private String b = "";
    private String c = "";

    private static final int ERROR_STATE = 99;

    // Parser state transitions: transitions[Current State][Symbol]
    private static final int[][] transitions = { 
	    { 99,  1,  1,  2,  3 }, 
	    { 99, 99,  1,  2,  3 }, 
	    { 99, 99,  2, 99,  3 }, 
	    { 99,  4,  4, 99, 99 }, 
	    { 99, 99,  4, 99, 99 }
    };

    private static final java.text.DecimalFormatSymbols symbols = new java.text.DecimalFormatSymbols();

/**
 * Creates new parser.
 */
public FloatingPointParser() {
	super();
}
/**
 * Performs action associated with each state.
 * Creation date: (12/16/2001 17:04:50)
 * @param state int
 * @param ch Char
 */
private final void doAction(int state, int symbol, char ch) {
    switch (state) {
        case 1 :
            if (symbol == 1)
                a = ch + "0";
            else
                a += ch;
            break;
        case 2 :
            if (symbol != 3)
                b += ch;
            break;
        case 4 :
            if (symbol == 1)
                c = ch + "0";
            else
                c += ch;
    }
}
/**
 * Returns symbol based on character.
 * Creation date: (12/16/2001 16:49:11)
 * @return int
 * @param ch char
 */
private final int getNextSymbol(char ch) {

    if (Character.isDigit(ch))
        return 2;

    if ((ch == '+') || (ch == '-'))
        return 1;

    if (ch == symbols.getDecimalSeparator())
        return 3;

    if ((ch == 'e') || (ch == 'E'))
        return 4;

    return 0;
}
/**
 * Performs the actual parsing.
 * Creation date: (12/16/2001 16:48:37)
 * @return boolean
 * @param s java.lang.String
 */
private final boolean parse(String s) {

    char[] chars = s.toCharArray();
    int n = chars.length;
    a = "";
    b = "";
    c = "";

    int currState = 0;
    int nextState = ERROR_STATE;
    int nextSymbol = 0;
    char nextChar = ' ';

    int c = 0;

    while ((c < n) && (currState != ERROR_STATE)) {
        nextChar = chars[c++];
        nextSymbol = getNextSymbol(nextChar);
        nextState = transitions[currState][nextSymbol];
        doAction(nextState, nextSymbol, nextChar);
        currState = nextState;
    }
    return (currState != ERROR_STATE);
}
/**
 * Parses the supplied string and tries to convert it to double.
 * Creation date: (12/16/2001 16:47:57)
 * @throws RuntimeException
 * @return double
 * @param s java.lang.String
 */
public synchronized final double parseDouble(String s) throws RuntimeException {
    if (parse(s)) {
        if (a == "")
            a = "0";
        if (b == "")
            b = "0";
        if (c == "")
            c = "0";

		String d = a + "." + b + "e" + c;

        return Double.parseDouble(d);
    }
    throw new RuntimeException(s + " is not acceptible floating point value");
}
}
