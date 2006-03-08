/*
	Simple class for testing antlr-generated HTML parser/lexer.
	Alexander Hinds, Magelang Institute
	ahinds@magelang.com

*/

#include <iostream>
#include "HTMLLexer.hpp"
#include "HTMLParser.hpp"
#include "antlr/TokenBuffer.hpp"

int main( int, char** )
{
	ANTLR_USING_NAMESPACE(std)
	ANTLR_USING_NAMESPACE(antlr)
	try {
		HTMLLexer lexer(cin);
		TokenBuffer buffer(lexer);
		HTMLParser parser(buffer);
		parser.document();
	}
	catch( ANTLRException& e )
	{
	  cout << "exception: " << e.getMessage() << endl;
	  return -1;
	}
	catch( exception& e )
	{
	  cout << "exception: " << e.what() << endl;
	  return -1;
	}

	cout << "The HTML read on cin has been  parsed." << endl;
	return 0;
}

