/* $ANTLR 2.7.6 (20060308): "html.g" -> "HTMLParser.cpp"$ */
#include "HTMLParser.hpp"
#include <antlr/NoViableAltException.hpp>
#include <antlr/SemanticException.hpp>
#include <antlr/ASTFactory.hpp>
#line 1 "html.g"
#line 8 "HTMLParser.cpp"
HTMLParser::HTMLParser(ANTLR_USE_NAMESPACE(antlr)TokenBuffer& tokenBuf, int k)
: ANTLR_USE_NAMESPACE(antlr)LLkParser(tokenBuf,k)
{
}

HTMLParser::HTMLParser(ANTLR_USE_NAMESPACE(antlr)TokenBuffer& tokenBuf)
: ANTLR_USE_NAMESPACE(antlr)LLkParser(tokenBuf,1)
{
}

HTMLParser::HTMLParser(ANTLR_USE_NAMESPACE(antlr)TokenStream& lexer, int k)
: ANTLR_USE_NAMESPACE(antlr)LLkParser(lexer,k)
{
}

HTMLParser::HTMLParser(ANTLR_USE_NAMESPACE(antlr)TokenStream& lexer)
: ANTLR_USE_NAMESPACE(antlr)LLkParser(lexer,1)
{
}

HTMLParser::HTMLParser(const ANTLR_USE_NAMESPACE(antlr)ParserSharedInputState& state)
: ANTLR_USE_NAMESPACE(antlr)LLkParser(state,1)
{
}

void HTMLParser::document() {
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case PCDATA:
		{
			match(PCDATA);
			break;
		}
		case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
		case DOCTYPE:
		case OHTML:
		case CHTML:
		case OHEAD:
		case ISINDEX:
		case BASE:
		case META:
		case LINK:
		case OTITLE:
		case OSCRIPT:
		case OSTYLE:
		case OBODY:
		case ADDRESS:
		case HR:
		case IMG:
		case BFONT:
		case BR:
		case OH1:
		case OH2:
		case OH3:
		case OH4:
		case OH5:
		case OH6:
		case OPARA:
		case OULIST:
		case OOLIST:
		case ODLIST:
		case OPRE:
		case ODIV:
		case OCENTER:
		case OBQUOTE:
		case OFORM:
		case OTABLE:
		case OTTYPE:
		case OITALIC:
		case OBOLD:
		case OUNDER:
		case OSTRIKE:
		case OBIG:
		case OSMALL:
		case OSUB:
		case OSUP:
		case OEM:
		case OSTRONG:
		case ODFN:
		case OCODE:
		case OSAMP:
		case OKBD:
		case OVAR:
		case OCITE:
		case OANCHOR:
		case OAPPLET:
		case OFONT:
		case OMAP:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		{
		switch ( LA(1)) {
		case DOCTYPE:
		{
			match(DOCTYPE);
			{
			switch ( LA(1)) {
			case PCDATA:
			{
				match(PCDATA);
				break;
			}
			case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
			case OHTML:
			case CHTML:
			case OHEAD:
			case ISINDEX:
			case BASE:
			case META:
			case LINK:
			case OTITLE:
			case OSCRIPT:
			case OSTYLE:
			case OBODY:
			case ADDRESS:
			case HR:
			case IMG:
			case BFONT:
			case BR:
			case OH1:
			case OH2:
			case OH3:
			case OH4:
			case OH5:
			case OH6:
			case OPARA:
			case OULIST:
			case OOLIST:
			case ODLIST:
			case OPRE:
			case ODIV:
			case OCENTER:
			case OBQUOTE:
			case OFORM:
			case OTABLE:
			case OTTYPE:
			case OITALIC:
			case OBOLD:
			case OUNDER:
			case OSTRIKE:
			case OBIG:
			case OSMALL:
			case OSUB:
			case OSUP:
			case OEM:
			case OSTRONG:
			case ODFN:
			case OCODE:
			case OSAMP:
			case OKBD:
			case OVAR:
			case OCITE:
			case OANCHOR:
			case OAPPLET:
			case OFONT:
			case OMAP:
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			break;
		}
		case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
		case OHTML:
		case CHTML:
		case OHEAD:
		case ISINDEX:
		case BASE:
		case META:
		case LINK:
		case OTITLE:
		case OSCRIPT:
		case OSTYLE:
		case OBODY:
		case ADDRESS:
		case HR:
		case IMG:
		case BFONT:
		case BR:
		case OH1:
		case OH2:
		case OH3:
		case OH4:
		case OH5:
		case OH6:
		case OPARA:
		case OULIST:
		case OOLIST:
		case ODLIST:
		case OPRE:
		case ODIV:
		case OCENTER:
		case OBQUOTE:
		case OFORM:
		case OTABLE:
		case OTTYPE:
		case OITALIC:
		case OBOLD:
		case OUNDER:
		case OSTRIKE:
		case OBIG:
		case OSMALL:
		case OSUB:
		case OSUP:
		case OEM:
		case OSTRONG:
		case ODFN:
		case OCODE:
		case OSAMP:
		case OKBD:
		case OVAR:
		case OCITE:
		case OANCHOR:
		case OAPPLET:
		case OFONT:
		case OMAP:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		{
		switch ( LA(1)) {
		case OHTML:
		{
			match(OHTML);
			{
			switch ( LA(1)) {
			case PCDATA:
			{
				match(PCDATA);
				break;
			}
			case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
			case CHTML:
			case OHEAD:
			case ISINDEX:
			case BASE:
			case META:
			case LINK:
			case OTITLE:
			case OSCRIPT:
			case OSTYLE:
			case OBODY:
			case ADDRESS:
			case HR:
			case IMG:
			case BFONT:
			case BR:
			case OH1:
			case OH2:
			case OH3:
			case OH4:
			case OH5:
			case OH6:
			case OPARA:
			case OULIST:
			case OOLIST:
			case ODLIST:
			case OPRE:
			case ODIV:
			case OCENTER:
			case OBQUOTE:
			case OFORM:
			case OTABLE:
			case OTTYPE:
			case OITALIC:
			case OBOLD:
			case OUNDER:
			case OSTRIKE:
			case OBIG:
			case OSMALL:
			case OSUB:
			case OSUP:
			case OEM:
			case OSTRONG:
			case ODFN:
			case OCODE:
			case OSAMP:
			case OKBD:
			case OVAR:
			case OCITE:
			case OANCHOR:
			case OAPPLET:
			case OFONT:
			case OMAP:
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			break;
		}
		case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
		case CHTML:
		case OHEAD:
		case ISINDEX:
		case BASE:
		case META:
		case LINK:
		case OTITLE:
		case OSCRIPT:
		case OSTYLE:
		case OBODY:
		case ADDRESS:
		case HR:
		case IMG:
		case BFONT:
		case BR:
		case OH1:
		case OH2:
		case OH3:
		case OH4:
		case OH5:
		case OH6:
		case OPARA:
		case OULIST:
		case OOLIST:
		case ODLIST:
		case OPRE:
		case ODIV:
		case OCENTER:
		case OBQUOTE:
		case OFORM:
		case OTABLE:
		case OTTYPE:
		case OITALIC:
		case OBOLD:
		case OUNDER:
		case OSTRIKE:
		case OBIG:
		case OSMALL:
		case OSUB:
		case OSUP:
		case OEM:
		case OSTRONG:
		case ODFN:
		case OCODE:
		case OSAMP:
		case OKBD:
		case OVAR:
		case OCITE:
		case OANCHOR:
		case OAPPLET:
		case OFONT:
		case OMAP:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		{
		switch ( LA(1)) {
		case OHEAD:
		case ISINDEX:
		case BASE:
		case META:
		case LINK:
		case OTITLE:
		case OSCRIPT:
		case OSTYLE:
		{
			head();
			break;
		}
		case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
		case CHTML:
		case OBODY:
		case ADDRESS:
		case HR:
		case IMG:
		case BFONT:
		case BR:
		case OH1:
		case OH2:
		case OH3:
		case OH4:
		case OH5:
		case OH6:
		case OPARA:
		case OULIST:
		case OOLIST:
		case ODLIST:
		case OPRE:
		case ODIV:
		case OCENTER:
		case OBQUOTE:
		case OFORM:
		case OTABLE:
		case OTTYPE:
		case OITALIC:
		case OBOLD:
		case OUNDER:
		case OSTRIKE:
		case OBIG:
		case OSMALL:
		case OSUB:
		case OSUP:
		case OEM:
		case OSTRONG:
		case ODFN:
		case OCODE:
		case OSAMP:
		case OKBD:
		case OVAR:
		case OCITE:
		case OANCHOR:
		case OAPPLET:
		case OFONT:
		case OMAP:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		{
		switch ( LA(1)) {
		case OBODY:
		case ADDRESS:
		case HR:
		case IMG:
		case BFONT:
		case BR:
		case OH1:
		case OH2:
		case OH3:
		case OH4:
		case OH5:
		case OH6:
		case OPARA:
		case OULIST:
		case OOLIST:
		case ODLIST:
		case OPRE:
		case ODIV:
		case OCENTER:
		case OBQUOTE:
		case OFORM:
		case OTABLE:
		case OTTYPE:
		case OITALIC:
		case OBOLD:
		case OUNDER:
		case OSTRIKE:
		case OBIG:
		case OSMALL:
		case OSUB:
		case OSUP:
		case OEM:
		case OSTRONG:
		case ODFN:
		case OCODE:
		case OSAMP:
		case OKBD:
		case OVAR:
		case OCITE:
		case OANCHOR:
		case OAPPLET:
		case OFONT:
		case OMAP:
		{
			body();
			break;
		}
		case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
		case CHTML:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		{
		switch ( LA(1)) {
		case CHTML:
		{
			match(CHTML);
			{
			switch ( LA(1)) {
			case PCDATA:
			{
				match(PCDATA);
				break;
			}
			case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			break;
		}
		case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_0);
	}
}

void HTMLParser::head() {
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case OHEAD:
		{
			match(OHEAD);
			{
			switch ( LA(1)) {
			case PCDATA:
			{
				match(PCDATA);
				break;
			}
			case ISINDEX:
			case BASE:
			case META:
			case LINK:
			case OTITLE:
			case OSCRIPT:
			case OSTYLE:
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			break;
		}
		case ISINDEX:
		case BASE:
		case META:
		case LINK:
		case OTITLE:
		case OSCRIPT:
		case OSTYLE:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		head_element();
		{ // ( ... )*
		for (;;) {
			switch ( LA(1)) {
			case PCDATA:
			{
				match(PCDATA);
				break;
			}
			case ISINDEX:
			case BASE:
			case META:
			case LINK:
			case OTITLE:
			case OSCRIPT:
			case OSTYLE:
			{
				head_element();
				break;
			}
			default:
			{
				goto _loop15;
			}
			}
		}
		_loop15:;
		} // ( ... )*
		{
		switch ( LA(1)) {
		case CHEAD:
		{
			match(CHEAD);
			{
			switch ( LA(1)) {
			case PCDATA:
			{
				match(PCDATA);
				break;
			}
			case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
			case CHTML:
			case OBODY:
			case ADDRESS:
			case HR:
			case IMG:
			case BFONT:
			case BR:
			case OH1:
			case OH2:
			case OH3:
			case OH4:
			case OH5:
			case OH6:
			case OPARA:
			case OULIST:
			case OOLIST:
			case ODLIST:
			case OPRE:
			case ODIV:
			case OCENTER:
			case OBQUOTE:
			case OFORM:
			case OTABLE:
			case OTTYPE:
			case OITALIC:
			case OBOLD:
			case OUNDER:
			case OSTRIKE:
			case OBIG:
			case OSMALL:
			case OSUB:
			case OSUP:
			case OEM:
			case OSTRONG:
			case ODFN:
			case OCODE:
			case OSAMP:
			case OKBD:
			case OVAR:
			case OCITE:
			case OANCHOR:
			case OAPPLET:
			case OFONT:
			case OMAP:
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
			}
			}
			}
			break;
		}
		case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
		case CHTML:
		case OBODY:
		case ADDRESS:
		case HR:
		case IMG:
		case BFONT:
		case BR:
		case OH1:
		case OH2:
		case OH3:
		case OH4:
		case OH5:
		case OH6:
		case OPARA:
		case OULIST:
		case OOLIST:
		case ODLIST:
		case OPRE:
		case ODIV:
		case OCENTER:
		case OBQUOTE:
		case OFORM:
		case OTABLE:
		case OTTYPE:
		case OITALIC:
		case OBOLD:
		case OUNDER:
		case OSTRIKE:
		case OBIG:
		case OSMALL:
		case OSUB:
		case OSUP:
		case OEM:
		case OSTRONG:
		case ODFN:
		case OCODE:
		case OSAMP:
		case OKBD:
		case OVAR:
		case OCITE:
		case OANCHOR:
		case OAPPLET:
		case OFONT:
		case OMAP:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_1);
	}
}

void HTMLParser::body() {
	
	try {      // for error handling
		{
		switch ( LA(1)) {
		case OBODY:
		{
			match(OBODY);
			{ // ( ... )*
			for (;;) {
				if ((LA(1) == PCDATA)) {
					match(PCDATA);
				}
				else {
					goto _loop30;
				}
				
			}
			_loop30:;
			} // ( ... )*
			break;
		}
		case ADDRESS:
		case HR:
		case IMG:
		case BFONT:
		case BR:
		case OH1:
		case OH2:
		case OH3:
		case OH4:
		case OH5:
		case OH6:
		case OPARA:
		case OULIST:
		case OOLIST:
		case ODLIST:
		case OPRE:
		case ODIV:
		case OCENTER:
		case OBQUOTE:
		case OFORM:
		case OTABLE:
		case OTTYPE:
		case OITALIC:
		case OBOLD:
		case OUNDER:
		case OSTRIKE:
		case OBIG:
		case OSMALL:
		case OSUB:
		case OSUP:
		case OEM:
		case OSTRONG:
		case ODFN:
		case OCODE:
		case OSAMP:
		case OKBD:
		case OVAR:
		case OCITE:
		case OANCHOR:
		case OAPPLET:
		case OFONT:
		case OMAP:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		body_content_no_PCDATA();
		{ // ( ... )+
		int _cnt32=0;
		for (;;) {
			if ((_tokenSet_2.member(LA(1)))) {
				body_content();
			}
			else {
				if ( _cnt32>=1 ) { goto _loop32; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt32++;
		}
		_loop32:;
		}  // ( ... )+
		{
		switch ( LA(1)) {
		case CBODY:
		{
			match(CBODY);
			{ // ( ... )*
			for (;;) {
				if ((LA(1) == PCDATA)) {
					match(PCDATA);
				}
				else {
					goto _loop35;
				}
				
			}
			_loop35:;
			} // ( ... )*
			break;
		}
		case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
		case CHTML:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_3);
	}
}

void HTMLParser::head_element() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case OTITLE:
		{
			title();
			break;
		}
		case OSCRIPT:
		{
			script();
			break;
		}
		case OSTYLE:
		{
			style();
			break;
		}
		case ISINDEX:
		{
			match(ISINDEX);
			break;
		}
		case BASE:
		{
			match(BASE);
			break;
		}
		case META:
		{
			match(META);
			break;
		}
		case LINK:
		{
			match(LINK);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_4);
	}
}

void HTMLParser::title() {
	
	try {      // for error handling
		match(OTITLE);
		{
		switch ( LA(1)) {
		case PCDATA:
		{
			match(PCDATA);
			break;
		}
		case CTITLE:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		match(CTITLE);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_4);
	}
}

void HTMLParser::script() {
	
	try {      // for error handling
		match(OSCRIPT);
		{ // ( ... )+
		int _cnt23=0;
		for (;;) {
			if ((_tokenSet_5.member(LA(1)))) {
				matchNot(CSCRIPT);
			}
			else {
				if ( _cnt23>=1 ) { goto _loop23; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt23++;
		}
		_loop23:;
		}  // ( ... )+
		match(CSCRIPT);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_4);
	}
}

void HTMLParser::style() {
	
	try {      // for error handling
		match(OSTYLE);
		{ // ( ... )+
		int _cnt26=0;
		for (;;) {
			if ((_tokenSet_6.member(LA(1)))) {
				matchNot(CSTYLE);
			}
			else {
				if ( _cnt26>=1 ) { goto _loop26; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt26++;
		}
		_loop26:;
		}  // ( ... )+
		match(CSTYLE);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_4);
	}
}

void HTMLParser::body_content_no_PCDATA() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case ADDRESS:
		case HR:
		case OH1:
		case OH2:
		case OH3:
		case OH4:
		case OH5:
		case OH6:
		case OPARA:
		case OULIST:
		case OOLIST:
		case ODLIST:
		case OPRE:
		case ODIV:
		case OCENTER:
		case OBQUOTE:
		case OTABLE:
		{
			body_tag();
			break;
		}
		case IMG:
		case BFONT:
		case BR:
		case OFORM:
		case OTTYPE:
		case OITALIC:
		case OBOLD:
		case OUNDER:
		case OSTRIKE:
		case OBIG:
		case OSMALL:
		case OSUB:
		case OSUP:
		case OEM:
		case OSTRONG:
		case ODFN:
		case OCODE:
		case OSAMP:
		case OKBD:
		case OVAR:
		case OCITE:
		case OANCHOR:
		case OAPPLET:
		case OFONT:
		case OMAP:
		{
			text_tag();
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_2);
	}
}

void HTMLParser::body_content() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case ADDRESS:
		case HR:
		case OH1:
		case OH2:
		case OH3:
		case OH4:
		case OH5:
		case OH6:
		case OPARA:
		case OULIST:
		case OOLIST:
		case ODLIST:
		case OPRE:
		case ODIV:
		case OCENTER:
		case OBQUOTE:
		case OTABLE:
		{
			body_tag();
			break;
		}
		case PCDATA:
		case IMG:
		case BFONT:
		case BR:
		case OFORM:
		case OTTYPE:
		case OITALIC:
		case OBOLD:
		case OUNDER:
		case OSTRIKE:
		case OBIG:
		case OSMALL:
		case OSUB:
		case OSUP:
		case OEM:
		case OSTRONG:
		case ODFN:
		case OCODE:
		case OSAMP:
		case OKBD:
		case OVAR:
		case OCITE:
		case OANCHOR:
		case OAPPLET:
		case OFONT:
		case OMAP:
		{
			text();
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_7);
	}
}

void HTMLParser::body_tag() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case OH1:
		case OH2:
		case OH3:
		case OH4:
		case OH5:
		case OH6:
		{
			heading();
			break;
		}
		case HR:
		case OPARA:
		case OULIST:
		case OOLIST:
		case ODLIST:
		case OPRE:
		case ODIV:
		case OCENTER:
		case OBQUOTE:
		case OTABLE:
		{
			block();
			break;
		}
		case ADDRESS:
		{
			match(ADDRESS);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_7);
	}
}

void HTMLParser::text_tag() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case OTTYPE:
		case OITALIC:
		case OBOLD:
		case OUNDER:
		case OSTRIKE:
		case OBIG:
		case OSMALL:
		case OSUB:
		case OSUP:
		{
			font();
			break;
		}
		case OEM:
		case OSTRONG:
		case ODFN:
		case OCODE:
		case OSAMP:
		case OKBD:
		case OVAR:
		case OCITE:
		{
			phrase();
			break;
		}
		case IMG:
		case BFONT:
		case BR:
		case OANCHOR:
		case OAPPLET:
		case OFONT:
		case OMAP:
		{
			special();
			break;
		}
		case OFORM:
		{
			form();
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::heading() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case OH1:
		{
			h1();
			break;
		}
		case OH2:
		{
			h2();
			break;
		}
		case OH3:
		{
			h3();
			break;
		}
		case OH4:
		{
			h4();
			break;
		}
		case OH5:
		{
			h5();
			break;
		}
		case OH6:
		{
			h6();
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_7);
	}
}

void HTMLParser::block() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case OPARA:
		{
			paragraph();
			break;
		}
		case OULIST:
		case OOLIST:
		case ODLIST:
		{
			list();
			break;
		}
		case OPRE:
		{
			preformatted();
			break;
		}
		case ODIV:
		{
			div();
			break;
		}
		case OCENTER:
		{
			center();
			break;
		}
		case OBQUOTE:
		{
			blockquote();
			break;
		}
		case HR:
		{
			match(HR);
			break;
		}
		case OTABLE:
		{
			table();
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_9);
	}
}

void HTMLParser::text() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case PCDATA:
		{
			match(PCDATA);
			break;
		}
		case IMG:
		case BFONT:
		case BR:
		case OFORM:
		case OTTYPE:
		case OITALIC:
		case OBOLD:
		case OUNDER:
		case OSTRIKE:
		case OBIG:
		case OSMALL:
		case OSUB:
		case OSUP:
		case OEM:
		case OSTRONG:
		case ODFN:
		case OCODE:
		case OSAMP:
		case OKBD:
		case OVAR:
		case OCITE:
		case OANCHOR:
		case OAPPLET:
		case OFONT:
		case OMAP:
		{
			text_tag();
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::h1() {
	
	try {      // for error handling
		match(OH1);
		{ // ( ... )*
		for (;;) {
			switch ( LA(1)) {
			case HR:
			case OPARA:
			case OULIST:
			case OOLIST:
			case ODLIST:
			case OPRE:
			case ODIV:
			case OCENTER:
			case OBQUOTE:
			case OTABLE:
			{
				block();
				break;
			}
			case PCDATA:
			case IMG:
			case BFONT:
			case BR:
			case OFORM:
			case OTTYPE:
			case OITALIC:
			case OBOLD:
			case OUNDER:
			case OSTRIKE:
			case OBIG:
			case OSMALL:
			case OSUB:
			case OSUP:
			case OEM:
			case OSTRONG:
			case ODFN:
			case OCODE:
			case OSAMP:
			case OKBD:
			case OVAR:
			case OCITE:
			case OANCHOR:
			case OAPPLET:
			case OFONT:
			case OMAP:
			{
				text();
				break;
			}
			default:
			{
				goto _loop48;
			}
			}
		}
		_loop48:;
		} // ( ... )*
		match(CH1);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_7);
	}
}

void HTMLParser::h2() {
	
	try {      // for error handling
		match(OH2);
		{ // ( ... )*
		for (;;) {
			switch ( LA(1)) {
			case HR:
			case OPARA:
			case OULIST:
			case OOLIST:
			case ODLIST:
			case OPRE:
			case ODIV:
			case OCENTER:
			case OBQUOTE:
			case OTABLE:
			{
				block();
				break;
			}
			case PCDATA:
			case IMG:
			case BFONT:
			case BR:
			case OFORM:
			case OTTYPE:
			case OITALIC:
			case OBOLD:
			case OUNDER:
			case OSTRIKE:
			case OBIG:
			case OSMALL:
			case OSUB:
			case OSUP:
			case OEM:
			case OSTRONG:
			case ODFN:
			case OCODE:
			case OSAMP:
			case OKBD:
			case OVAR:
			case OCITE:
			case OANCHOR:
			case OAPPLET:
			case OFONT:
			case OMAP:
			{
				text();
				break;
			}
			default:
			{
				goto _loop51;
			}
			}
		}
		_loop51:;
		} // ( ... )*
		match(CH2);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_7);
	}
}

void HTMLParser::h3() {
	
	try {      // for error handling
		match(OH3);
		{ // ( ... )*
		for (;;) {
			switch ( LA(1)) {
			case HR:
			case OPARA:
			case OULIST:
			case OOLIST:
			case ODLIST:
			case OPRE:
			case ODIV:
			case OCENTER:
			case OBQUOTE:
			case OTABLE:
			{
				block();
				break;
			}
			case PCDATA:
			case IMG:
			case BFONT:
			case BR:
			case OFORM:
			case OTTYPE:
			case OITALIC:
			case OBOLD:
			case OUNDER:
			case OSTRIKE:
			case OBIG:
			case OSMALL:
			case OSUB:
			case OSUP:
			case OEM:
			case OSTRONG:
			case ODFN:
			case OCODE:
			case OSAMP:
			case OKBD:
			case OVAR:
			case OCITE:
			case OANCHOR:
			case OAPPLET:
			case OFONT:
			case OMAP:
			{
				text();
				break;
			}
			default:
			{
				goto _loop54;
			}
			}
		}
		_loop54:;
		} // ( ... )*
		match(CH3);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_7);
	}
}

void HTMLParser::h4() {
	
	try {      // for error handling
		match(OH4);
		{ // ( ... )*
		for (;;) {
			switch ( LA(1)) {
			case HR:
			case OPARA:
			case OULIST:
			case OOLIST:
			case ODLIST:
			case OPRE:
			case ODIV:
			case OCENTER:
			case OBQUOTE:
			case OTABLE:
			{
				block();
				break;
			}
			case PCDATA:
			case IMG:
			case BFONT:
			case BR:
			case OFORM:
			case OTTYPE:
			case OITALIC:
			case OBOLD:
			case OUNDER:
			case OSTRIKE:
			case OBIG:
			case OSMALL:
			case OSUB:
			case OSUP:
			case OEM:
			case OSTRONG:
			case ODFN:
			case OCODE:
			case OSAMP:
			case OKBD:
			case OVAR:
			case OCITE:
			case OANCHOR:
			case OAPPLET:
			case OFONT:
			case OMAP:
			{
				text();
				break;
			}
			default:
			{
				goto _loop57;
			}
			}
		}
		_loop57:;
		} // ( ... )*
		match(CH4);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_7);
	}
}

void HTMLParser::h5() {
	
	try {      // for error handling
		match(OH5);
		{ // ( ... )*
		for (;;) {
			switch ( LA(1)) {
			case HR:
			case OPARA:
			case OULIST:
			case OOLIST:
			case ODLIST:
			case OPRE:
			case ODIV:
			case OCENTER:
			case OBQUOTE:
			case OTABLE:
			{
				block();
				break;
			}
			case PCDATA:
			case IMG:
			case BFONT:
			case BR:
			case OFORM:
			case OTTYPE:
			case OITALIC:
			case OBOLD:
			case OUNDER:
			case OSTRIKE:
			case OBIG:
			case OSMALL:
			case OSUB:
			case OSUP:
			case OEM:
			case OSTRONG:
			case ODFN:
			case OCODE:
			case OSAMP:
			case OKBD:
			case OVAR:
			case OCITE:
			case OANCHOR:
			case OAPPLET:
			case OFONT:
			case OMAP:
			{
				text();
				break;
			}
			default:
			{
				goto _loop60;
			}
			}
		}
		_loop60:;
		} // ( ... )*
		match(CH5);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_7);
	}
}

void HTMLParser::h6() {
	
	try {      // for error handling
		match(OH6);
		{ // ( ... )*
		for (;;) {
			switch ( LA(1)) {
			case HR:
			case OPARA:
			case OULIST:
			case OOLIST:
			case ODLIST:
			case OPRE:
			case ODIV:
			case OCENTER:
			case OBQUOTE:
			case OTABLE:
			{
				block();
				break;
			}
			case PCDATA:
			case IMG:
			case BFONT:
			case BR:
			case OFORM:
			case OTTYPE:
			case OITALIC:
			case OBOLD:
			case OUNDER:
			case OSTRIKE:
			case OBIG:
			case OSMALL:
			case OSUB:
			case OSUP:
			case OEM:
			case OSTRONG:
			case ODFN:
			case OCODE:
			case OSAMP:
			case OKBD:
			case OVAR:
			case OCITE:
			case OANCHOR:
			case OAPPLET:
			case OFONT:
			case OMAP:
			{
				text();
				break;
			}
			default:
			{
				goto _loop63;
			}
			}
		}
		_loop63:;
		} // ( ... )*
		match(CH6);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_7);
	}
}

void HTMLParser::paragraph() {
	
	try {      // for error handling
		match(OPARA);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				goto _loop69;
			}
			
		}
		_loop69:;
		} // ( ... )*
		{
		switch ( LA(1)) {
		case CPARA:
		{
			match(CPARA);
			break;
		}
		case ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE:
		case PCDATA:
		case CHTML:
		case CBODY:
		case ADDRESS:
		case HR:
		case IMG:
		case BFONT:
		case BR:
		case OH1:
		case CH1:
		case OH2:
		case CH2:
		case OH3:
		case CH3:
		case OH4:
		case CH4:
		case OH5:
		case CH5:
		case OH6:
		case CH6:
		case OPARA:
		case OULIST:
		case OOLIST:
		case ODLIST:
		case CDTERM:
		case OPRE:
		case ODIV:
		case CDIV:
		case OCENTER:
		case CCENTER:
		case OBQUOTE:
		case CBQUOTE:
		case OFORM:
		case CFORM:
		case OTABLE:
		case CTABLE:
		case O_TR:
		case C_TR:
		case O_TH_OR_TD:
		case C_TH_OR_TD:
		case OTTYPE:
		case OITALIC:
		case OBOLD:
		case OUNDER:
		case OSTRIKE:
		case OBIG:
		case OSMALL:
		case OSUB:
		case OSUP:
		case OEM:
		case OSTRONG:
		case ODFN:
		case OCODE:
		case OSAMP:
		case OKBD:
		case OVAR:
		case OCITE:
		case INPUT:
		case OSELECT:
		case OTAREA:
		case OANCHOR:
		case OAPPLET:
		case OFONT:
		case OMAP:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_9);
	}
}

void HTMLParser::list() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case OULIST:
		{
			unordered_list();
			break;
		}
		case OOLIST:
		{
			ordered_list();
			break;
		}
		case ODLIST:
		{
			def_list();
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_11);
	}
}

void HTMLParser::preformatted() {
	
	try {      // for error handling
		match(OPRE);
		{ // ( ... )+
		int _cnt112=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt112>=1 ) { goto _loop112; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt112++;
		}
		_loop112:;
		}  // ( ... )+
		match(CPRE);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_9);
	}
}

void HTMLParser::div() {
	
	try {      // for error handling
		match(ODIV);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_2.member(LA(1)))) {
				body_content();
			}
			else {
				goto _loop115;
			}
			
		}
		_loop115:;
		} // ( ... )*
		match(CDIV);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_9);
	}
}

void HTMLParser::center() {
	
	try {      // for error handling
		match(OCENTER);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_2.member(LA(1)))) {
				body_content();
			}
			else {
				goto _loop118;
			}
			
		}
		_loop118:;
		} // ( ... )*
		match(CCENTER);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_9);
	}
}

void HTMLParser::blockquote() {
	
	try {      // for error handling
		match(OBQUOTE);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_2.member(LA(1)))) {
				body_content();
			}
			else {
				goto _loop121;
			}
			
		}
		_loop121:;
		} // ( ... )*
		match(CBQUOTE);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_9);
	}
}

void HTMLParser::table() {
	
	try {      // for error handling
		match(OTABLE);
		{
		switch ( LA(1)) {
		case OCAP:
		{
			caption();
			break;
		}
		case PCDATA:
		case O_TR:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == PCDATA)) {
				match(PCDATA);
			}
			else {
				goto _loop128;
			}
			
		}
		_loop128:;
		} // ( ... )*
		{ // ( ... )+
		int _cnt130=0;
		for (;;) {
			if ((LA(1) == O_TR)) {
				tr();
			}
			else {
				if ( _cnt130>=1 ) { goto _loop130; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt130++;
		}
		_loop130:;
		}  // ( ... )+
		match(CTABLE);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_9);
	}
}

void HTMLParser::font() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case OTTYPE:
		{
			teletype();
			break;
		}
		case OITALIC:
		{
			italic();
			break;
		}
		case OBOLD:
		{
			bold();
			break;
		}
		case OUNDER:
		{
			underline();
			break;
		}
		case OSTRIKE:
		{
			strike();
			break;
		}
		case OBIG:
		{
			big();
			break;
		}
		case OSMALL:
		{
			small();
			break;
		}
		case OSUB:
		{
			subscript();
			break;
		}
		case OSUP:
		{
			superscript();
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::teletype() {
	
	try {      // for error handling
		match(OTTYPE);
		{ // ( ... )+
		int _cnt150=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt150>=1 ) { goto _loop150; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt150++;
		}
		_loop150:;
		}  // ( ... )+
		match(CTTYPE);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::italic() {
	
	try {      // for error handling
		match(OITALIC);
		{ // ( ... )+
		int _cnt153=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt153>=1 ) { goto _loop153; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt153++;
		}
		_loop153:;
		}  // ( ... )+
		match(CITALIC);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::bold() {
	
	try {      // for error handling
		match(OBOLD);
		{ // ( ... )+
		int _cnt156=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt156>=1 ) { goto _loop156; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt156++;
		}
		_loop156:;
		}  // ( ... )+
		match(CBOLD);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::underline() {
	
	try {      // for error handling
		match(OUNDER);
		{ // ( ... )+
		int _cnt159=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt159>=1 ) { goto _loop159; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt159++;
		}
		_loop159:;
		}  // ( ... )+
		match(CUNDER);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::strike() {
	
	try {      // for error handling
		match(OSTRIKE);
		{ // ( ... )+
		int _cnt162=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt162>=1 ) { goto _loop162; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt162++;
		}
		_loop162:;
		}  // ( ... )+
		match(CSTRIKE);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::big() {
	
	try {      // for error handling
		match(OBIG);
		{ // ( ... )+
		int _cnt165=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt165>=1 ) { goto _loop165; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt165++;
		}
		_loop165:;
		}  // ( ... )+
		match(CBIG);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::small() {
	
	try {      // for error handling
		match(OSMALL);
		{ // ( ... )+
		int _cnt168=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt168>=1 ) { goto _loop168; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt168++;
		}
		_loop168:;
		}  // ( ... )+
		match(CSMALL);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::subscript() {
	
	try {      // for error handling
		match(OSUB);
		{ // ( ... )+
		int _cnt171=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt171>=1 ) { goto _loop171; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt171++;
		}
		_loop171:;
		}  // ( ... )+
		match(CSUB);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::superscript() {
	
	try {      // for error handling
		match(OSUP);
		{ // ( ... )+
		int _cnt174=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt174>=1 ) { goto _loop174; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt174++;
		}
		_loop174:;
		}  // ( ... )+
		match(CSUP);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::phrase() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case OEM:
		{
			emphasize();
			break;
		}
		case OSTRONG:
		{
			strong();
			break;
		}
		case ODFN:
		{
			definition();
			break;
		}
		case OCODE:
		{
			code();
			break;
		}
		case OSAMP:
		{
			sample_output();
			break;
		}
		case OKBD:
		{
			keyboard_text();
			break;
		}
		case OVAR:
		{
			variable();
			break;
		}
		case OCITE:
		{
			citation();
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::emphasize() {
	
	try {      // for error handling
		match(OEM);
		{ // ( ... )+
		int _cnt177=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt177>=1 ) { goto _loop177; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt177++;
		}
		_loop177:;
		}  // ( ... )+
		match(CEM);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::strong() {
	
	try {      // for error handling
		match(OSTRONG);
		{ // ( ... )+
		int _cnt180=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt180>=1 ) { goto _loop180; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt180++;
		}
		_loop180:;
		}  // ( ... )+
		match(CSTRONG);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::definition() {
	
	try {      // for error handling
		match(ODFN);
		{ // ( ... )+
		int _cnt183=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt183>=1 ) { goto _loop183; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt183++;
		}
		_loop183:;
		}  // ( ... )+
		match(CDFN);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::code() {
	
	try {      // for error handling
		match(OCODE);
		{ // ( ... )+
		int _cnt186=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt186>=1 ) { goto _loop186; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt186++;
		}
		_loop186:;
		}  // ( ... )+
		match(CCODE);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::sample_output() {
	
	try {      // for error handling
		match(OSAMP);
		{ // ( ... )+
		int _cnt189=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt189>=1 ) { goto _loop189; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt189++;
		}
		_loop189:;
		}  // ( ... )+
		match(CSAMP);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::keyboard_text() {
	
	try {      // for error handling
		match(OKBD);
		{ // ( ... )+
		int _cnt192=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt192>=1 ) { goto _loop192; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt192++;
		}
		_loop192:;
		}  // ( ... )+
		match(CKBD);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::variable() {
	
	try {      // for error handling
		match(OVAR);
		{ // ( ... )+
		int _cnt195=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt195>=1 ) { goto _loop195; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt195++;
		}
		_loop195:;
		}  // ( ... )+
		match(CVAR);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::citation() {
	
	try {      // for error handling
		match(OCITE);
		{ // ( ... )+
		int _cnt198=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt198>=1 ) { goto _loop198; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt198++;
		}
		_loop198:;
		}  // ( ... )+
		match(CCITE);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::special() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case OANCHOR:
		{
			anchor();
			break;
		}
		case IMG:
		{
			match(IMG);
			break;
		}
		case OAPPLET:
		{
			applet();
			break;
		}
		case OFONT:
		{
			font_dfn();
			break;
		}
		case BFONT:
		{
			match(BFONT);
			break;
		}
		case OMAP:
		{
			map();
			break;
		}
		case BR:
		{
			match(BR);
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::anchor() {
	
	try {      // for error handling
		match(OANCHOR);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				goto _loop213;
			}
			
		}
		_loop213:;
		} // ( ... )*
		match(CANCHOR);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::applet() {
	
	try {      // for error handling
		match(OAPPLET);
		{
		switch ( LA(1)) {
		case APARAM:
		{
			match(APARAM);
			break;
		}
		case PCDATA:
		case CAPPLET:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == PCDATA)) {
				match(PCDATA);
			}
			else {
				goto _loop217;
			}
			
		}
		_loop217:;
		} // ( ... )*
		match(CAPPLET);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::font_dfn() {
	
	try {      // for error handling
		match(OFONT);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				goto _loop220;
			}
			
		}
		_loop220:;
		} // ( ... )*
		match(CFONT);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::map() {
	
	try {      // for error handling
		match(OMAP);
		{ // ( ... )+
		int _cnt223=0;
		for (;;) {
			if ((LA(1) == AREA)) {
				match(AREA);
			}
			else {
				if ( _cnt223>=1 ) { goto _loop223; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt223++;
		}
		_loop223:;
		}  // ( ... )+
		match(CMAP);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::form() {
	
	try {      // for error handling
		match(OFORM);
		{ // ( ... )*
		for (;;) {
			switch ( LA(1)) {
			case INPUT:
			case OSELECT:
			case OTAREA:
			{
				form_field();
				break;
			}
			case PCDATA:
			case ADDRESS:
			case HR:
			case IMG:
			case BFONT:
			case BR:
			case OH1:
			case OH2:
			case OH3:
			case OH4:
			case OH5:
			case OH6:
			case OPARA:
			case OULIST:
			case OOLIST:
			case ODLIST:
			case OPRE:
			case ODIV:
			case OCENTER:
			case OBQUOTE:
			case OFORM:
			case OTABLE:
			case OTTYPE:
			case OITALIC:
			case OBOLD:
			case OUNDER:
			case OSTRIKE:
			case OBIG:
			case OSMALL:
			case OSUB:
			case OSUP:
			case OEM:
			case OSTRONG:
			case ODFN:
			case OCODE:
			case OSAMP:
			case OKBD:
			case OVAR:
			case OCITE:
			case OANCHOR:
			case OAPPLET:
			case OFONT:
			case OMAP:
			{
				body_content();
				break;
			}
			default:
			{
				goto _loop124;
			}
			}
		}
		_loop124:;
		} // ( ... )*
		match(CFORM);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_8);
	}
}

void HTMLParser::address() {
	
	try {      // for error handling
		match(OADDRESS);
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == PCDATA)) {
				match(PCDATA);
			}
			else {
				goto _loop66;
			}
			
		}
		_loop66:;
		} // ( ... )*
		match(CADDRESS);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_0);
	}
}

void HTMLParser::unordered_list() {
	
	try {      // for error handling
		match(OULIST);
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == PCDATA)) {
				match(PCDATA);
			}
			else {
				goto _loop74;
			}
			
		}
		_loop74:;
		} // ( ... )*
		{ // ( ... )+
		int _cnt76=0;
		for (;;) {
			if ((LA(1) == OLITEM)) {
				list_item();
			}
			else {
				if ( _cnt76>=1 ) { goto _loop76; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt76++;
		}
		_loop76:;
		}  // ( ... )+
		match(CULIST);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_11);
	}
}

void HTMLParser::ordered_list() {
	
	try {      // for error handling
		match(OOLIST);
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == PCDATA)) {
				match(PCDATA);
			}
			else {
				goto _loop79;
			}
			
		}
		_loop79:;
		} // ( ... )*
		{ // ( ... )+
		int _cnt81=0;
		for (;;) {
			if ((LA(1) == OLITEM)) {
				list_item();
			}
			else {
				if ( _cnt81>=1 ) { goto _loop81; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt81++;
		}
		_loop81:;
		}  // ( ... )+
		match(COLIST);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_11);
	}
}

void HTMLParser::def_list() {
	
	try {      // for error handling
		match(ODLIST);
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == PCDATA)) {
				match(PCDATA);
			}
			else {
				goto _loop84;
			}
			
		}
		_loop84:;
		} // ( ... )*
		{ // ( ... )+
		int _cnt86=0;
		for (;;) {
			if ((LA(1) == ODTERM || LA(1) == ODDEF)) {
				def_list_item();
			}
			else {
				if ( _cnt86>=1 ) { goto _loop86; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt86++;
		}
		_loop86:;
		}  // ( ... )+
		match(CDLIST);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_11);
	}
}

void HTMLParser::list_item() {
	
	try {      // for error handling
		match(OLITEM);
		{ // ( ... )+
		int _cnt89=0;
		for (;;) {
			switch ( LA(1)) {
			case PCDATA:
			case IMG:
			case BFONT:
			case BR:
			case OFORM:
			case OTTYPE:
			case OITALIC:
			case OBOLD:
			case OUNDER:
			case OSTRIKE:
			case OBIG:
			case OSMALL:
			case OSUB:
			case OSUP:
			case OEM:
			case OSTRONG:
			case ODFN:
			case OCODE:
			case OSAMP:
			case OKBD:
			case OVAR:
			case OCITE:
			case OANCHOR:
			case OAPPLET:
			case OFONT:
			case OMAP:
			{
				text();
				break;
			}
			case OULIST:
			case OOLIST:
			case ODLIST:
			{
				list();
				break;
			}
			default:
			{
				if ( _cnt89>=1 ) { goto _loop89; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			}
			_cnt89++;
		}
		_loop89:;
		}  // ( ... )+
		{
		switch ( LA(1)) {
		case CLITEM:
		{
			match(CLITEM);
			{ // ( ... )*
			for (;;) {
				if ((LA(1) == PCDATA)) {
					match(PCDATA);
				}
				else {
					goto _loop92;
				}
				
			}
			_loop92:;
			} // ( ... )*
			break;
		}
		case CULIST:
		case COLIST:
		case OLITEM:
		case CDIR:
		case CMENU:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_12);
	}
}

void HTMLParser::def_list_item() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case ODTERM:
		{
			dt();
			break;
		}
		case ODDEF:
		{
			dd();
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_13);
	}
}

void HTMLParser::dt() {
	
	try {      // for error handling
		match(ODTERM);
		{ // ( ... )+
		int _cnt96=0;
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				if ( _cnt96>=1 ) { goto _loop96; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt96++;
		}
		_loop96:;
		}  // ( ... )+
		match(CDTERM);
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == PCDATA)) {
				match(PCDATA);
			}
			else {
				goto _loop98;
			}
			
		}
		_loop98:;
		} // ( ... )*
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_13);
	}
}

void HTMLParser::dd() {
	
	try {      // for error handling
		match(ODDEF);
		{ // ( ... )+
		int _cnt101=0;
		for (;;) {
			switch ( LA(1)) {
			case PCDATA:
			case IMG:
			case BFONT:
			case BR:
			case OFORM:
			case OTTYPE:
			case OITALIC:
			case OBOLD:
			case OUNDER:
			case OSTRIKE:
			case OBIG:
			case OSMALL:
			case OSUB:
			case OSUP:
			case OEM:
			case OSTRONG:
			case ODFN:
			case OCODE:
			case OSAMP:
			case OKBD:
			case OVAR:
			case OCITE:
			case OANCHOR:
			case OAPPLET:
			case OFONT:
			case OMAP:
			{
				text();
				break;
			}
			case HR:
			case OPARA:
			case OULIST:
			case OOLIST:
			case ODLIST:
			case OPRE:
			case ODIV:
			case OCENTER:
			case OBQUOTE:
			case OTABLE:
			{
				block();
				break;
			}
			default:
			{
				if ( _cnt101>=1 ) { goto _loop101; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			}
			_cnt101++;
		}
		_loop101:;
		}  // ( ... )+
		match(CDTERM);
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == PCDATA)) {
				match(PCDATA);
			}
			else {
				goto _loop103;
			}
			
		}
		_loop103:;
		} // ( ... )*
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_13);
	}
}

void HTMLParser::dir() {
	
	try {      // for error handling
		match(ODIR);
		{ // ( ... )+
		int _cnt106=0;
		for (;;) {
			if ((LA(1) == OLITEM)) {
				list_item();
			}
			else {
				if ( _cnt106>=1 ) { goto _loop106; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt106++;
		}
		_loop106:;
		}  // ( ... )+
		match(CDIR);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_0);
	}
}

void HTMLParser::menu() {
	
	try {      // for error handling
		match(OMENU);
		{ // ( ... )+
		int _cnt109=0;
		for (;;) {
			if ((LA(1) == OLITEM)) {
				list_item();
			}
			else {
				if ( _cnt109>=1 ) { goto _loop109; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt109++;
		}
		_loop109:;
		}  // ( ... )+
		match(CMENU);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_0);
	}
}

void HTMLParser::form_field() {
	
	try {      // for error handling
		switch ( LA(1)) {
		case INPUT:
		{
			match(INPUT);
			break;
		}
		case OSELECT:
		{
			select();
			break;
		}
		case OTAREA:
		{
			textarea();
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_14);
	}
}

void HTMLParser::caption() {
	
	try {      // for error handling
		match(OCAP);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_10.member(LA(1)))) {
				text();
			}
			else {
				goto _loop133;
			}
			
		}
		_loop133:;
		} // ( ... )*
		match(CCAP);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_15);
	}
}

void HTMLParser::tr() {
	
	try {      // for error handling
		match(O_TR);
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == PCDATA)) {
				match(PCDATA);
			}
			else {
				goto _loop136;
			}
			
		}
		_loop136:;
		} // ( ... )*
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == O_TH_OR_TD)) {
				th_or_td();
			}
			else {
				goto _loop138;
			}
			
		}
		_loop138:;
		} // ( ... )*
		{
		switch ( LA(1)) {
		case C_TR:
		{
			match(C_TR);
			{ // ( ... )*
			for (;;) {
				if ((LA(1) == PCDATA)) {
					match(PCDATA);
				}
				else {
					goto _loop141;
				}
				
			}
			_loop141:;
			} // ( ... )*
			break;
		}
		case CTABLE:
		case O_TR:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_16);
	}
}

void HTMLParser::th_or_td() {
	
	try {      // for error handling
		match(O_TH_OR_TD);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_2.member(LA(1)))) {
				body_content();
			}
			else {
				goto _loop144;
			}
			
		}
		_loop144:;
		} // ( ... )*
		{
		switch ( LA(1)) {
		case C_TH_OR_TD:
		{
			match(C_TH_OR_TD);
			{ // ( ... )*
			for (;;) {
				if ((LA(1) == PCDATA)) {
					match(PCDATA);
				}
				else {
					goto _loop147;
				}
				
			}
			_loop147:;
			} // ( ... )*
			break;
		}
		case CTABLE:
		case O_TR:
		case C_TR:
		case O_TH_OR_TD:
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());
		}
		}
		}
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_17);
	}
}

void HTMLParser::select() {
	
	try {      // for error handling
		match(OSELECT);
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == PCDATA)) {
				match(PCDATA);
			}
			else {
				goto _loop202;
			}
			
		}
		_loop202:;
		} // ( ... )*
		{ // ( ... )+
		int _cnt204=0;
		for (;;) {
			if ((LA(1) == SELOPT)) {
				select_option();
			}
			else {
				if ( _cnt204>=1 ) { goto _loop204; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt204++;
		}
		_loop204:;
		}  // ( ... )+
		match(CSELECT);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_14);
	}
}

void HTMLParser::textarea() {
	
	try {      // for error handling
		match(OTAREA);
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == PCDATA)) {
				match(PCDATA);
			}
			else {
				goto _loop210;
			}
			
		}
		_loop210:;
		} // ( ... )*
		match(CTAREA);
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_14);
	}
}

void HTMLParser::select_option() {
	
	try {      // for error handling
		match(SELOPT);
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == PCDATA)) {
				match(PCDATA);
			}
			else {
				goto _loop207;
			}
			
		}
		_loop207:;
		} // ( ... )*
	}
	catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& ex) {
		reportError(ex);
		recover(ex,_tokenSet_18);
	}
}

void HTMLParser::initializeASTFactory( ANTLR_USE_NAMESPACE(antlr)ASTFactory& )
{
}
const char* HTMLParser::tokenNames[] = {
	"<0>",
	"EOF",
	"<2>",
	"NULL_TREE_LOOKAHEAD",
	"PCDATA",
	"DOCTYPE",
	"OHTML",
	"CHTML",
	"OHEAD",
	"CHEAD",
	"ISINDEX",
	"BASE",
	"META",
	"LINK",
	"OTITLE",
	"CTITLE",
	"OSCRIPT",
	"CSCRIPT",
	"OSTYLE",
	"CSTYLE",
	"OBODY",
	"CBODY",
	"ADDRESS",
	"HR",
	"IMG",
	"BFONT",
	"BR",
	"OH1",
	"CH1",
	"OH2",
	"CH2",
	"OH3",
	"CH3",
	"OH4",
	"CH4",
	"OH5",
	"CH5",
	"OH6",
	"CH6",
	"OADDRESS",
	"CADDRESS",
	"OPARA",
	"CPARA",
	"OULIST",
	"CULIST",
	"OOLIST",
	"COLIST",
	"ODLIST",
	"CDLIST",
	"OLITEM",
	"CLITEM",
	"ODTERM",
	"CDTERM",
	"ODDEF",
	"ODIR",
	"CDIR",
	"OMENU",
	"CMENU",
	"OPRE",
	"CPRE",
	"ODIV",
	"CDIV",
	"OCENTER",
	"CCENTER",
	"OBQUOTE",
	"CBQUOTE",
	"OFORM",
	"CFORM",
	"OTABLE",
	"CTABLE",
	"OCAP",
	"CCAP",
	"O_TR",
	"C_TR",
	"O_TH_OR_TD",
	"C_TH_OR_TD",
	"OTTYPE",
	"CTTYPE",
	"OITALIC",
	"CITALIC",
	"OBOLD",
	"CBOLD",
	"OUNDER",
	"CUNDER",
	"OSTRIKE",
	"CSTRIKE",
	"OBIG",
	"CBIG",
	"OSMALL",
	"CSMALL",
	"OSUB",
	"CSUB",
	"OSUP",
	"CSUP",
	"OEM",
	"CEM",
	"OSTRONG",
	"CSTRONG",
	"ODFN",
	"CDFN",
	"OCODE",
	"CCODE",
	"OSAMP",
	"CSAMP",
	"OKBD",
	"CKBD",
	"OVAR",
	"CVAR",
	"OCITE",
	"CCITE",
	"INPUT",
	"OSELECT",
	"CSELECT",
	"SELOPT",
	"OTAREA",
	"CTAREA",
	"OANCHOR",
	"CANCHOR",
	"OAPPLET",
	"APARAM",
	"CAPPLET",
	"OFONT",
	"CFONT",
	"OMAP",
	"AREA",
	"CMAP",
	"CDDEF",
	"CDIR_OR_CDIV",
	"OSTRIKE_OR_OSTRONG",
	"CST_LEFT_FACTORED",
	"CSUB_OR_CSUP",
	"APARM",
	"CFORM_OR_CFONT",
	"BFONT_OR_BASE",
	"COMMENT_DATA",
	"COMMENT",
	"WS_",
	"ATTR",
	"WORD",
	"STRING",
	"WSCHARS",
	"SPECIAL",
	"HEXNUM",
	"INT",
	"HEXINT",
	"DIGIT",
	"HEXDIGIT",
	"LCLETTER",
	"UNDEFINED_TOKEN",
	0
};

const unsigned long HTMLParser::_tokenSet_0_data_[] = { 2UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// EOF 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_0(_tokenSet_0_data_,6);
const unsigned long HTMLParser::_tokenSet_1_data_[] = { 2949644418UL, 1409329706UL, 1431654421UL, 173020501UL, 0UL, 0UL, 0UL, 0UL };
// EOF CHTML OBODY ADDRESS HR IMG BFONT BR OH1 OH2 OH3 OH4 OH5 OH6 OPARA 
// OULIST OOLIST ODLIST OPRE ODIV OCENTER OBQUOTE OFORM OTABLE OTTYPE OITALIC 
// OBOLD OUNDER OSTRIKE OBIG OSMALL OSUB OSUP OEM OSTRONG ODFN OCODE OSAMP 
// OKBD OVAR OCITE OANCHOR OAPPLET OFONT OMAP 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_1(_tokenSet_1_data_,8);
const unsigned long HTMLParser::_tokenSet_2_data_[] = { 2948595728UL, 1409329706UL, 1431654421UL, 173020501UL, 0UL, 0UL, 0UL, 0UL };
// PCDATA ADDRESS HR IMG BFONT BR OH1 OH2 OH3 OH4 OH5 OH6 OPARA OULIST 
// OOLIST ODLIST OPRE ODIV OCENTER OBQUOTE OFORM OTABLE OTTYPE OITALIC 
// OBOLD OUNDER OSTRIKE OBIG OSMALL OSUB OSUP OEM OSTRONG ODFN OCODE OSAMP 
// OKBD OVAR OCITE OANCHOR OAPPLET OFONT OMAP 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_2(_tokenSet_2_data_,8);
const unsigned long HTMLParser::_tokenSet_3_data_[] = { 130UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// EOF CHTML 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_3(_tokenSet_3_data_,6);
const unsigned long HTMLParser::_tokenSet_4_data_[] = { 2950004370UL, 1409329706UL, 1431654421UL, 173020501UL, 0UL, 0UL, 0UL, 0UL };
// EOF PCDATA CHTML CHEAD ISINDEX BASE META LINK OTITLE OSCRIPT OSTYLE 
// OBODY ADDRESS HR IMG BFONT BR OH1 OH2 OH3 OH4 OH5 OH6 OPARA OULIST OOLIST 
// ODLIST OPRE ODIV OCENTER OBQUOTE OFORM OTABLE OTTYPE OITALIC OBOLD OUNDER 
// OSTRIKE OBIG OSMALL OSUB OSUP OEM OSTRONG ODFN OCODE OSAMP OKBD OVAR 
// OCITE OANCHOR OAPPLET OFONT OMAP 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_4(_tokenSet_4_data_,8);
const unsigned long HTMLParser::_tokenSet_5_data_[] = { 4294836208UL, 4294967295UL, 4294967295UL, 4294967295UL, 2097151UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// PCDATA DOCTYPE OHTML CHTML OHEAD CHEAD ISINDEX BASE META LINK OTITLE 
// CTITLE OSCRIPT OSTYLE CSTYLE OBODY CBODY ADDRESS HR IMG BFONT BR OH1 
// CH1 OH2 CH2 OH3 CH3 OH4 CH4 OH5 CH5 OH6 CH6 OADDRESS CADDRESS OPARA 
// CPARA OULIST CULIST OOLIST COLIST ODLIST CDLIST OLITEM CLITEM ODTERM 
// CDTERM ODDEF ODIR CDIR OMENU CMENU OPRE CPRE ODIV CDIV OCENTER CCENTER 
// OBQUOTE CBQUOTE OFORM CFORM OTABLE CTABLE OCAP CCAP O_TR C_TR O_TH_OR_TD 
// C_TH_OR_TD OTTYPE CTTYPE OITALIC CITALIC OBOLD CBOLD OUNDER CUNDER OSTRIKE 
// CSTRIKE OBIG CBIG OSMALL CSMALL OSUB CSUB OSUP CSUP OEM CEM OSTRONG 
// CSTRONG ODFN CDFN OCODE CCODE OSAMP CSAMP OKBD CKBD OVAR CVAR OCITE 
// CCITE INPUT OSELECT CSELECT SELOPT OTAREA CTAREA OANCHOR CANCHOR OAPPLET 
// APARAM CAPPLET OFONT CFONT OMAP AREA CMAP CDDEF CDIR_OR_CDIV OSTRIKE_OR_OSTRONG 
// CST_LEFT_FACTORED CSUB_OR_CSUP APARM CFORM_OR_CFONT BFONT_OR_BASE COMMENT_DATA 
// COMMENT WS_ ATTR WORD STRING WSCHARS SPECIAL HEXNUM INT HEXINT DIGIT 
// HEXDIGIT LCLETTER UNDEFINED_TOKEN 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_5(_tokenSet_5_data_,12);
const unsigned long HTMLParser::_tokenSet_6_data_[] = { 4294442992UL, 4294967295UL, 4294967295UL, 4294967295UL, 2097151UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// PCDATA DOCTYPE OHTML CHTML OHEAD CHEAD ISINDEX BASE META LINK OTITLE 
// CTITLE OSCRIPT CSCRIPT OSTYLE OBODY CBODY ADDRESS HR IMG BFONT BR OH1 
// CH1 OH2 CH2 OH3 CH3 OH4 CH4 OH5 CH5 OH6 CH6 OADDRESS CADDRESS OPARA 
// CPARA OULIST CULIST OOLIST COLIST ODLIST CDLIST OLITEM CLITEM ODTERM 
// CDTERM ODDEF ODIR CDIR OMENU CMENU OPRE CPRE ODIV CDIV OCENTER CCENTER 
// OBQUOTE CBQUOTE OFORM CFORM OTABLE CTABLE OCAP CCAP O_TR C_TR O_TH_OR_TD 
// C_TH_OR_TD OTTYPE CTTYPE OITALIC CITALIC OBOLD CBOLD OUNDER CUNDER OSTRIKE 
// CSTRIKE OBIG CBIG OSMALL CSMALL OSUB CSUB OSUP CSUP OEM CEM OSTRONG 
// CSTRONG ODFN CDFN OCODE CCODE OSAMP CSAMP OKBD CKBD OVAR CVAR OCITE 
// CCITE INPUT OSELECT CSELECT SELOPT OTAREA CTAREA OANCHOR CANCHOR OAPPLET 
// APARAM CAPPLET OFONT CFONT OMAP AREA CMAP CDDEF CDIR_OR_CDIV OSTRIKE_OR_OSTRONG 
// CST_LEFT_FACTORED CSUB_OR_CSUP APARM CFORM_OR_CFONT BFONT_OR_BASE COMMENT_DATA 
// COMMENT WS_ ATTR WORD STRING WSCHARS SPECIAL HEXNUM INT HEXINT DIGIT 
// HEXDIGIT LCLETTER UNDEFINED_TOKEN 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_6(_tokenSet_6_data_,12);
const unsigned long HTMLParser::_tokenSet_7_data_[] = { 2950693010UL, 4093684266UL, 1431658303UL, 173331797UL, 0UL, 0UL, 0UL, 0UL };
// EOF PCDATA CHTML CBODY ADDRESS HR IMG BFONT BR OH1 OH2 OH3 OH4 OH5 OH6 
// OPARA OULIST OOLIST ODLIST OPRE ODIV CDIV OCENTER CCENTER OBQUOTE CBQUOTE 
// OFORM CFORM OTABLE CTABLE O_TR C_TR O_TH_OR_TD C_TH_OR_TD OTTYPE OITALIC 
// OBOLD OUNDER OSTRIKE OBIG OSMALL OSUB OSUP OEM OSTRONG ODFN OCODE OSAMP 
// OKBD OVAR OCITE INPUT OSELECT OTAREA OANCHOR OAPPLET OFONT OMAP 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_7(_tokenSet_7_data_,8);
const unsigned long HTMLParser::_tokenSet_8_data_[] = { 4292870290UL, 4271308415UL, 4294967231UL, 242548735UL, 0UL, 0UL, 0UL, 0UL };
// EOF PCDATA CHTML CBODY ADDRESS HR IMG BFONT BR OH1 CH1 OH2 CH2 OH3 CH3 
// OH4 CH4 OH5 CH5 OH6 CH6 OPARA CPARA OULIST CULIST OOLIST COLIST ODLIST 
// OLITEM CLITEM CDTERM CDIR CMENU OPRE CPRE ODIV CDIV OCENTER CCENTER 
// OBQUOTE CBQUOTE OFORM CFORM OTABLE CTABLE CCAP O_TR C_TR O_TH_OR_TD 
// C_TH_OR_TD OTTYPE CTTYPE OITALIC CITALIC OBOLD CBOLD OUNDER CUNDER OSTRIKE 
// CSTRIKE OBIG CBIG OSMALL CSMALL OSUB CSUB OSUP CSUP OEM CEM OSTRONG 
// CSTRONG ODFN CDFN OCODE CCODE OSAMP CSAMP OKBD CKBD OVAR CVAR OCITE 
// CCITE INPUT OSELECT OTAREA OANCHOR CANCHOR OAPPLET OFONT CFONT OMAP 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_8(_tokenSet_8_data_,8);
const unsigned long HTMLParser::_tokenSet_9_data_[] = { 4292870290UL, 4094732927UL, 1431658303UL, 173331797UL, 0UL, 0UL, 0UL, 0UL };
// EOF PCDATA CHTML CBODY ADDRESS HR IMG BFONT BR OH1 CH1 OH2 CH2 OH3 CH3 
// OH4 CH4 OH5 CH5 OH6 CH6 OPARA OULIST OOLIST ODLIST CDTERM OPRE ODIV 
// CDIV OCENTER CCENTER OBQUOTE CBQUOTE OFORM CFORM OTABLE CTABLE O_TR 
// C_TR O_TH_OR_TD C_TH_OR_TD OTTYPE OITALIC OBOLD OUNDER OSTRIKE OBIG 
// OSMALL OSUB OSUP OEM OSTRONG ODFN OCODE OSAMP OKBD OVAR OCITE INPUT 
// OSELECT OTAREA OANCHOR OAPPLET OFONT OMAP 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_9(_tokenSet_9_data_,8);
const unsigned long HTMLParser::_tokenSet_10_data_[] = { 117440528UL, 0UL, 1431654404UL, 173020501UL, 0UL, 0UL, 0UL, 0UL };
// PCDATA IMG BFONT BR OFORM OTTYPE OITALIC OBOLD OUNDER OSTRIKE OBIG OSMALL 
// OSUB OSUP OEM OSTRONG ODFN OCODE OSAMP OKBD OVAR OCITE OANCHOR OAPPLET 
// OFONT OMAP 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_10(_tokenSet_10_data_,8);
const unsigned long HTMLParser::_tokenSet_11_data_[] = { 4292870290UL, 4137089663UL, 1431658303UL, 173331797UL, 0UL, 0UL, 0UL, 0UL };
// EOF PCDATA CHTML CBODY ADDRESS HR IMG BFONT BR OH1 CH1 OH2 CH2 OH3 CH3 
// OH4 CH4 OH5 CH5 OH6 CH6 OPARA OULIST CULIST OOLIST COLIST ODLIST OLITEM 
// CLITEM CDTERM CDIR CMENU OPRE ODIV CDIV OCENTER CCENTER OBQUOTE CBQUOTE 
// OFORM CFORM OTABLE CTABLE O_TR C_TR O_TH_OR_TD C_TH_OR_TD OTTYPE OITALIC 
// OBOLD OUNDER OSTRIKE OBIG OSMALL OSUB OSUP OEM OSTRONG ODFN OCODE OSAMP 
// OKBD OVAR OCITE INPUT OSELECT OTAREA OANCHOR OAPPLET OFONT OMAP 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_11(_tokenSet_11_data_,8);
const unsigned long HTMLParser::_tokenSet_12_data_[] = { 0UL, 42094592UL, 0UL, 0UL, 0UL, 0UL };
// CULIST COLIST OLITEM CDIR CMENU 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_12(_tokenSet_12_data_,6);
const unsigned long HTMLParser::_tokenSet_13_data_[] = { 0UL, 2686976UL, 0UL, 0UL, 0UL, 0UL };
// CDLIST ODTERM ODDEF 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_13(_tokenSet_13_data_,6);
const unsigned long HTMLParser::_tokenSet_14_data_[] = { 2948595728UL, 1409329706UL, 1431654429UL, 173331797UL, 0UL, 0UL, 0UL, 0UL };
// PCDATA ADDRESS HR IMG BFONT BR OH1 OH2 OH3 OH4 OH5 OH6 OPARA OULIST 
// OOLIST ODLIST OPRE ODIV OCENTER OBQUOTE OFORM CFORM OTABLE OTTYPE OITALIC 
// OBOLD OUNDER OSTRIKE OBIG OSMALL OSUB OSUP OEM OSTRONG ODFN OCODE OSAMP 
// OKBD OVAR OCITE INPUT OSELECT OTAREA OANCHOR OAPPLET OFONT OMAP 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_14(_tokenSet_14_data_,8);
const unsigned long HTMLParser::_tokenSet_15_data_[] = { 16UL, 0UL, 256UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// PCDATA O_TR 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_15(_tokenSet_15_data_,8);
const unsigned long HTMLParser::_tokenSet_16_data_[] = { 0UL, 0UL, 288UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// CTABLE O_TR 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_16(_tokenSet_16_data_,8);
const unsigned long HTMLParser::_tokenSet_17_data_[] = { 0UL, 0UL, 1824UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// CTABLE O_TR C_TR O_TH_OR_TD 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_17(_tokenSet_17_data_,8);
const unsigned long HTMLParser::_tokenSet_18_data_[] = { 0UL, 0UL, 0UL, 196608UL, 0UL, 0UL, 0UL, 0UL };
// CSELECT SELOPT 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLParser::_tokenSet_18(_tokenSet_18_data_,8);


