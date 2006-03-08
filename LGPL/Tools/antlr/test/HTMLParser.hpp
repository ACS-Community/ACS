#ifndef INC_HTMLParser_hpp_
#define INC_HTMLParser_hpp_

#include <antlr/config.hpp>
/* $ANTLR 2.7.6 (20060308): "html.g" -> "HTMLParser.hpp"$ */
#include <antlr/TokenStream.hpp>
#include <antlr/TokenBuffer.hpp>
#include "HTMLTokenTypes.hpp"
#include <antlr/LLkParser.hpp>

#line 1 "html.g"

#include <iostream>

#line 16 "HTMLParser.hpp"
class CUSTOM_API HTMLParser : public ANTLR_USE_NAMESPACE(antlr)LLkParser, public HTMLTokenTypes
{
#line 1 "html.g"
#line 20 "HTMLParser.hpp"
public:
	void initializeASTFactory( ANTLR_USE_NAMESPACE(antlr)ASTFactory& factory );
protected:
	HTMLParser(ANTLR_USE_NAMESPACE(antlr)TokenBuffer& tokenBuf, int k);
public:
	HTMLParser(ANTLR_USE_NAMESPACE(antlr)TokenBuffer& tokenBuf);
protected:
	HTMLParser(ANTLR_USE_NAMESPACE(antlr)TokenStream& lexer, int k);
public:
	HTMLParser(ANTLR_USE_NAMESPACE(antlr)TokenStream& lexer);
	HTMLParser(const ANTLR_USE_NAMESPACE(antlr)ParserSharedInputState& state);
	int getNumTokens() const
	{
		return HTMLParser::NUM_TOKENS;
	}
	const char* getTokenName( int type ) const
	{
		if( type > getNumTokens() ) return 0;
		return HTMLParser::tokenNames[type];
	}
	const char* const* getTokenNames() const
	{
		return HTMLParser::tokenNames;
	}
	public: void document();
	public: void head();
	public: void body();
	public: void head_element();
	public: void title();
	public: void script();
	public: void style();
	public: void body_content_no_PCDATA();
	public: void body_content();
	public: void body_tag();
	public: void text_tag();
	public: void heading();
	public: void block();
	public: void text();
	public: void h1();
	public: void h2();
	public: void h3();
	public: void h4();
	public: void h5();
	public: void h6();
	public: void paragraph();
	public: void list();
	public: void preformatted();
	public: void div();
	public: void center();
	public: void blockquote();
	public: void table();
	public: void font();
	public: void teletype();
	public: void italic();
	public: void bold();
	public: void underline();
	public: void strike();
	public: void big();
	public: void small();
	public: void subscript();
	public: void superscript();
	public: void phrase();
	public: void emphasize();
	public: void strong();
	public: void definition();
	public: void code();
	public: void sample_output();
	public: void keyboard_text();
	public: void variable();
	public: void citation();
	public: void special();
	public: void anchor();
	public: void applet();
	public: void font_dfn();
	public: void map();
	public: void form();
	public: void address();
	public: void unordered_list();
	public: void ordered_list();
	public: void def_list();
	public: void list_item();
	public: void def_list_item();
	public: void dt();
	public: void dd();
	public: void dir();
	public: void menu();
	public: void form_field();
	public: void caption();
	public: void tr();
	public: void th_or_td();
	public: void select();
	public: void textarea();
	public: void select_option();
public:
	ANTLR_USE_NAMESPACE(antlr)RefAST getAST()
	{
		return returnAST;
	}
	
protected:
	ANTLR_USE_NAMESPACE(antlr)RefAST returnAST;
private:
	static const char* tokenNames[];
#ifndef NO_STATIC_CONSTS
	static const int NUM_TOKENS = 149;
#else
	enum {
		NUM_TOKENS = 149
	};
#endif
	
	static const unsigned long _tokenSet_0_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_0;
	static const unsigned long _tokenSet_1_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_1;
	static const unsigned long _tokenSet_2_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_2;
	static const unsigned long _tokenSet_3_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_3;
	static const unsigned long _tokenSet_4_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_4;
	static const unsigned long _tokenSet_5_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_5;
	static const unsigned long _tokenSet_6_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_6;
	static const unsigned long _tokenSet_7_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_7;
	static const unsigned long _tokenSet_8_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_8;
	static const unsigned long _tokenSet_9_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_9;
	static const unsigned long _tokenSet_10_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_10;
	static const unsigned long _tokenSet_11_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_11;
	static const unsigned long _tokenSet_12_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_12;
	static const unsigned long _tokenSet_13_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_13;
	static const unsigned long _tokenSet_14_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_14;
	static const unsigned long _tokenSet_15_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_15;
	static const unsigned long _tokenSet_16_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_16;
	static const unsigned long _tokenSet_17_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_17;
	static const unsigned long _tokenSet_18_data_[];
	static const ANTLR_USE_NAMESPACE(antlr)BitSet _tokenSet_18;
};

#endif /*INC_HTMLParser_hpp_*/
