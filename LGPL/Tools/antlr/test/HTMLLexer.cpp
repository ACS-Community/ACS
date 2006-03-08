/* $ANTLR 2.7.6 (20060308): "html.g" -> "HTMLLexer.cpp"$ */
#include "HTMLLexer.hpp"
#include <antlr/CharBuffer.hpp>
#include <antlr/TokenStreamException.hpp>
#include <antlr/TokenStreamIOException.hpp>
#include <antlr/TokenStreamRecognitionException.hpp>
#include <antlr/CharStreamException.hpp>
#include <antlr/CharStreamIOException.hpp>
#include <antlr/NoViableAltForCharException.hpp>

#line 1 "html.g"
#line 13 "HTMLLexer.cpp"
HTMLLexer::HTMLLexer(ANTLR_USE_NAMESPACE(std)istream& in)
	: ANTLR_USE_NAMESPACE(antlr)CharScanner(new ANTLR_USE_NAMESPACE(antlr)CharBuffer(in),false)
{
	initLiterals();
}

HTMLLexer::HTMLLexer(ANTLR_USE_NAMESPACE(antlr)InputBuffer& ib)
	: ANTLR_USE_NAMESPACE(antlr)CharScanner(ib,false)
{
	initLiterals();
}

HTMLLexer::HTMLLexer(const ANTLR_USE_NAMESPACE(antlr)LexerSharedInputState& state)
	: ANTLR_USE_NAMESPACE(antlr)CharScanner(state,false)
{
	initLiterals();
}

void HTMLLexer::initLiterals()
{
}

ANTLR_USE_NAMESPACE(antlr)RefToken HTMLLexer::nextToken()
{
	ANTLR_USE_NAMESPACE(antlr)RefToken theRetToken;
	for (;;) {
		ANTLR_USE_NAMESPACE(antlr)RefToken theRetToken;
		int _ttype = ANTLR_USE_NAMESPACE(antlr)Token::INVALID_TYPE;
		setCommitToPath(false);
		int _m;
		_m = mark();
		resetText();
		try {   // for lexical and char stream error handling
			if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x68 /* 'h' */ ) && (LA(4) == 0x74 /* 't' */ )) {
				mCHTML(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x68 /* 'h' */ ) && (LA(4) == 0x65 /* 'e' */ )) {
				mCHEAD(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x62 /* 'b' */ ) && (LA(4) == 0x6f /* 'o' */ )) {
				mCBODY(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x74 /* 't' */ ) && (LA(4) == 0x69 /* 'i' */ )) {
				mCTITLE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x73 /* 's' */ ) && (LA(4) == 0x63 /* 'c' */ )) {
				mCSCRIPT(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x6d /* 'm' */ ) && (LA(3) == 0x65 /* 'e' */ ) && (LA(4) == 0x74 /* 't' */ )) {
				mMETA(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x6c /* 'l' */ ) && (LA(3) == 0x69 /* 'i' */ ) && (LA(4) == 0x6e /* 'n' */ )) {
				mLINK(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x68 /* 'h' */ ) && (LA(4) == 0x31 /* '1' */ )) {
				mCH1(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x68 /* 'h' */ ) && (LA(4) == 0x32 /* '2' */ )) {
				mCH2(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x68 /* 'h' */ ) && (LA(4) == 0x33 /* '3' */ )) {
				mCH3(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x68 /* 'h' */ ) && (LA(4) == 0x34 /* '4' */ )) {
				mCH4(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x68 /* 'h' */ ) && (LA(4) == 0x35 /* '5' */ )) {
				mCH5(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x68 /* 'h' */ ) && (LA(4) == 0x36 /* '6' */ )) {
				mCH6(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x61 /* 'a' */ ) && (LA(4) == 0x64 /* 'd' */ )) {
				mCADDRESS(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x70 /* 'p' */ ) && (LA(4) == 0x3e /* '>' */ )) {
				mCPARA(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x75 /* 'u' */ ) && (LA(4) == 0x6c /* 'l' */ )) {
				mCULIST(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x6c /* 'l' */ ) && (LA(3) == 0x69 /* 'i' */ ) && (_tokenSet_0.member(LA(4)))) {
				mOLITEM(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x64 /* 'd' */ ) && (LA(4) == 0x6c /* 'l' */ )) {
				mCDLIST(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x64 /* 'd' */ ) && (LA(4) == 0x74 /* 't' */ )) {
				mCDTERM(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x64 /* 'd' */ ) && (LA(4) == 0x64 /* 'd' */ )) {
				mCDDEF(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x64 /* 'd' */ ) && (LA(3) == 0x69 /* 'i' */ ) && (LA(4) == 0x72 /* 'r' */ )) {
				mODIR(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x64 /* 'd' */ ) && (LA(4) == 0x69 /* 'i' */ )) {
				mCDIR_OR_CDIV(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x64 /* 'd' */ ) && (LA(3) == 0x69 /* 'i' */ ) && (LA(4) == 0x76 /* 'v' */ )) {
				mODIV(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x6d /* 'm' */ ) && (LA(3) == 0x65 /* 'e' */ ) && (LA(4) == 0x6e /* 'n' */ )) {
				mOMENU(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x6d /* 'm' */ ) && (LA(4) == 0x65 /* 'e' */ )) {
				mCMENU(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x70 /* 'p' */  || LA(3) == 0x78 /* 'x' */ ) && (LA(4) == 0x6d /* 'm' */  || LA(4) == 0x72 /* 'r' */ )) {
				mCPRE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x63 /* 'c' */ ) && (LA(4) == 0x65 /* 'e' */ )) {
				mCCENTER(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x62 /* 'b' */ ) && (LA(4) == 0x6c /* 'l' */ )) {
				mCBQUOTE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x74 /* 't' */ ) && (LA(4) == 0x61 /* 'a' */ )) {
				mCTABLE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x63 /* 'c' */ ) && (LA(4) == 0x61 /* 'a' */ )) {
				mCCAP(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x74 /* 't' */ ) && (LA(4) == 0x72 /* 'r' */ )) {
				mC_TR(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x74 /* 't' */ ) && (LA(4) == 0x64 /* 'd' */  || LA(4) == 0x68 /* 'h' */ )) {
				mC_TH_OR_TD(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x74 /* 't' */ ) && (LA(4) == 0x74 /* 't' */ )) {
				mCTTYPE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x62 /* 'b' */ ) && (LA(4) == 0x3e /* '>' */ )) {
				mCBOLD(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x75 /* 'u' */ ) && (LA(4) == 0x3e /* '>' */ )) {
				mCUNDER(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x73 /* 's' */ ) && (LA(3) == 0x74 /* 't' */ ) && (LA(4) == 0x72 /* 'r' */ )) {
				mOSTRIKE_OR_OSTRONG(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x73 /* 's' */ ) && (LA(4) == 0x74 /* 't' */ )) {
				mCST_LEFT_FACTORED(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x73 /* 's' */ ) && (LA(3) == 0x74 /* 't' */ ) && (LA(4) == 0x79 /* 'y' */ )) {
				mOSTYLE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x62 /* 'b' */ ) && (LA(4) == 0x69 /* 'i' */ )) {
				mCBIG(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x73 /* 's' */ ) && (LA(4) == 0x6d /* 'm' */ )) {
				mCSMALL(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x73 /* 's' */ ) && (LA(3) == 0x75 /* 'u' */ ) && (LA(4) == 0x62 /* 'b' */ )) {
				mOSUB(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x73 /* 's' */ ) && (LA(3) == 0x75 /* 'u' */ ) && (LA(4) == 0x70 /* 'p' */ )) {
				mOSUP(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x73 /* 's' */ ) && (LA(4) == 0x75 /* 'u' */ )) {
				mCSUB_OR_CSUP(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x64 /* 'd' */ ) && (LA(4) == 0x66 /* 'f' */ )) {
				mCDFN(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x63 /* 'c' */ ) && (LA(4) == 0x6f /* 'o' */ )) {
				mCCODE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x73 /* 's' */ ) && (LA(4) == 0x61 /* 'a' */ )) {
				mCSAMP(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x63 /* 'c' */ ) && (LA(4) == 0x69 /* 'i' */ )) {
				mCCITE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x73 /* 's' */ ) && (LA(4) == 0x65 /* 'e' */ )) {
				mCSELECT(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x74 /* 't' */ ) && (LA(4) == 0x65 /* 'e' */ )) {
				mCTAREA(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x61 /* 'a' */ ) && (LA(4) == 0x3e /* '>' */ )) {
				mCANCHOR(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x61 /* 'a' */ ) && (LA(4) == 0x70 /* 'p' */ )) {
				mCAPPLET(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x66 /* 'f' */ ) && (LA(3) == 0x6f /* 'o' */ ) && (LA(4) == 0x72 /* 'r' */ )) {
				mOFORM(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x66 /* 'f' */ ) && (LA(3) == 0x6f /* 'o' */ ) && (LA(4) == 0x6e /* 'n' */ )) {
				mOFONT(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x6d /* 'm' */ ) && (LA(4) == 0x61 /* 'a' */ )) {
				mCMAP(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x21 /* '!' */ ) && (LA(3) == 0x64 /* 'd' */ )) {
				mDOCTYPE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x68 /* 'h' */ ) && (LA(3) == 0x74 /* 't' */ )) {
				mOHTML(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x68 /* 'h' */ ) && (LA(3) == 0x65 /* 'e' */ )) {
				mOHEAD(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x62 /* 'b' */ ) && (LA(3) == 0x6f /* 'o' */ )) {
				mOBODY(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x74 /* 't' */ ) && (LA(3) == 0x69 /* 'i' */ )) {
				mOTITLE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x73 /* 's' */ ) && (LA(3) == 0x63 /* 'c' */ )) {
				mOSCRIPT(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x69 /* 'i' */ ) && (LA(3) == 0x73 /* 's' */ )) {
				mISINDEX(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x68 /* 'h' */ ) && (LA(3) == 0x31 /* '1' */ )) {
				mOH1(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x68 /* 'h' */ ) && (LA(3) == 0x32 /* '2' */ )) {
				mOH2(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x68 /* 'h' */ ) && (LA(3) == 0x33 /* '3' */ )) {
				mOH3(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x68 /* 'h' */ ) && (LA(3) == 0x34 /* '4' */ )) {
				mOH4(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x68 /* 'h' */ ) && (LA(3) == 0x35 /* '5' */ )) {
				mOH5(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x68 /* 'h' */ ) && (LA(3) == 0x36 /* '6' */ )) {
				mOH6(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x61 /* 'a' */ ) && (LA(3) == 0x64 /* 'd' */ )) {
				mOADDRESS(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x70 /* 'p' */ ) && (_tokenSet_0.member(LA(3)))) {
				mOPARA(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x75 /* 'u' */ ) && (LA(3) == 0x6c /* 'l' */ )) {
				mOULIST(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x6f /* 'o' */ ) && (LA(3) == 0x6c /* 'l' */ )) {
				mOOLIST(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x6f /* 'o' */ )) {
				mCOLIST(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x6c /* 'l' */ )) {
				mCLITEM(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x64 /* 'd' */ ) && (LA(3) == 0x6c /* 'l' */ )) {
				mODLIST(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x64 /* 'd' */ ) && (LA(3) == 0x74 /* 't' */ )) {
				mODTERM(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x64 /* 'd' */ ) && (LA(3) == 0x64 /* 'd' */ )) {
				mODDEF(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x70 /* 'p' */  || LA(2) == 0x78 /* 'x' */ ) && (LA(3) == 0x6d /* 'm' */  || LA(3) == 0x72 /* 'r' */ )) {
				mOPRE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x63 /* 'c' */ ) && (LA(3) == 0x65 /* 'e' */ )) {
				mOCENTER(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x62 /* 'b' */ ) && (LA(3) == 0x6c /* 'l' */ )) {
				mOBQUOTE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x68 /* 'h' */ ) && (LA(3) == 0x72 /* 'r' */ )) {
				mHR(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x74 /* 't' */ ) && (LA(3) == 0x61 /* 'a' */ )) {
				mOTABLE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x63 /* 'c' */ ) && (LA(3) == 0x61 /* 'a' */ )) {
				mOCAP(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x74 /* 't' */ ) && (LA(3) == 0x72 /* 'r' */ )) {
				mO_TR(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x74 /* 't' */ ) && (LA(3) == 0x64 /* 'd' */  || LA(3) == 0x68 /* 'h' */ )) {
				mO_TH_OR_TD(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x74 /* 't' */ ) && (LA(3) == 0x74 /* 't' */ )) {
				mOTTYPE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x69 /* 'i' */ ) && (LA(3) == 0x3e /* '>' */ )) {
				mOITALIC(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x69 /* 'i' */ )) {
				mCITALIC(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x62 /* 'b' */ ) && (LA(3) == 0x3e /* '>' */ )) {
				mOBOLD(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x75 /* 'u' */ ) && (LA(3) == 0x3e /* '>' */ )) {
				mOUNDER(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x62 /* 'b' */ ) && (LA(3) == 0x69 /* 'i' */ )) {
				mOBIG(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x73 /* 's' */ ) && (LA(3) == 0x6d /* 'm' */ )) {
				mOSMALL(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x65 /* 'e' */ )) {
				mCEM(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x64 /* 'd' */ ) && (LA(3) == 0x66 /* 'f' */ )) {
				mODFN(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x63 /* 'c' */ ) && (LA(3) == 0x6f /* 'o' */ )) {
				mOCODE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x73 /* 's' */ ) && (LA(3) == 0x61 /* 'a' */ )) {
				mOSAMP(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x6b /* 'k' */ )) {
				mCKBD(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x76 /* 'v' */ )) {
				mCVAR(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x63 /* 'c' */ ) && (LA(3) == 0x69 /* 'i' */ )) {
				mOCITE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x69 /* 'i' */ ) && (LA(3) == 0x6e /* 'n' */ )) {
				mINPUT(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x73 /* 's' */ ) && (LA(3) == 0x65 /* 'e' */ )) {
				mOSELECT(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x74 /* 't' */ ) && (LA(3) == 0x65 /* 'e' */ )) {
				mOTAREA(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x6f /* 'o' */ ) && (LA(3) == 0x70 /* 'p' */ )) {
				mSELOPT(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x61 /* 'a' */ ) && (_tokenSet_1.member(LA(3)))) {
				mOANCHOR(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x69 /* 'i' */ ) && (LA(3) == 0x6d /* 'm' */ )) {
				mIMG(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x61 /* 'a' */ ) && (LA(3) == 0x70 /* 'p' */ )) {
				mOAPPLET(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x70 /* 'p' */ ) && (LA(3) == 0x61 /* 'a' */ )) {
				mAPARM(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x66 /* 'f' */ )) {
				mCFORM_OR_CFONT(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x62 /* 'b' */ ) && (LA(3) == 0x61 /* 'a' */ )) {
				mBFONT_OR_BASE(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x62 /* 'b' */ ) && (LA(3) == 0x72 /* 'r' */ )) {
				mBR(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x6d /* 'm' */ ) && (LA(3) == 0x61 /* 'a' */ )) {
				mOMAP(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x61 /* 'a' */ ) && (LA(3) == 0x72 /* 'r' */ )) {
				mAREA(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x21 /* '!' */ ) && (LA(3) == 0x2d /* '-' */ )) {
				mCOMMENT(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x65 /* 'e' */ )) {
				mOEM(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x6b /* 'k' */ )) {
				mOKBD(true);
				theRetToken=_returnToken;
			}
			else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x76 /* 'v' */ )) {
				mOVAR(true);
				theRetToken=_returnToken;
			}
			else if ((_tokenSet_2.member(LA(1)))) {
				mPCDATA(true);
				theRetToken=_returnToken;
			}
			else {
				if (LA(1)==EOF_CHAR)
				{
					uponEOF();
					_returnToken = makeToken(ANTLR_USE_NAMESPACE(antlr)Token::EOF_TYPE);
				}
				else {
					commit();
					try {mUNDEFINED_TOKEN(false);}
					catch(ANTLR_USE_NAMESPACE(antlr)RecognitionException& e) {
						// catastrophic failure
						reportError(e);
						consume();
					}
					goto tryAgain;
				}
			}
			
			commit();
			if ( !_returnToken )
				goto tryAgain; // found SKIP token

			_ttype = _returnToken->getType();
			_ttype = testLiteralsTable(_ttype);
			_returnToken->setType(_ttype);
			return _returnToken;
		}
		catch (ANTLR_USE_NAMESPACE(antlr)RecognitionException& e) {
			if ( !getCommitToPath() ) {
				rewind(_m);
				resetText();
				try {mUNDEFINED_TOKEN(false);}
				catch(ANTLR_USE_NAMESPACE(antlr)RecognitionException& ee) {
					// horrendous failure: error in filter rule
					reportError(ee);
					consume();
				}
			}
			else
				throw ANTLR_USE_NAMESPACE(antlr)TokenStreamRecognitionException(e);
		}
		catch (ANTLR_USE_NAMESPACE(antlr)CharStreamIOException& csie) {
			throw ANTLR_USE_NAMESPACE(antlr)TokenStreamIOException(csie.io);
		}
		catch (ANTLR_USE_NAMESPACE(antlr)CharStreamException& cse) {
			throw ANTLR_USE_NAMESPACE(antlr)TokenStreamException(cse.getMessage());
		}
tryAgain:;
	}
}

void HTMLLexer::mDOCTYPE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = DOCTYPE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<!doctype");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		_saveIndex = text.length();
		mWS_(false);
		text.erase(_saveIndex);
		break;
	}
	case 0x68 /* 'h' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match("html");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		_saveIndex = text.length();
		mWS_(false);
		text.erase(_saveIndex);
		break;
	}
	case 0x70 /* 'p' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match("public");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		_saveIndex = text.length();
		mWS_(false);
		text.erase(_saveIndex);
		break;
	}
	case 0x22 /* '\"' */ :
	case 0x27 /* '\'' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	mSTRING(false);
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		_saveIndex = text.length();
		mWS_(false);
		text.erase(_saveIndex);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mWS_(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = WS_;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	{ // ( ... )+
	int _cnt423=0;
	for (;;) {
		switch ( LA(1)) {
		case 0x20 /* ' ' */ :
		{
			match(' ' /* charlit */ );
			break;
		}
		case 0x9 /* '\t' */ :
		{
			match('\t' /* charlit */ );
			break;
		}
		case 0xa /* '\n' */ :
		{
			match('\n' /* charlit */ );
#line 951 "html.g"
			newline();
#line 693 "HTMLLexer.cpp"
			break;
		}
		default:
			if ((LA(1) == 0xd /* '\r' */ ) && (LA(2) == 0xa /* '\n' */ ) && (_tokenSet_3.member(LA(3))) && (true)) {
				match("\r\n");
#line 952 "html.g"
				newline();
#line 701 "HTMLLexer.cpp"
			}
			else if ((LA(1) == 0xd /* '\r' */ ) && (_tokenSet_3.member(LA(2))) && (true) && (true)) {
				match('\r' /* charlit */ );
#line 953 "html.g"
				newline();
#line 707 "HTMLLexer.cpp"
			}
		else {
			if ( _cnt423>=1 ) { goto _loop423; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		}
		_cnt423++;
	}
	_loop423:;
	}  // ( ... )+
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mSTRING(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = STRING;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	switch ( LA(1)) {
	case 0x22 /* '\"' */ :
	{
		match('\"' /* charlit */ );
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_4.member(LA(1)))) {
				matchNot('\"' /* charlit */ );
			}
			else {
				goto _loop443;
			}
			
		}
		_loop443:;
		} // ( ... )*
		match('\"' /* charlit */ );
		break;
	}
	case 0x27 /* '\'' */ :
	{
		match('\'' /* charlit */ );
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_5.member(LA(1)))) {
				matchNot('\'' /* charlit */ );
			}
			else {
				goto _loop445;
			}
			
		}
		_loop445:;
		} // ( ... )*
		match('\'' /* charlit */ );
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOHTML(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OHTML;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<html>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCHTML(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CHTML;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</html>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOHEAD(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OHEAD;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<head>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCHEAD(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CHEAD;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</head>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOBODY(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OBODY;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<body");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_6.member(LA(1)))) {
				mATTR(false);
			}
			else {
				goto _loop236;
			}
			
		}
		_loop236:;
		} // ( ... )*
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mATTR(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = ATTR;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	mWORD(false);
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		_saveIndex = text.length();
		mWS_(false);
		text.erase(_saveIndex);
		break;
	}
	case 0x2e /* '.' */ :
	case 0x3d /* '=' */ :
	case 0x3e /* '>' */ :
	case 0x61 /* 'a' */ :
	case 0x62 /* 'b' */ :
	case 0x63 /* 'c' */ :
	case 0x64 /* 'd' */ :
	case 0x65 /* 'e' */ :
	case 0x66 /* 'f' */ :
	case 0x67 /* 'g' */ :
	case 0x68 /* 'h' */ :
	case 0x69 /* 'i' */ :
	case 0x6a /* 'j' */ :
	case 0x6b /* 'k' */ :
	case 0x6c /* 'l' */ :
	case 0x6d /* 'm' */ :
	case 0x6e /* 'n' */ :
	case 0x6f /* 'o' */ :
	case 0x70 /* 'p' */ :
	case 0x71 /* 'q' */ :
	case 0x72 /* 'r' */ :
	case 0x73 /* 's' */ :
	case 0x74 /* 't' */ :
	case 0x75 /* 'u' */ :
	case 0x76 /* 'v' */ :
	case 0x77 /* 'w' */ :
	case 0x78 /* 'x' */ :
	case 0x79 /* 'y' */ :
	case 0x7a /* 'z' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	{
	switch ( LA(1)) {
	case 0x3d /* '=' */ :
	{
		match('=' /* charlit */ );
		{
		switch ( LA(1)) {
		case 0x9 /* '\t' */ :
		case 0xa /* '\n' */ :
		case 0xd /* '\r' */ :
		case 0x20 /* ' ' */ :
		{
			_saveIndex = text.length();
			mWS_(false);
			text.erase(_saveIndex);
			break;
		}
		case 0x22 /* '\"' */ :
		case 0x23 /* '#' */ :
		case 0x27 /* '\'' */ :
		case 0x2d /* '-' */ :
		case 0x2e /* '.' */ :
		case 0x30 /* '0' */ :
		case 0x31 /* '1' */ :
		case 0x32 /* '2' */ :
		case 0x33 /* '3' */ :
		case 0x34 /* '4' */ :
		case 0x35 /* '5' */ :
		case 0x36 /* '6' */ :
		case 0x37 /* '7' */ :
		case 0x38 /* '8' */ :
		case 0x39 /* '9' */ :
		case 0x61 /* 'a' */ :
		case 0x62 /* 'b' */ :
		case 0x63 /* 'c' */ :
		case 0x64 /* 'd' */ :
		case 0x65 /* 'e' */ :
		case 0x66 /* 'f' */ :
		case 0x67 /* 'g' */ :
		case 0x68 /* 'h' */ :
		case 0x69 /* 'i' */ :
		case 0x6a /* 'j' */ :
		case 0x6b /* 'k' */ :
		case 0x6c /* 'l' */ :
		case 0x6d /* 'm' */ :
		case 0x6e /* 'n' */ :
		case 0x6f /* 'o' */ :
		case 0x70 /* 'p' */ :
		case 0x71 /* 'q' */ :
		case 0x72 /* 'r' */ :
		case 0x73 /* 's' */ :
		case 0x74 /* 't' */ :
		case 0x75 /* 'u' */ :
		case 0x76 /* 'v' */ :
		case 0x77 /* 'w' */ :
		case 0x78 /* 'x' */ :
		case 0x79 /* 'y' */ :
		case 0x7a /* 'z' */ :
		{
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
		}
		}
		}
		{
		switch ( LA(1)) {
		case 0x2e /* '.' */ :
		case 0x61 /* 'a' */ :
		case 0x62 /* 'b' */ :
		case 0x63 /* 'c' */ :
		case 0x64 /* 'd' */ :
		case 0x65 /* 'e' */ :
		case 0x66 /* 'f' */ :
		case 0x67 /* 'g' */ :
		case 0x68 /* 'h' */ :
		case 0x69 /* 'i' */ :
		case 0x6a /* 'j' */ :
		case 0x6b /* 'k' */ :
		case 0x6c /* 'l' */ :
		case 0x6d /* 'm' */ :
		case 0x6e /* 'n' */ :
		case 0x6f /* 'o' */ :
		case 0x70 /* 'p' */ :
		case 0x71 /* 'q' */ :
		case 0x72 /* 'r' */ :
		case 0x73 /* 's' */ :
		case 0x74 /* 't' */ :
		case 0x75 /* 'u' */ :
		case 0x76 /* 'v' */ :
		case 0x77 /* 'w' */ :
		case 0x78 /* 'x' */ :
		case 0x79 /* 'y' */ :
		case 0x7a /* 'z' */ :
		{
			mWORD(false);
			{
			switch ( LA(1)) {
			case 0x9 /* '\t' */ :
			case 0xa /* '\n' */ :
			case 0xd /* '\r' */ :
			case 0x20 /* ' ' */ :
			{
				_saveIndex = text.length();
				mWS_(false);
				text.erase(_saveIndex);
				break;
			}
			case 0x25 /* '%' */ :
			case 0x2e /* '.' */ :
			case 0x3e /* '>' */ :
			case 0x61 /* 'a' */ :
			case 0x62 /* 'b' */ :
			case 0x63 /* 'c' */ :
			case 0x64 /* 'd' */ :
			case 0x65 /* 'e' */ :
			case 0x66 /* 'f' */ :
			case 0x67 /* 'g' */ :
			case 0x68 /* 'h' */ :
			case 0x69 /* 'i' */ :
			case 0x6a /* 'j' */ :
			case 0x6b /* 'k' */ :
			case 0x6c /* 'l' */ :
			case 0x6d /* 'm' */ :
			case 0x6e /* 'n' */ :
			case 0x6f /* 'o' */ :
			case 0x70 /* 'p' */ :
			case 0x71 /* 'q' */ :
			case 0x72 /* 'r' */ :
			case 0x73 /* 's' */ :
			case 0x74 /* 't' */ :
			case 0x75 /* 'u' */ :
			case 0x76 /* 'v' */ :
			case 0x77 /* 'w' */ :
			case 0x78 /* 'x' */ :
			case 0x79 /* 'y' */ :
			case 0x7a /* 'z' */ :
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
			}
			}
			}
			{
			switch ( LA(1)) {
			case 0x25 /* '%' */ :
			{
				match('%' /* charlit */ );
				{
				switch ( LA(1)) {
				case 0x9 /* '\t' */ :
				case 0xa /* '\n' */ :
				case 0xd /* '\r' */ :
				case 0x20 /* ' ' */ :
				{
					_saveIndex = text.length();
					mWS_(false);
					text.erase(_saveIndex);
					break;
				}
				case 0x2e /* '.' */ :
				case 0x3e /* '>' */ :
				case 0x61 /* 'a' */ :
				case 0x62 /* 'b' */ :
				case 0x63 /* 'c' */ :
				case 0x64 /* 'd' */ :
				case 0x65 /* 'e' */ :
				case 0x66 /* 'f' */ :
				case 0x67 /* 'g' */ :
				case 0x68 /* 'h' */ :
				case 0x69 /* 'i' */ :
				case 0x6a /* 'j' */ :
				case 0x6b /* 'k' */ :
				case 0x6c /* 'l' */ :
				case 0x6d /* 'm' */ :
				case 0x6e /* 'n' */ :
				case 0x6f /* 'o' */ :
				case 0x70 /* 'p' */ :
				case 0x71 /* 'q' */ :
				case 0x72 /* 'r' */ :
				case 0x73 /* 's' */ :
				case 0x74 /* 't' */ :
				case 0x75 /* 'u' */ :
				case 0x76 /* 'v' */ :
				case 0x77 /* 'w' */ :
				case 0x78 /* 'x' */ :
				case 0x79 /* 'y' */ :
				case 0x7a /* 'z' */ :
				{
					break;
				}
				default:
				{
					throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
				}
				}
				}
				break;
			}
			case 0x2e /* '.' */ :
			case 0x3e /* '>' */ :
			case 0x61 /* 'a' */ :
			case 0x62 /* 'b' */ :
			case 0x63 /* 'c' */ :
			case 0x64 /* 'd' */ :
			case 0x65 /* 'e' */ :
			case 0x66 /* 'f' */ :
			case 0x67 /* 'g' */ :
			case 0x68 /* 'h' */ :
			case 0x69 /* 'i' */ :
			case 0x6a /* 'j' */ :
			case 0x6b /* 'k' */ :
			case 0x6c /* 'l' */ :
			case 0x6d /* 'm' */ :
			case 0x6e /* 'n' */ :
			case 0x6f /* 'o' */ :
			case 0x70 /* 'p' */ :
			case 0x71 /* 'q' */ :
			case 0x72 /* 'r' */ :
			case 0x73 /* 's' */ :
			case 0x74 /* 't' */ :
			case 0x75 /* 'u' */ :
			case 0x76 /* 'v' */ :
			case 0x77 /* 'w' */ :
			case 0x78 /* 'x' */ :
			case 0x79 /* 'y' */ :
			case 0x7a /* 'z' */ :
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
			}
			}
			}
			break;
		}
		case 0x2d /* '-' */ :
		case 0x30 /* '0' */ :
		case 0x31 /* '1' */ :
		case 0x32 /* '2' */ :
		case 0x33 /* '3' */ :
		case 0x34 /* '4' */ :
		case 0x35 /* '5' */ :
		case 0x36 /* '6' */ :
		case 0x37 /* '7' */ :
		case 0x38 /* '8' */ :
		case 0x39 /* '9' */ :
		{
			{
			switch ( LA(1)) {
			case 0x2d /* '-' */ :
			{
				match('-' /* charlit */ );
				{
				switch ( LA(1)) {
				case 0x9 /* '\t' */ :
				case 0xa /* '\n' */ :
				case 0xd /* '\r' */ :
				case 0x20 /* ' ' */ :
				{
					_saveIndex = text.length();
					mWS_(false);
					text.erase(_saveIndex);
					break;
				}
				case 0x30 /* '0' */ :
				case 0x31 /* '1' */ :
				case 0x32 /* '2' */ :
				case 0x33 /* '3' */ :
				case 0x34 /* '4' */ :
				case 0x35 /* '5' */ :
				case 0x36 /* '6' */ :
				case 0x37 /* '7' */ :
				case 0x38 /* '8' */ :
				case 0x39 /* '9' */ :
				{
					break;
				}
				default:
				{
					throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
				}
				}
				}
				break;
			}
			case 0x30 /* '0' */ :
			case 0x31 /* '1' */ :
			case 0x32 /* '2' */ :
			case 0x33 /* '3' */ :
			case 0x34 /* '4' */ :
			case 0x35 /* '5' */ :
			case 0x36 /* '6' */ :
			case 0x37 /* '7' */ :
			case 0x38 /* '8' */ :
			case 0x39 /* '9' */ :
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
			}
			}
			}
			mINT(false);
			{
			switch ( LA(1)) {
			case 0x9 /* '\t' */ :
			case 0xa /* '\n' */ :
			case 0xd /* '\r' */ :
			case 0x20 /* ' ' */ :
			{
				_saveIndex = text.length();
				mWS_(false);
				text.erase(_saveIndex);
				break;
			}
			case 0x2e /* '.' */ :
			case 0x3e /* '>' */ :
			case 0x61 /* 'a' */ :
			case 0x62 /* 'b' */ :
			case 0x63 /* 'c' */ :
			case 0x64 /* 'd' */ :
			case 0x65 /* 'e' */ :
			case 0x66 /* 'f' */ :
			case 0x67 /* 'g' */ :
			case 0x68 /* 'h' */ :
			case 0x69 /* 'i' */ :
			case 0x6a /* 'j' */ :
			case 0x6b /* 'k' */ :
			case 0x6c /* 'l' */ :
			case 0x6d /* 'm' */ :
			case 0x6e /* 'n' */ :
			case 0x6f /* 'o' */ :
			case 0x70 /* 'p' */ :
			case 0x71 /* 'q' */ :
			case 0x72 /* 'r' */ :
			case 0x73 /* 's' */ :
			case 0x74 /* 't' */ :
			case 0x75 /* 'u' */ :
			case 0x76 /* 'v' */ :
			case 0x77 /* 'w' */ :
			case 0x78 /* 'x' */ :
			case 0x79 /* 'y' */ :
			case 0x7a /* 'z' */ :
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
			}
			}
			}
			break;
		}
		case 0x22 /* '\"' */ :
		case 0x27 /* '\'' */ :
		{
			mSTRING(false);
			{
			switch ( LA(1)) {
			case 0x9 /* '\t' */ :
			case 0xa /* '\n' */ :
			case 0xd /* '\r' */ :
			case 0x20 /* ' ' */ :
			{
				_saveIndex = text.length();
				mWS_(false);
				text.erase(_saveIndex);
				break;
			}
			case 0x2e /* '.' */ :
			case 0x3e /* '>' */ :
			case 0x61 /* 'a' */ :
			case 0x62 /* 'b' */ :
			case 0x63 /* 'c' */ :
			case 0x64 /* 'd' */ :
			case 0x65 /* 'e' */ :
			case 0x66 /* 'f' */ :
			case 0x67 /* 'g' */ :
			case 0x68 /* 'h' */ :
			case 0x69 /* 'i' */ :
			case 0x6a /* 'j' */ :
			case 0x6b /* 'k' */ :
			case 0x6c /* 'l' */ :
			case 0x6d /* 'm' */ :
			case 0x6e /* 'n' */ :
			case 0x6f /* 'o' */ :
			case 0x70 /* 'p' */ :
			case 0x71 /* 'q' */ :
			case 0x72 /* 'r' */ :
			case 0x73 /* 's' */ :
			case 0x74 /* 't' */ :
			case 0x75 /* 'u' */ :
			case 0x76 /* 'v' */ :
			case 0x77 /* 'w' */ :
			case 0x78 /* 'x' */ :
			case 0x79 /* 'y' */ :
			case 0x7a /* 'z' */ :
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
			}
			}
			}
			break;
		}
		case 0x23 /* '#' */ :
		{
			mHEXNUM(false);
			{
			switch ( LA(1)) {
			case 0x9 /* '\t' */ :
			case 0xa /* '\n' */ :
			case 0xd /* '\r' */ :
			case 0x20 /* ' ' */ :
			{
				_saveIndex = text.length();
				mWS_(false);
				text.erase(_saveIndex);
				break;
			}
			case 0x2e /* '.' */ :
			case 0x3e /* '>' */ :
			case 0x61 /* 'a' */ :
			case 0x62 /* 'b' */ :
			case 0x63 /* 'c' */ :
			case 0x64 /* 'd' */ :
			case 0x65 /* 'e' */ :
			case 0x66 /* 'f' */ :
			case 0x67 /* 'g' */ :
			case 0x68 /* 'h' */ :
			case 0x69 /* 'i' */ :
			case 0x6a /* 'j' */ :
			case 0x6b /* 'k' */ :
			case 0x6c /* 'l' */ :
			case 0x6d /* 'm' */ :
			case 0x6e /* 'n' */ :
			case 0x6f /* 'o' */ :
			case 0x70 /* 'p' */ :
			case 0x71 /* 'q' */ :
			case 0x72 /* 'r' */ :
			case 0x73 /* 's' */ :
			case 0x74 /* 't' */ :
			case 0x75 /* 'u' */ :
			case 0x76 /* 'v' */ :
			case 0x77 /* 'w' */ :
			case 0x78 /* 'x' */ :
			case 0x79 /* 'y' */ :
			case 0x7a /* 'z' */ :
			{
				break;
			}
			default:
			{
				throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
			}
			}
			}
			break;
		}
		default:
		{
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
		}
		}
		}
		break;
	}
	case 0x2e /* '.' */ :
	case 0x3e /* '>' */ :
	case 0x61 /* 'a' */ :
	case 0x62 /* 'b' */ :
	case 0x63 /* 'c' */ :
	case 0x64 /* 'd' */ :
	case 0x65 /* 'e' */ :
	case 0x66 /* 'f' */ :
	case 0x67 /* 'g' */ :
	case 0x68 /* 'h' */ :
	case 0x69 /* 'i' */ :
	case 0x6a /* 'j' */ :
	case 0x6b /* 'k' */ :
	case 0x6c /* 'l' */ :
	case 0x6d /* 'm' */ :
	case 0x6e /* 'n' */ :
	case 0x6f /* 'o' */ :
	case 0x70 /* 'p' */ :
	case 0x71 /* 'q' */ :
	case 0x72 /* 'r' */ :
	case 0x73 /* 's' */ :
	case 0x74 /* 't' */ :
	case 0x75 /* 'u' */ :
	case 0x76 /* 'v' */ :
	case 0x77 /* 'w' */ :
	case 0x78 /* 'x' */ :
	case 0x79 /* 'y' */ :
	case 0x7a /* 'z' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCBODY(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CBODY;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</body>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOTITLE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OTITLE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<title>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCTITLE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CTITLE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</title>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOSCRIPT(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OSCRIPT;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<script>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCSCRIPT(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CSCRIPT;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</script>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mISINDEX(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = ISINDEX;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<isindex");
	mWS_(false);
	mATTR(false);
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mMETA(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = META;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<meta");
	mWS_(false);
	{ // ( ... )+
	int _cnt245=0;
	for (;;) {
		if ((_tokenSet_6.member(LA(1)))) {
			mATTR(false);
		}
		else {
			if ( _cnt245>=1 ) { goto _loop245; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		
		_cnt245++;
	}
	_loop245:;
	}  // ( ... )+
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mLINK(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = LINK;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<link");
	mWS_(false);
	{ // ( ... )+
	int _cnt248=0;
	for (;;) {
		if ((_tokenSet_6.member(LA(1)))) {
			mATTR(false);
		}
		else {
			if ( _cnt248>=1 ) { goto _loop248; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		
		_cnt248++;
	}
	_loop248:;
	}  // ( ... )+
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOH1(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OH1;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<h1");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCH1(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CH1;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</h1>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOH2(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OH2;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<h2");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCH2(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CH2;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</h2>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOH3(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OH3;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<h3");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCH3(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CH3;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</h3>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOH4(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OH4;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<h4");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCH4(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CH4;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</h4>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOH5(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OH5;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<h5");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCH5(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CH5;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</h5>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOH6(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OH6;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<h6");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCH6(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CH6;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</h6>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOADDRESS(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OADDRESS;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<address>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCADDRESS(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CADDRESS;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</address>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOPARA(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OPARA;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<p");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCPARA(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CPARA;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</p>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOULIST(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OULIST;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<ul");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCULIST(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CULIST;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</ul>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOOLIST(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OOLIST;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<ol");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCOLIST(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = COLIST;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</ol>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOLITEM(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OLITEM;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<li");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCLITEM(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CLITEM;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</li>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mODLIST(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = ODLIST;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<dl");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCDLIST(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CDLIST;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</dl>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mODTERM(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = ODTERM;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<dt>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCDTERM(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CDTERM;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</dt>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mODDEF(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = ODDEF;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<dd>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCDDEF(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CDDEF;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</dd>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mODIR(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = ODIR;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<dir>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCDIR_OR_CDIV(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CDIR_OR_CDIV;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</di");
	{
	switch ( LA(1)) {
	case 0x72 /* 'r' */ :
	{
		match('r' /* charlit */ );
#line 569 "html.g"
		_ttype = CDIR;
#line 2273 "HTMLLexer.cpp"
		break;
	}
	case 0x76 /* 'v' */ :
	{
		match('v' /* charlit */ );
#line 570 "html.g"
		_ttype = CDIV;
#line 2281 "HTMLLexer.cpp"
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mODIV(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = ODIV;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<div");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOMENU(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OMENU;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<menu>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCMENU(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CMENU;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</menu>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOPRE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OPRE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	{
	if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x70 /* 'p' */ )) {
		match("<pre>");
	}
	else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x78 /* 'x' */ )) {
		match("<xmp>");
	}
	else {
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	
	}
	{
	if ((LA(1) == 0xa /* '\n' */ )) {
		match('\n' /* charlit */ );
	}
	else {
	}
	
	}
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCPRE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CPRE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x70 /* 'p' */ )) {
		match("</pre>");
	}
	else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x78 /* 'x' */ )) {
		match("</xmp>");
	}
	else {
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOCENTER(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OCENTER;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<center>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCCENTER(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CCENTER;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</center>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOBQUOTE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OBQUOTE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<blockquote>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCBQUOTE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CBQUOTE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</blockquote>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mHR(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = HR;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<hr");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_6.member(LA(1)))) {
				mATTR(false);
			}
			else {
				goto _loop306;
			}
			
		}
		_loop306:;
		} // ( ... )*
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOTABLE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OTABLE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<table");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_6.member(LA(1)))) {
				mATTR(false);
			}
			else {
				goto _loop310;
			}
			
		}
		_loop310:;
		} // ( ... )*
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCTABLE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CTABLE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</table>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOCAP(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OCAP;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<caption");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_6.member(LA(1)))) {
				mATTR(false);
			}
			else {
				goto _loop315;
			}
			
		}
		_loop315:;
		} // ( ... )*
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCCAP(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CCAP;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</caption>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mO_TR(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = O_TR;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<tr");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_6.member(LA(1)))) {
				mATTR(false);
			}
			else {
				goto _loop320;
			}
			
		}
		_loop320:;
		} // ( ... )*
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mC_TR(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = C_TR;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</tr>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mO_TH_OR_TD(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = O_TH_OR_TD;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	{
	if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x74 /* 't' */ ) && (LA(3) == 0x68 /* 'h' */ )) {
		match("<th");
	}
	else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x74 /* 't' */ ) && (LA(3) == 0x64 /* 'd' */ )) {
		match("<td");
	}
	else {
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	
	}
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_6.member(LA(1)))) {
				mATTR(false);
			}
			else {
				goto _loop326;
			}
			
		}
		_loop326:;
		} // ( ... )*
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mC_TH_OR_TD(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = C_TH_OR_TD;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x74 /* 't' */ ) && (LA(4) == 0x68 /* 'h' */ )) {
		match("</th>");
	}
	else if ((LA(1) == 0x3c /* '<' */ ) && (LA(2) == 0x2f /* '/' */ ) && (LA(3) == 0x74 /* 't' */ ) && (LA(4) == 0x64 /* 'd' */ )) {
		match("</td>");
	}
	else {
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOTTYPE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OTTYPE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<tt>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCTTYPE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CTTYPE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</tt>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOITALIC(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OITALIC;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<i>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCITALIC(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CITALIC;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</i>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOBOLD(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OBOLD;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<b>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCBOLD(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CBOLD;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</b>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOUNDER(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OUNDER;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<u>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCUNDER(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CUNDER;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</u>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

/** Left-factor <strike> and <strong> to reduce lookahead */
void HTMLLexer::mOSTRIKE_OR_OSTRONG(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OSTRIKE_OR_OSTRONG;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<str");
	{
	switch ( LA(1)) {
	case 0x69 /* 'i' */ :
	{
		match("ike");
#line 685 "html.g"
		_ttype = OSTRIKE;
#line 2912 "HTMLLexer.cpp"
		break;
	}
	case 0x6f /* 'o' */ :
	{
		match("ong");
#line 686 "html.g"
		_ttype = OSTRONG;
#line 2920 "HTMLLexer.cpp"
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCST_LEFT_FACTORED(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CST_LEFT_FACTORED;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</st");
	{
	if ((LA(1) == 0x72 /* 'r' */ ) && (LA(2) == 0x69 /* 'i' */ )) {
		match("rike");
#line 693 "html.g"
		_ttype = CSTRIKE;
#line 2949 "HTMLLexer.cpp"
	}
	else if ((LA(1) == 0x72 /* 'r' */ ) && (LA(2) == 0x6f /* 'o' */ )) {
		match("rong");
#line 694 "html.g"
		_ttype = CSTRONG;
#line 2955 "HTMLLexer.cpp"
	}
	else if ((LA(1) == 0x79 /* 'y' */ )) {
		match("yle");
#line 695 "html.g"
		_ttype = CSTYLE;
#line 2961 "HTMLLexer.cpp"
	}
	else {
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOSTYLE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OSTYLE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<style>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOBIG(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OBIG;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<big>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCBIG(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CBIG;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</big>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOSMALL(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OSMALL;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<small>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCSMALL(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CSMALL;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</small>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOSUB(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OSUB;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<sub>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOSUP(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OSUP;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<sup>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCSUB_OR_CSUP(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CSUB_OR_CSUP;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</su");
	{
	switch ( LA(1)) {
	case 0x62 /* 'b' */ :
	{
		match('b' /* charlit */ );
#line 726 "html.g"
		_ttype = CSUB;
#line 3088 "HTMLLexer.cpp"
		break;
	}
	case 0x70 /* 'p' */ :
	{
		match('p' /* charlit */ );
#line 727 "html.g"
		_ttype = CSUP;
#line 3096 "HTMLLexer.cpp"
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOEM(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OEM;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<em>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCEM(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CEM;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</em>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mODFN(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = ODFN;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<dfn>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCDFN(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CDFN;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</dfn>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOCODE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OCODE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<code>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCCODE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CCODE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</code>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOSAMP(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OSAMP;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<samp>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCSAMP(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CSAMP;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</samp>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOKBD(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OKBD;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<kbd>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCKBD(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CKBD;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</kbd>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOVAR(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OVAR;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<var>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCVAR(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CVAR;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</var>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOCITE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OCITE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<cite>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCCITE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CCITE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</cite>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mINPUT(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = INPUT;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<input");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_6.member(LA(1)))) {
				mATTR(false);
			}
			else {
				goto _loop366;
			}
			
		}
		_loop366:;
		} // ( ... )*
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOSELECT(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OSELECT;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<select");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_6.member(LA(1)))) {
				mATTR(false);
			}
			else {
				goto _loop370;
			}
			
		}
		_loop370:;
		} // ( ... )*
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCSELECT(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CSELECT;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</select>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOTAREA(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OTAREA;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<textarea");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_6.member(LA(1)))) {
				mATTR(false);
			}
			else {
				goto _loop375;
			}
			
		}
		_loop375:;
		} // ( ... )*
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCTAREA(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CTAREA;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</textarea>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mSELOPT(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = SELOPT;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<option");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_6.member(LA(1)))) {
				mATTR(false);
			}
			else {
				goto _loop380;
			}
			
		}
		_loop380:;
		} // ( ... )*
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOANCHOR(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OANCHOR;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<a");
	mWS_(false);
	{ // ( ... )+
	int _cnt383=0;
	for (;;) {
		if ((_tokenSet_6.member(LA(1)))) {
			mATTR(false);
		}
		else {
			if ( _cnt383>=1 ) { goto _loop383; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		
		_cnt383++;
	}
	_loop383:;
	}  // ( ... )+
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCANCHOR(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CANCHOR;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</a>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mIMG(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = IMG;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<img");
	mWS_(false);
	{ // ( ... )+
	int _cnt387=0;
	for (;;) {
		if ((_tokenSet_6.member(LA(1)))) {
			mATTR(false);
		}
		else {
			if ( _cnt387>=1 ) { goto _loop387; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		
		_cnt387++;
	}
	_loop387:;
	}  // ( ... )+
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOAPPLET(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OAPPLET;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<applet");
	mWS_(false);
	{ // ( ... )+
	int _cnt390=0;
	for (;;) {
		if ((_tokenSet_6.member(LA(1)))) {
			mATTR(false);
		}
		else {
			if ( _cnt390>=1 ) { goto _loop390; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		
		_cnt390++;
	}
	_loop390:;
	}  // ( ... )+
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCAPPLET(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CAPPLET;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</applet>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mAPARM(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = APARM;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<param");
	mWS_(false);
	{ // ( ... )+
	int _cnt394=0;
	for (;;) {
		if ((_tokenSet_6.member(LA(1)))) {
			mATTR(false);
		}
		else {
			if ( _cnt394>=1 ) { goto _loop394; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		
		_cnt394++;
	}
	_loop394:;
	}  // ( ... )+
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOFORM(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OFORM;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<form");
	mWS_(false);
	{ // ( ... )+
	int _cnt397=0;
	for (;;) {
		if ((_tokenSet_6.member(LA(1)))) {
			mATTR(false);
		}
		else {
			if ( _cnt397>=1 ) { goto _loop397; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		
		_cnt397++;
	}
	_loop397:;
	}  // ( ... )+
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOFONT(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OFONT;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<font");
	mWS_(false);
	{ // ( ... )+
	int _cnt400=0;
	for (;;) {
		if ((_tokenSet_6.member(LA(1)))) {
			mATTR(false);
		}
		else {
			if ( _cnt400>=1 ) { goto _loop400; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		
		_cnt400++;
	}
	_loop400:;
	}  // ( ... )+
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCFORM_OR_CFONT(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CFORM_OR_CFONT;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</fo");
	{
	switch ( LA(1)) {
	case 0x72 /* 'r' */ :
	{
		match("rm");
#line 842 "html.g"
		_ttype = CFORM;
#line 3747 "HTMLLexer.cpp"
		break;
	}
	case 0x6e /* 'n' */ :
	{
		match("nt");
#line 843 "html.g"
		_ttype = CFONT;
#line 3755 "HTMLLexer.cpp"
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mBFONT_OR_BASE(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = BFONT_OR_BASE;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<base");
	{
	switch ( LA(1)) {
	case 0x66 /* 'f' */ :
	{
		match("font");
		mWS_(false);
		mATTR(false);
#line 860 "html.g"
		_ttype = BFONT;
#line 3788 "HTMLLexer.cpp"
		break;
	}
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
#line 861 "html.g"
		_ttype = BASE;
#line 3800 "HTMLLexer.cpp"
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mBR(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = BR;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<br");
	{
	switch ( LA(1)) {
	case 0x9 /* '\t' */ :
	case 0xa /* '\n' */ :
	case 0xd /* '\r' */ :
	case 0x20 /* ' ' */ :
	{
		mWS_(false);
		mATTR(false);
		break;
	}
	case 0x3e /* '>' */ :
	{
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mOMAP(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = OMAP;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<map");
	mWS_(false);
	mATTR(false);
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCMAP(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = CMAP;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("</map>");
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mAREA(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = AREA;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<area");
	mWS_(false);
	{ // ( ... )+
	int _cnt411=0;
	for (;;) {
		if ((_tokenSet_6.member(LA(1)))) {
			mATTR(false);
		}
		else {
			if ( _cnt411>=1 ) { goto _loop411; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		
		_cnt411++;
	}
	_loop411:;
	}  // ( ... )+
	match('>' /* charlit */ );
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mPCDATA(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = PCDATA;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	{ // ( ... )+
	int _cnt415=0;
	for (;;) {
		if ((LA(1) == 0xd /* '\r' */ ) && (LA(2) == 0xa /* '\n' */ ) && (true) && (true)) {
			match('\r' /* charlit */ );
			match('\n' /* charlit */ );
#line 899 "html.g"
			newline();
#line 3928 "HTMLLexer.cpp"
		}
		else if ((LA(1) == 0xd /* '\r' */ ) && (true) && (true) && (true)) {
			match('\r' /* charlit */ );
#line 900 "html.g"
			newline();
#line 3934 "HTMLLexer.cpp"
		}
		else if ((LA(1) == 0xa /* '\n' */ )) {
			match('\n' /* charlit */ );
#line 901 "html.g"
			newline();
#line 3940 "HTMLLexer.cpp"
		}
		else if ((_tokenSet_7.member(LA(1)))) {
			{
			match(_tokenSet_7);
			}
		}
		else {
			if ( _cnt415>=1 ) { goto _loop415; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		
		_cnt415++;
	}
	_loop415:;
	}  // ( ... )+
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCOMMENT_DATA(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = COMMENT_DATA;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	{ // ( ... )*
	for (;;) {
		if (((LA(1) == 0x2d /* '-' */ ) && ((LA(2) >= 0x3 /* '\3' */  && LA(2) <= 0xff)) && ((LA(3) >= 0x3 /* '\3' */  && LA(3) <= 0xff)) && ((LA(4) >= 0x3 /* '\3' */  && LA(4) <= 0xff)))&&(LA(2)!='-' && LA(3)!='>')) {
			match('-' /* charlit */ );
		}
		else if ((LA(1) == 0xd /* '\r' */ ) && (LA(2) == 0xa /* '\n' */ ) && ((LA(3) >= 0x3 /* '\3' */  && LA(3) <= 0xff)) && ((LA(4) >= 0x3 /* '\3' */  && LA(4) <= 0xff))) {
			match('\r' /* charlit */ );
			match('\n' /* charlit */ );
#line 921 "html.g"
			newline();
#line 3978 "HTMLLexer.cpp"
		}
		else if ((LA(1) == 0xd /* '\r' */ ) && ((LA(2) >= 0x3 /* '\3' */  && LA(2) <= 0xff)) && ((LA(3) >= 0x3 /* '\3' */  && LA(3) <= 0xff)) && ((LA(4) >= 0x3 /* '\3' */  && LA(4) <= 0xff))) {
			match('\r' /* charlit */ );
#line 922 "html.g"
			newline();
#line 3984 "HTMLLexer.cpp"
		}
		else if ((LA(1) == 0xa /* '\n' */ )) {
			match('\n' /* charlit */ );
#line 923 "html.g"
			newline();
#line 3990 "HTMLLexer.cpp"
		}
		else if ((_tokenSet_8.member(LA(1)))) {
			{
			match(_tokenSet_8);
			}
		}
		else {
			goto _loop419;
		}
		
	}
	_loop419:;
	} // ( ... )*
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mCOMMENT(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = COMMENT;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match("<!--");
	mCOMMENT_DATA(false);
	match("-->");
#line 930 "html.g"
	_ttype = ANTLR_USE_NAMESPACE(antlr)Token::SKIP;
#line 4022 "HTMLLexer.cpp"
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mWORD(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = WORD;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	{
	switch ( LA(1)) {
	case 0x61 /* 'a' */ :
	case 0x62 /* 'b' */ :
	case 0x63 /* 'c' */ :
	case 0x64 /* 'd' */ :
	case 0x65 /* 'e' */ :
	case 0x66 /* 'f' */ :
	case 0x67 /* 'g' */ :
	case 0x68 /* 'h' */ :
	case 0x69 /* 'i' */ :
	case 0x6a /* 'j' */ :
	case 0x6b /* 'k' */ :
	case 0x6c /* 'l' */ :
	case 0x6d /* 'm' */ :
	case 0x6e /* 'n' */ :
	case 0x6f /* 'o' */ :
	case 0x70 /* 'p' */ :
	case 0x71 /* 'q' */ :
	case 0x72 /* 'r' */ :
	case 0x73 /* 's' */ :
	case 0x74 /* 't' */ :
	case 0x75 /* 'u' */ :
	case 0x76 /* 'v' */ :
	case 0x77 /* 'w' */ :
	case 0x78 /* 'x' */ :
	case 0x79 /* 'y' */ :
	case 0x7a /* 'z' */ :
	{
		mLCLETTER(false);
		break;
	}
	case 0x2e /* '.' */ :
	{
		match('.' /* charlit */ );
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	}
	{ // ( ... )+
	int _cnt440=0;
	for (;;) {
		if (((LA(1) >= 0x61 /* 'a' */  && LA(1) <= 0x7a /* 'z' */ )) && (_tokenSet_9.member(LA(2))) && (true) && (true)) {
			mLCLETTER(false);
		}
		else if ((LA(1) == 0x2e /* '.' */ ) && (_tokenSet_9.member(LA(2))) && (true) && (true)) {
			match('.' /* charlit */ );
		}
		else if (((LA(1) >= 0x30 /* '0' */  && LA(1) <= 0x39 /* '9' */ ))) {
			mDIGIT(false);
		}
		else {
			if ( _cnt440>=1 ) { goto _loop440; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		
		_cnt440++;
	}
	_loop440:;
	}  // ( ... )+
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mINT(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = INT;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	{ // ( ... )+
	int _cnt451=0;
	for (;;) {
		if (((LA(1) >= 0x30 /* '0' */  && LA(1) <= 0x39 /* '9' */ ))) {
			mDIGIT(false);
		}
		else {
			if ( _cnt451>=1 ) { goto _loop451; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		
		_cnt451++;
	}
	_loop451:;
	}  // ( ... )+
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mHEXNUM(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = HEXNUM;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	match('#' /* charlit */ );
	mHEXINT(false);
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mLCLETTER(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = LCLETTER;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	matchRange('a','z');
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mDIGIT(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = DIGIT;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	matchRange('0','9');
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mWSCHARS(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = WSCHARS;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	switch ( LA(1)) {
	case 0x20 /* ' ' */ :
	{
		match(' ' /* charlit */ );
		break;
	}
	case 0x9 /* '\t' */ :
	{
		match('\t' /* charlit */ );
		break;
	}
	case 0xa /* '\n' */ :
	{
		match('\n' /* charlit */ );
		break;
	}
	case 0xd /* '\r' */ :
	{
		match('\r' /* charlit */ );
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mSPECIAL(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = SPECIAL;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	switch ( LA(1)) {
	case 0x3c /* '<' */ :
	{
		match('<' /* charlit */ );
		break;
	}
	case 0x7e /* '~' */ :
	{
		match('~' /* charlit */ );
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mHEXINT(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = HEXINT;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	{ // ( ... )+
	int _cnt454=0;
	for (;;) {
		if ((_tokenSet_10.member(LA(1))) && (_tokenSet_11.member(LA(2))) && (true) && (true)) {
			mHEXDIGIT(false);
		}
		else {
			if ( _cnt454>=1 ) { goto _loop454; } else {throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());}
		}
		
		_cnt454++;
	}
	_loop454:;
	}  // ( ... )+
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mHEXDIGIT(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = HEXDIGIT;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	switch ( LA(1)) {
	case 0x30 /* '0' */ :
	case 0x31 /* '1' */ :
	case 0x32 /* '2' */ :
	case 0x33 /* '3' */ :
	case 0x34 /* '4' */ :
	case 0x35 /* '5' */ :
	case 0x36 /* '6' */ :
	case 0x37 /* '7' */ :
	case 0x38 /* '8' */ :
	case 0x39 /* '9' */ :
	{
		matchRange('0','9');
		break;
	}
	case 0x61 /* 'a' */ :
	case 0x62 /* 'b' */ :
	case 0x63 /* 'c' */ :
	case 0x64 /* 'd' */ :
	case 0x65 /* 'e' */ :
	case 0x66 /* 'f' */ :
	{
		matchRange('a','f');
		break;
	}
	default:
	{
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	}
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}

void HTMLLexer::mUNDEFINED_TOKEN(bool _createToken) {
	int _ttype; ANTLR_USE_NAMESPACE(antlr)RefToken _token; ANTLR_USE_NAMESPACE(std)string::size_type _begin = text.length();
	_ttype = UNDEFINED_TOKEN;
	ANTLR_USE_NAMESPACE(std)string::size_type _saveIndex;
	
	if ((LA(1) == 0x3c /* '<' */ ) && ((LA(2) >= 0x3 /* '\3' */  && LA(2) <= 0xff))) {
		match('<' /* charlit */ );
		{ // ( ... )*
		for (;;) {
			if ((_tokenSet_12.member(LA(1)))) {
				matchNot('>' /* charlit */ );
			}
			else {
				goto _loop460;
			}
			
		}
		_loop460:;
		} // ( ... )*
		match('>' /* charlit */ );
		{ // ( ... )*
		for (;;) {
			if ((LA(1) == 0xa /* '\n' */  || LA(1) == 0xd /* '\r' */ )) {
				{
				if ((LA(1) == 0xd /* '\r' */ ) && (LA(2) == 0xa /* '\n' */ ) && (true) && (true)) {
					match("\r\n");
				}
				else if ((LA(1) == 0xd /* '\r' */ ) && (true) && (true) && (true)) {
					match('\r' /* charlit */ );
				}
				else if ((LA(1) == 0xa /* '\n' */ )) {
					match('\n' /* charlit */ );
				}
				else {
					throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
				}
				
				}
#line 1068 "html.g"
				newline();
#line 4355 "HTMLLexer.cpp"
			}
			else {
				goto _loop463;
			}
			
		}
		_loop463:;
		} // ( ... )*
#line 1070 "html.g"
		std::cerr << "invalid tag: " << text.substr(_begin,text.length()-_begin) << std::endl;
#line 4366 "HTMLLexer.cpp"
	}
	else if ((LA(1) == 0xa /* '\n' */  || LA(1) == 0xd /* '\r' */ ) && (true) && (true) && (true)) {
		{
		if ((LA(1) == 0xd /* '\r' */ ) && (LA(2) == 0xa /* '\n' */ )) {
			match("\r\n");
		}
		else if ((LA(1) == 0xd /* '\r' */ ) && (true)) {
			match('\r' /* charlit */ );
		}
		else if ((LA(1) == 0xa /* '\n' */ )) {
			match('\n' /* charlit */ );
		}
		else {
			throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
		}
		
		}
#line 1071 "html.g"
		newline();
#line 4386 "HTMLLexer.cpp"
	}
	else if (((LA(1) >= 0x3 /* '\3' */  && LA(1) <= 0xff)) && (true) && (true) && (true)) {
		matchNot(EOF/*_CHAR*/);
	}
	else {
		throw ANTLR_USE_NAMESPACE(antlr)NoViableAltForCharException(LA(1), getFilename(), getLine(), getColumn());
	}
	
	if ( _createToken && _token==ANTLR_USE_NAMESPACE(antlr)nullToken && _ttype!=ANTLR_USE_NAMESPACE(antlr)Token::SKIP ) {
	   _token = makeToken(_ttype);
	   _token->setText(text.substr(_begin, text.length()-_begin));
	}
	_returnToken = _token;
	_saveIndex=0;
}


const unsigned long HTMLLexer::_tokenSet_0_data_[] = { 9728UL, 1073741825UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// 0x9 0xa 0xd   > 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLLexer::_tokenSet_0(_tokenSet_0_data_,10);
const unsigned long HTMLLexer::_tokenSet_1_data_[] = { 9728UL, 1UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// 0x9 0xa 0xd   
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLLexer::_tokenSet_1(_tokenSet_1_data_,10);
const unsigned long HTMLLexer::_tokenSet_2_data_[] = { 4294967288UL, 2952790011UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf 0x10 0x11 0x12 0x13 
// 0x14 0x15 0x16 0x17 0x18 0x19 0x1a 0x1b 0x1c 0x1d 0x1e 0x1f   ! # $ 
// % & \' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; = ? @ A B C D E F G H 
// I J K L M N O P Q R S T U V W X Y Z [ 0x5c ] ^ _ ` a b c d e f g h i 
// j k l m n o p q r s t u v w x y z { | } ~ 0x7f 0x80 0x81 0x82 0x83 0x84 
// 0x85 0x86 0x87 0x88 0x89 0x8a 0x8b 0x8c 0x8d 0x8e 0x8f 0x90 0x91 0x92 
// 0x93 0x94 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLLexer::_tokenSet_2(_tokenSet_2_data_,16);
const unsigned long HTMLLexer::_tokenSet_3_data_[] = { 9728UL, 1073758209UL, 0UL, 134217726UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// 0x9 0xa 0xd   . > a b c d e f g h i j k l m n o p q r s t u v w x y 
// z 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLLexer::_tokenSet_3(_tokenSet_3_data_,10);
const unsigned long HTMLLexer::_tokenSet_4_data_[] = { 4294967288UL, 4294967291UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf 0x10 0x11 0x12 0x13 
// 0x14 0x15 0x16 0x17 0x18 0x19 0x1a 0x1b 0x1c 0x1d 0x1e 0x1f   ! # $ 
// % & \' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ? @ A B C D E F 
// G H I J K L M N O P Q R S T U V W X Y Z [ 0x5c ] ^ _ ` a b c d e f g 
// h i j k l m n o p q r s t u v w x y z { | } ~ 0x7f 0x80 0x81 0x82 0x83 
// 0x84 0x85 0x86 0x87 0x88 0x89 0x8a 0x8b 0x8c 0x8d 0x8e 0x8f 0x90 0x91 
// 0x92 0x93 0x94 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLLexer::_tokenSet_4(_tokenSet_4_data_,16);
const unsigned long HTMLLexer::_tokenSet_5_data_[] = { 4294967288UL, 4294967167UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf 0x10 0x11 0x12 0x13 
// 0x14 0x15 0x16 0x17 0x18 0x19 0x1a 0x1b 0x1c 0x1d 0x1e 0x1f   ! \" # 
// $ % & ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ? @ A B C D E F 
// G H I J K L M N O P Q R S T U V W X Y Z [ 0x5c ] ^ _ ` a b c d e f g 
// h i j k l m n o p q r s t u v w x y z { | } ~ 0x7f 0x80 0x81 0x82 0x83 
// 0x84 0x85 0x86 0x87 0x88 0x89 0x8a 0x8b 0x8c 0x8d 0x8e 0x8f 0x90 0x91 
// 0x92 0x93 0x94 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLLexer::_tokenSet_5(_tokenSet_5_data_,16);
const unsigned long HTMLLexer::_tokenSet_6_data_[] = { 0UL, 16384UL, 0UL, 134217726UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// . a b c d e f g h i j k l m n o p q r s t u v w x y z 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLLexer::_tokenSet_6(_tokenSet_6_data_,10);
const unsigned long HTMLLexer::_tokenSet_7_data_[] = { 4294958072UL, 2952790011UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xb 0xc 0xe 0xf 0x10 0x11 0x12 0x13 0x14 
// 0x15 0x16 0x17 0x18 0x19 0x1a 0x1b 0x1c 0x1d 0x1e 0x1f   ! # $ % & \' 
// ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; = ? @ A B C D E F G H I J K 
// L M N O P Q R S T U V W X Y Z [ 0x5c ] ^ _ ` a b c d e f g h i j k l 
// m n o p q r s t u v w x y z { | } ~ 0x7f 0x80 0x81 0x82 0x83 0x84 0x85 
// 0x86 0x87 0x88 0x89 0x8a 0x8b 0x8c 0x8d 0x8e 0x8f 0x90 0x91 0x92 0x93 
// 0x94 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLLexer::_tokenSet_7(_tokenSet_7_data_,16);
const unsigned long HTMLLexer::_tokenSet_8_data_[] = { 4294958072UL, 4294959103UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xb 0xc 0xe 0xf 0x10 0x11 0x12 0x13 0x14 
// 0x15 0x16 0x17 0x18 0x19 0x1a 0x1b 0x1c 0x1d 0x1e 0x1f   ! \" # $ % 
// & \' ( ) * + , . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ? @ A B C D E F G H 
// I J K L M N O P Q R S T U V W X Y Z [ 0x5c ] ^ _ ` a b c d e f g h i 
// j k l m n o p q r s t u v w x y z { | } ~ 0x7f 0x80 0x81 0x82 0x83 0x84 
// 0x85 0x86 0x87 0x88 0x89 0x8a 0x8b 0x8c 0x8d 0x8e 0x8f 0x90 0x91 0x92 
// 0x93 0x94 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLLexer::_tokenSet_8(_tokenSet_8_data_,16);
const unsigned long HTMLLexer::_tokenSet_9_data_[] = { 9728UL, 1677672481UL, 0UL, 134217726UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// 0x9 0xa 0xd   % . 0 1 2 3 4 5 6 7 8 9 = > a b c d e f g h i j k l m 
// n o p q r s t u v w x y z 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLLexer::_tokenSet_9(_tokenSet_9_data_,10);
const unsigned long HTMLLexer::_tokenSet_10_data_[] = { 0UL, 67043328UL, 0UL, 126UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// 0 1 2 3 4 5 6 7 8 9 a b c d e f 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLLexer::_tokenSet_10(_tokenSet_10_data_,10);
const unsigned long HTMLLexer::_tokenSet_11_data_[] = { 9728UL, 1140801537UL, 0UL, 134217726UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// 0x9 0xa 0xd   . 0 1 2 3 4 5 6 7 8 9 > a b c d e f g h i j k l m n o 
// p q r s t u v w x y z 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLLexer::_tokenSet_11(_tokenSet_11_data_,10);
const unsigned long HTMLLexer::_tokenSet_12_data_[] = { 4294967288UL, 3221225471UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 4294967295UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL, 0UL };
// 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf 0x10 0x11 0x12 0x13 
// 0x14 0x15 0x16 0x17 0x18 0x19 0x1a 0x1b 0x1c 0x1d 0x1e 0x1f   ! \" # 
// $ % & \' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = ? @ A B C D E F 
// G H I J K L M N O P Q R S T U V W X Y Z [ 0x5c ] ^ _ ` a b c d e f g 
// h i j k l m n o p q r s t u v w x y z { | } ~ 0x7f 0x80 0x81 0x82 0x83 
// 0x84 0x85 0x86 0x87 0x88 0x89 0x8a 0x8b 0x8c 0x8d 0x8e 0x8f 0x90 0x91 
// 0x92 0x93 0x94 
const ANTLR_USE_NAMESPACE(antlr)BitSet HTMLLexer::_tokenSet_12(_tokenSet_12_data_,16);

