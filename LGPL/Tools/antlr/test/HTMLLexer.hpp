#ifndef INC_HTMLLexer_hpp_
#define INC_HTMLLexer_hpp_

#include <antlr/config.hpp>
/* $ANTLR 2.7.6 (20060308): "html.g" -> "HTMLLexer.hpp"$ */
#include <antlr/CommonToken.hpp>
#include <antlr/InputBuffer.hpp>
#include <antlr/BitSet.hpp>
#include "HTMLTokenTypes.hpp"
#include <antlr/CharScanner.hpp>
#line 1 "html.g"

#include <iostream>

#line 16 "HTMLLexer.hpp"
class CUSTOM_API HTMLLexer : public ANTLR_USE_NAMESPACE(antlr)CharScanner, public HTMLTokenTypes
{
#line 1 "html.g"
#line 20 "HTMLLexer.hpp"
private:
	void initLiterals();
public:
	bool getCaseSensitiveLiterals() const
	{
		return true;
	}
public:
	HTMLLexer(ANTLR_USE_NAMESPACE(std)istream& in);
	HTMLLexer(ANTLR_USE_NAMESPACE(antlr)InputBuffer& ib);
	HTMLLexer(const ANTLR_USE_NAMESPACE(antlr)LexerSharedInputState& state);
	ANTLR_USE_NAMESPACE(antlr)RefToken nextToken();
	public: void mDOCTYPE(bool _createToken);
	protected: void mWS_(bool _createToken);
	protected: void mSTRING(bool _createToken);
	public: void mOHTML(bool _createToken);
	public: void mCHTML(bool _createToken);
	public: void mOHEAD(bool _createToken);
	public: void mCHEAD(bool _createToken);
	public: void mOBODY(bool _createToken);
	protected: void mATTR(bool _createToken);
	public: void mCBODY(bool _createToken);
	public: void mOTITLE(bool _createToken);
	public: void mCTITLE(bool _createToken);
	public: void mOSCRIPT(bool _createToken);
	public: void mCSCRIPT(bool _createToken);
	public: void mISINDEX(bool _createToken);
	public: void mMETA(bool _createToken);
	public: void mLINK(bool _createToken);
	public: void mOH1(bool _createToken);
	public: void mCH1(bool _createToken);
	public: void mOH2(bool _createToken);
	public: void mCH2(bool _createToken);
	public: void mOH3(bool _createToken);
	public: void mCH3(bool _createToken);
	public: void mOH4(bool _createToken);
	public: void mCH4(bool _createToken);
	public: void mOH5(bool _createToken);
	public: void mCH5(bool _createToken);
	public: void mOH6(bool _createToken);
	public: void mCH6(bool _createToken);
	public: void mOADDRESS(bool _createToken);
	public: void mCADDRESS(bool _createToken);
	public: void mOPARA(bool _createToken);
	public: void mCPARA(bool _createToken);
	public: void mOULIST(bool _createToken);
	public: void mCULIST(bool _createToken);
	public: void mOOLIST(bool _createToken);
	public: void mCOLIST(bool _createToken);
	public: void mOLITEM(bool _createToken);
	public: void mCLITEM(bool _createToken);
	public: void mODLIST(bool _createToken);
	public: void mCDLIST(bool _createToken);
	public: void mODTERM(bool _createToken);
	public: void mCDTERM(bool _createToken);
	public: void mODDEF(bool _createToken);
	public: void mCDDEF(bool _createToken);
	public: void mODIR(bool _createToken);
	public: void mCDIR_OR_CDIV(bool _createToken);
	public: void mODIV(bool _createToken);
	public: void mOMENU(bool _createToken);
	public: void mCMENU(bool _createToken);
	public: void mOPRE(bool _createToken);
	public: void mCPRE(bool _createToken);
	public: void mOCENTER(bool _createToken);
	public: void mCCENTER(bool _createToken);
	public: void mOBQUOTE(bool _createToken);
	public: void mCBQUOTE(bool _createToken);
	public: void mHR(bool _createToken);
	public: void mOTABLE(bool _createToken);
	public: void mCTABLE(bool _createToken);
	public: void mOCAP(bool _createToken);
	public: void mCCAP(bool _createToken);
	public: void mO_TR(bool _createToken);
	public: void mC_TR(bool _createToken);
	public: void mO_TH_OR_TD(bool _createToken);
	public: void mC_TH_OR_TD(bool _createToken);
	public: void mOTTYPE(bool _createToken);
	public: void mCTTYPE(bool _createToken);
	public: void mOITALIC(bool _createToken);
	public: void mCITALIC(bool _createToken);
	public: void mOBOLD(bool _createToken);
	public: void mCBOLD(bool _createToken);
	public: void mOUNDER(bool _createToken);
	public: void mCUNDER(bool _createToken);
	public: void mOSTRIKE_OR_OSTRONG(bool _createToken);
	public: void mCST_LEFT_FACTORED(bool _createToken);
	public: void mOSTYLE(bool _createToken);
	public: void mOBIG(bool _createToken);
	public: void mCBIG(bool _createToken);
	public: void mOSMALL(bool _createToken);
	public: void mCSMALL(bool _createToken);
	public: void mOSUB(bool _createToken);
	public: void mOSUP(bool _createToken);
	public: void mCSUB_OR_CSUP(bool _createToken);
	public: void mOEM(bool _createToken);
	public: void mCEM(bool _createToken);
	public: void mODFN(bool _createToken);
	public: void mCDFN(bool _createToken);
	public: void mOCODE(bool _createToken);
	public: void mCCODE(bool _createToken);
	public: void mOSAMP(bool _createToken);
	public: void mCSAMP(bool _createToken);
	public: void mOKBD(bool _createToken);
	public: void mCKBD(bool _createToken);
	public: void mOVAR(bool _createToken);
	public: void mCVAR(bool _createToken);
	public: void mOCITE(bool _createToken);
	public: void mCCITE(bool _createToken);
	public: void mINPUT(bool _createToken);
	public: void mOSELECT(bool _createToken);
	public: void mCSELECT(bool _createToken);
	public: void mOTAREA(bool _createToken);
	public: void mCTAREA(bool _createToken);
	public: void mSELOPT(bool _createToken);
	public: void mOANCHOR(bool _createToken);
	public: void mCANCHOR(bool _createToken);
	public: void mIMG(bool _createToken);
	public: void mOAPPLET(bool _createToken);
	public: void mCAPPLET(bool _createToken);
	public: void mAPARM(bool _createToken);
	public: void mOFORM(bool _createToken);
	public: void mOFONT(bool _createToken);
	public: void mCFORM_OR_CFONT(bool _createToken);
	public: void mBFONT_OR_BASE(bool _createToken);
	public: void mBR(bool _createToken);
	public: void mOMAP(bool _createToken);
	public: void mCMAP(bool _createToken);
	public: void mAREA(bool _createToken);
	public: void mPCDATA(bool _createToken);
	protected: void mCOMMENT_DATA(bool _createToken);
	public: void mCOMMENT(bool _createToken);
	protected: void mWORD(bool _createToken);
	protected: void mINT(bool _createToken);
	protected: void mHEXNUM(bool _createToken);
	protected: void mLCLETTER(bool _createToken);
	protected: void mDIGIT(bool _createToken);
	protected: void mWSCHARS(bool _createToken);
	protected: void mSPECIAL(bool _createToken);
	protected: void mHEXINT(bool _createToken);
	protected: void mHEXDIGIT(bool _createToken);
	protected: void mUNDEFINED_TOKEN(bool _createToken);
private:
	
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
};

#endif /*INC_HTMLLexer_hpp_*/
