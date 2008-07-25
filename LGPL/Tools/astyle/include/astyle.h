/*
 * Copyright (c) 1998,1999,2000,2001,2002 Tal Davidson. All rights reserved.
 *
 * astyle.h
 * by Tal Davidson (davidsont@bigfoot.com)
 * This file is a part of "Artistic Style" - an indentater and reformatter
 * of C, C++, C# and Java source files.
 *
 * The "Artistic Style" project, including all files needed to compile it,
 * is free software; you can redistribute it and/or use it and/or modify it
 * under the terms of the GNU General Public License as published 
 * by the Free Software Foundation; either version 2 of the License, 
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.
 */
#ifndef ASTYLE_H
#define ASTYLE_H

#include "compiler_defines.h"

#include <string>
#include <vector>


/* The enums below ave been moved up from inside the namespace astyle, since they
   for some strange reason are not recognized by 'std::vectors' in Microsoft Visual C++ 5
   when they are part of a namespace!!! There was no such problem with GNU's g++ compiler.
*/
enum BracketMode   { NONE_MODE, ATTACH_MODE, BREAK_MODE, BDAC_MODE };
enum BracketType   { NULL_TYPE = 0,
                     DEFINITION_TYPE = 1,
                     COMMAND_TYPE = 2,
                     ARRAY_TYPE  = 4,
                     SINGLE_LINE_TYPE = 8};


#ifdef USES_NAMESPACE
namespace astyle
{
#endif


class ASSourceIterator
{
    public:
        virtual bool hasMoreLines() const = 0;
        virtual std::string nextLine() = 0;
};



class ASResource
{
    public:
        static const std::string AS_IF, AS_ELSE;
        static const std::string AS_DO, AS_WHILE;
        static const std::string AS_FOR;
        static const std::string AS_SWITCH, AS_CASE, AS_DEFAULT;
        static const std::string AS_TRY, AS_CATCH, AS_THROWS, AS_FINALLY;
        static const std::string AS_PUBLIC, AS_PROTECTED, AS_PRIVATE;
        static const std::string AS_CLASS, AS_STRUCT, AS_UNION, AS_INTERFACE, AS_NAMESPACE, AS_EXTERN;
        static const std::string AS_STATIC;
        static const std::string AS_CONST;
        static const std::string AS_SYNCHRONIZED;
        static const std::string AS_OPERATOR, AS_TEMPLATE;
        static const std::string AS_OPEN_BRACKET, AS_CLOSE_BRACKET;
        static const std::string AS_OPEN_LINE_COMMENT, AS_OPEN_COMMENT, AS_CLOSE_COMMENT;
        static const std::string AS_BAR_DEFINE, AS_BAR_INCLUDE, AS_BAR_IF, AS_BAR_EL, AS_BAR_ENDIF;
        static const std::string AS_RETURN;
        static const std::string AS_ASSIGN, AS_PLUS_ASSIGN, AS_MINUS_ASSIGN, AS_MULT_ASSIGN;
        static const std::string AS_DIV_ASSIGN, AS_MOD_ASSIGN, AS_XOR_ASSIGN, AS_OR_ASSIGN, AS_AND_ASSIGN;
        static const std::string AS_GR_GR_ASSIGN, AS_LS_LS_ASSIGN, AS_GR_GR_GR_ASSIGN, AS_LS_LS_LS_ASSIGN;
        static const std::string AS_EQUAL, AS_PLUS_PLUS, AS_MINUS_MINUS, AS_NOT_EQUAL, AS_GR_EQUAL, AS_GR_GR_GR, AS_GR_GR;
        static const std::string AS_LS_EQUAL, AS_LS_LS_LS, AS_LS_LS, AS_ARROW, AS_AND, AS_OR;
        static const std::string AS_COLON_COLON, AS_PAREN_PAREN, AS_BLPAREN_BLPAREN;
        static const std::string AS_PLUS, AS_MINUS, AS_MULT, AS_DIV, AS_MOD, AS_GR, AS_LS;
        static const std::string AS_NOT, AS_BIT_XOR, AS_BIT_OR, AS_BIT_AND, AS_BIT_NOT;
        static const std::string AS_QUESTION, AS_COLON, AS_SEMICOLON, AS_COMMA;
		static const std::string AS_ASM;
        static const std::string AS_FOREACH, AS_LOCK, AS_UNSAFE, AS_FIXED; 
        static const std::string AS_GET, AS_SET, AS_ADD, AS_REMOVE;
};

class ASBeautifier : protected ASResource
{
    public:
        ASBeautifier();
        virtual ~ASBeautifier();
        virtual void init(ASSourceIterator* iter); // pointer to dynamically created iterator.
        virtual void init();
        virtual bool hasMoreLines() const;
        virtual std::string nextLine();
        virtual std::string beautify(const std::string &line);
        void setTabIndentation(int length = 4, bool forceTabs = false);
        void setSpaceIndentation(int length = 4);
        void setMaxInStatementIndentLength(int max);
        void setMinConditionalIndentLength(int min);
        void setClassIndent(bool state);
        void setSwitchIndent(bool state);
        void setCaseIndent(bool state);
        void setBracketIndent(bool state);
        void setBlockIndent(bool state);
        void setNamespaceIndent(bool state);
        void setLabelIndent(bool state);
        void setCStyle();
        void setJavaStyle();
        void setEmptyLineFill(bool state);
        void setPreprocessorIndent(bool state);


    protected:
        int getNextProgramCharDistance(const std::string &line, int i);
        bool isLegalNameChar(char ch) const;
        bool isWhiteSpace(char ch) const;
        const std::string *findHeader(const std::string &line, int i,
                                 const std::vector<const std::string*> &possibleHeaders,
                                 bool checkBoundry = true);
        std::string trim(const std::string &str);
        int indexOf(std::vector<const std::string*> &container, const std::string *element);

    private:
        ASBeautifier(const ASBeautifier &copy);
        void operator=(ASBeautifier&); // not to be implemented

        void initStatic();
        void registerInStatementIndent(const std::string &line, int i, int spaceTabCount,
                                       int minIndent, bool updateParenStack);
        std::string preLineWS(int spaceTabCount, int tabCount);

        static std::vector<const std::string*> headers;
        static std::vector<const std::string*> nonParenHeaders;
        static std::vector<const std::string*> preprocessorHeaders;
        static std::vector<const std::string*> preBlockStatements;
        static std::vector<const std::string*> assignmentOperators;
        static std::vector<const std::string*> nonAssignmentOperators;

        static bool calledInitStatic;

        ASSourceIterator *sourceIterator;
        std::vector<ASBeautifier*> *waitingBeautifierStack;
        std::vector<ASBeautifier*> *activeBeautifierStack;
        std::vector<int> *waitingBeautifierStackLengthStack;
        std::vector<int> *activeBeautifierStackLengthStack;
        std::vector<const std::string*> *headerStack;
        std::vector< std::vector<const std::string*>* > *tempStacks;
        std::vector<int> *blockParenDepthStack;
        std::vector<bool> *blockStatementStack;
        std::vector<bool> *parenStatementStack;
        std::vector<int> *inStatementIndentStack;
        std::vector<int> *inStatementIndentStackSizeStack;
        std::vector<int> *parenIndentStack;
        std::vector<bool> *bracketBlockStateStack;
        std::string indentString;
        const std::string *currentHeader;
        const std::string *previousLastLineHeader;
        const std::string *immediatelyPreviousAssignmentOp;
        const std::string *probationHeader;
        bool isInQuote;
        bool isInComment;
        bool isInCase;
        bool isInQuestion;
        bool isInStatement;
        bool isInHeader;
        bool isCStyle;
        bool isInOperator;
        bool isInTemplate;
        bool isInConst;
        bool isInDefine;
        bool isInDefineDefinition;
        bool classIndent;
        bool isInClassHeader;
        bool isInClassHeaderTab;
        bool switchIndent;
        bool caseIndent;
        bool namespaceIndent;
        bool bracketIndent;
        bool blockIndent;
        bool labelIndent;
        bool preprocessorIndent;
        bool isInConditional;
        bool isMinimalConditinalIndentSet;
		bool shouldForceTabIndentation;
        int minConditionalIndent;
        int parenDepth;
        int indentLength;
        int blockTabCount;
        int leadingWhiteSpaces;
        int maxInStatementIndent;
        int templateDepth;
        char quoteChar;
        char prevNonSpaceCh;
        char currentNonSpaceCh;
        char currentNonLegalCh;
        char prevNonLegalCh;
        int prevFinalLineSpaceTabCount;
        int prevFinalLineTabCount;
        bool emptyLineFill;
        bool backslashEndsPrevLine;
        int defineTabCount;
};


class ASFormatter : public ASBeautifier
{
    public:
        ASFormatter();
        virtual ~ASFormatter();
        virtual void init(ASSourceIterator* iter);
        virtual bool hasMoreLines() const;
        virtual std::string nextLine();
        void setBracketFormatMode(BracketMode mode);
        void setBreakClosingHeaderBracketsMode(bool state);
        void setOperatorPaddingMode(bool mode);
        void setParenthesisPaddingMode(bool mode);
        void setBreakOneLineBlocksMode(bool state);
        void setSingleStatementsMode(bool state);
        void setTabSpaceConversionMode(bool state);
		void setBreakBlocksMode(bool state);
		void setBreakClosingHeaderBlocksMode(bool state);
		void setBreakElseIfsMode(bool state);

    private:
        void ASformatter(ASFormatter &copy); // not to be imlpemented
        void operator=(ASFormatter&); // not to be implemented
        void staticInit();
        bool isFormattingEnabled() const;
        void goForward(int i);
        bool getNextChar();
        char peekNextChar() const;
        bool isBeforeComment() const;
        void trimNewLine();
        BracketType getBracketType() const;
        bool isPointerOrReference() const;
        bool isUrinaryMinus() const;
        bool isInExponent() const;
        bool isOneLineBlockReached() const;
        void appendChar(char ch, bool canBreakLine = true);
        void appendCurrentChar(bool canBreakLine = true);
        void appendSequence(const std::string &sequence, bool canBreakLine = true);
        void appendSpacePad();
        void breakLine();
        inline bool isSequenceReached(const std::string &sequence) const;
        const std::string *findHeader(const std::vector<const std::string*> &headers, bool checkBoundry = true);

        static std::vector<const std::string*> headers;
        static std::vector<const std::string*> nonParenHeaders;
        static std::vector<const std::string*> preprocessorHeaders;
        static std::vector<const std::string*> preDefinitionHeaders;
        static std::vector<const std::string*> preCommandHeaders;
        static std::vector<const std::string*> operators;
        static std::vector<const std::string*> assignmentOperators;
        static bool calledInitStatic;

        ASSourceIterator *sourceIterator;
        std::vector<const std::string*> *preBracketHeaderStack;
        std::vector<BracketType> *bracketTypeStack;
        std::vector<int> *parenStack;
        std::string readyFormattedLine;
        std::string currentLine;
        std::string formattedLine;
        const std::string *currentHeader;
        const std::string *previousOperator;
        char currentChar;
        char previousChar;
        char previousNonWSChar;
        char previousCommandChar;
        char quoteChar;
        int charNum;
        BracketMode bracketFormatMode;
        bool isVirgin;
        bool shouldPadOperators;
        bool shouldPadParenthesies;
        bool shouldConvertTabs;
        bool isInLineComment;
        bool isInComment;
        bool isInPreprocessor;
        bool isInTemplate;			// true both in template definitions (e.g. template<class A>) and template usage (e.g. F<int>).
        bool doesLineStartComment;
        bool isInQuote;
        bool isSpecialChar;
        bool isNonParenHeader;
        bool foundQuestionMark;
        bool foundPreDefinitionHeader;
        bool foundPreCommandHeader;
        bool isInLineBreak;
        bool isInClosingBracketLineBreak;
        bool endOfCodeReached;
        bool isLineReady;
        bool isPreviousBracketBlockRelated;
        bool isInPotentialCalculation;
        //bool foundOneLineBlock;
        bool shouldBreakOneLineBlocks;
        bool shouldReparseCurrentChar;
        bool shouldBreakOneLineStatements;
        bool shouldBreakLineAfterComments;
		bool shouldBreakClosingHeaderBrackets;
		bool shouldBreakElseIfs;
        bool passedSemicolon;
        bool passedColon;
        bool isImmediatelyPostComment;
        bool isImmediatelyPostLineComment;
		bool isImmediatelyPostEmptyBlock;

	    bool shouldBreakBlocks;
	    bool shouldBreakClosingHeaderBlocks;
        bool isPrependPostBlockEmptyLineRequested;
        bool isAppendPostBlockEmptyLineRequested;

	    bool prependEmptyLine;
	    bool foundClosingHeader;
	    int previousReadyFormattedLineLength;

		bool isInHeader;
		bool isImmediatelyPostHeader;

};


#ifdef USES_NAMESPACE
}
#endif

#endif // closes ASTYLE_H

