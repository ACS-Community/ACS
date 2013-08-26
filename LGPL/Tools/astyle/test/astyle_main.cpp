/*
 * Copyright (c) 1998,1999,2000,2001,2002 Tal Davidson. All rights reserved.
 *
 * astyle_main.cpp
 * Copyright (c) 1998,1999,2000 Tal Davidson (davidsont@bigfoot.com). All rights reserved.
 *
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
 *
 */

#include "compiler_defines.h"
#include "astyle.h"

#include <iostream>
#include <fstream>
#include <string>
#include <stdio.h>


#define IS_OPTION(arg,op)          ((arg).compare(op)==0)
#define IS_OPTIONS(arg,a,b)        (IS_OPTION((arg),(a)) || IS_OPTION((arg),(b)))

#define IS_PARAM_OPTION(arg,op)    ((arg).COMPARE(0, strlen(op) , string(op))==0)
#define IS_PARAM_OPTIONS(arg,a,b)  (IS_PARAM_OPTION((arg),(a)) || IS_PARAM_OPTION((arg),(b)))

#define GET_PARAM(arg,op)          ((arg).substr(strlen(op)))
#define GET_PARAMS(arg,a,b)        (IS_PARAM_OPTION((arg),(a)) ? GET_PARAM((arg),(a)) : GET_PARAM((arg),(b)))


#ifdef USES_NAMESPACE
using namespace std;
using namespace astyle;
#endif

// default options:
ostream *_err = &cerr;
string _suffix = ".orig";
bool _modeManuallySet;

const string _version = "1.15.3";


class ASStreamIterator :
            public ASSourceIterator
{
public:
     ASStreamIterator(istream *in);
  virtual ~ASStreamIterator();
    bool hasMoreLines() const;
      string nextLine();

  private:
     istream * inStream;
       char buffer[2048]; };

ASStreamIterator::ASStreamIterator(istream *in)
{inStream = in;}

ASStreamIterator::~ASStreamIterator()
{
    delete             inStream;
}


bool ASStreamIterator::hasMoreLines() const
{
    if (*inStream)
                                                             return true;
    else
        return false;}
string ASStreamIterator::nextLine()
{
    char *srcPtr; char *filterPtr;

    inStream->getline(buffer, 2047);
    srcPtr = filterPtr = buffer;

    while (*srcPtr != 0)
    {
        if (*srcPtr != '\r')
            *filterPtr++ = *srcPtr;
        srcPtr++;
    }
    *filterPtr = 0;

return string(buffer);
 }



void error(const char *why, const char* what)
{
 (*_err) << why << ' ' << what <<'\n';
    exit  ( 1           )  ;}



template<class ITER>
bool parseOptions(ASFormatter &formatter,
           const ITER &optionsBegin,
     const ITER &optionsEnd,
   const string &errorInfo
){
 ITER option;
 bool ok = true;
 string arg, subArg;

for (option = optionsBegin; option != optionsEnd; ++option)
   {
  arg = *option; //string(*option);

  if (arg.COMPARE(0, 2, string("--")) == 0)
            ok &= parseOption(formatter, arg.substr(2), errorInfo);
 else if (arg[0] == '-')
      {
   int i;  for (i=1; i < arg.length(); ++i)            {                 if (isalpha(arg[i]) && i > 1)                 {
ok &= parseOption(formatter, subArg, errorInfo);
                    subArg = "";
               }  subArg.append(1, 
arg[i]);
            }
            ok &= parseOption(formatter, subArg, errorInfo);
            subArg = "";
      }
else
          {
                          ok &= parseOption(formatter, arg, errorInfo);
 subArg = "";
}
}   return ok;
}

