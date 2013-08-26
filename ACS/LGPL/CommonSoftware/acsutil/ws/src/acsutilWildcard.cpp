/*******************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2002
* Copyright by ESO (in the framework of the ALMA collaboration)
* and Cosylab 2002, All rights reserved
* 
* This implementation was adpoted from:
*
* Copyright (C) 1996, 1997, 1998, 1999, 2000 Florian Schintke
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software  
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA02111-1307  USA
*
* F.Schintke, the author of the original code, has authorized to 
* distribute these files under LGPL License.
*
* "@(#) $Id: acsutilWildcard.cpp,v 1.15 2003/03/10 14:33:43 rgeorgie Exp $"
* 
* ----------------------
* Implementation of the UN*X wildcards
* Supported wild-characters: '*', '?'; sets: [a-z], '!' negation
* Examples:
*	 '[a-g]l*i?n' matches 'florian'
*	 '[!abc]*e' matches 'smile'
*	 '[-z] matches 'a'
*
* 
* who       when      what
* --------  --------  ----------------------------------------------
* gchiozzi 2002-12-05 Added proper GPL licence header
* msekoran  2001/06/20  created
*/

#include <vltPort.h>
#include <acsutilWildcard.h>

int
Wildcard::wildcardfit (const char *wildcard, const char *test)
{
  int fit = 1;
  
  for (; ('\000' != *wildcard) && (1 == fit) && ('\000' != *test); wildcard++)
    {
      switch (*wildcard)
        {
        case '[':
	  wildcard++; /* leave out the opening square bracket */ 
          fit = set (&wildcard, &test);
	  /* we don't need to decrement the wildcard as in case */
	  /* of asterisk because the closing ] is still there */
          break;
        case '?':
          test++;
          break;
        case '*':
          fit = asterisk (&wildcard, &test);
	  /* the asterisk was skipped by asterisk() but the loop will */
	  /* increment by itself. So we have to decrement */
	  wildcard--;
          break;
        default:
          fit = (int) (*wildcard == *test);
          test++;
        }
    }
  while ((*wildcard == '*') && (1 == fit)) 
    /* here the teststring is empty otherwise you cannot */
    /* leave the previous loop */ 
    wildcard++;
  return (int) ((1 == fit) && ('\0' == *test) && ('\0' == *wildcard));
}

int
Wildcard::set (const char **wildcard, const char **test)
{
  int fit = 0;
  int negation = 0;
  int at_beginning = 1;

  if ('!' == **wildcard)
    {
      negation = 1;
      (*wildcard)++;
    }
  while ((']' != **wildcard) || (1 == at_beginning))
    {
      if (0 == fit)
        {
          if (('-' == **wildcard) 
              && ((*(*wildcard - 1)) < (*(*wildcard + 1)))
              && (']' != *(*wildcard + 1))
	      && (0 == at_beginning))
            {
              if (((**test) >= (*(*wildcard - 1)))
                  && ((**test) <= (*(*wildcard + 1))))
                {
                  fit = 1;
                  (*wildcard)++;
                }
            }
          else if ((**wildcard) == (**test))
            {
              fit = 1;
            }
        }
      (*wildcard)++;
      at_beginning = 0;
    }
  if (1 == negation)
    /* change from zero to one and vice versa */
    fit = 1 - fit;
  if (1 == fit) 
    (*test)++;

  return (fit);
}

int
Wildcard::asterisk (const char **wildcard, const char **test)
{
  /* Warning: uses multiple returns */
  int fit = 1;

  /* erase the leading asterisk */
  (*wildcard)++; 
  while (('\000' != (**test))
	 && (('?' == **wildcard) 
	     || ('*' == **wildcard)))
    {
      if ('?' == **wildcard) 
	(*test)++;
      (*wildcard)++;
    }
  /* Now it could be that test is empty and wildcard contains */
  /* aterisks. Then we delete them to get a proper state */
  while ('*' == (**wildcard))
    (*wildcard)++;

  if (('\0' == (**test)) && ('\0' != (**wildcard)))
    return (fit = 0);
  if (('\0' == (**test)) && ('\0' == (**wildcard)))
    return (fit = 1); 
  else
    {
      /* Neither test nor wildcard are empty!          */
      /* the first character of wildcard isn't in [*?] */
      if (0 == wildcardfit(*wildcard, (*test)))
	{
	  do 
	    {
	      (*test)++;
	      /* skip as much characters as possible in the teststring */
	      /* stop if a character match occurs */
	      while (((**wildcard) != (**test)) 
		     && ('['  != (**wildcard))
		     && ('\0' != (**test)))
		(*test)++;
	    }
	  while ((('\0' != **test))? 
		 (0 == wildcardfit (*wildcard, (*test))) 
		 : (0 != (fit = 0)));
	}
      if (('\0' == **test) && ('\0' == **wildcard))
	fit = 1;
      return (fit);
    }
}

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: acsutilWildcard.cpp,v $
// Revision 1.15  2003/03/10 14:33:43  rgeorgie
// LGPL
//
// Revision 1.14  2002/12/05 12:31:58  vltsccm
// gchiozzi: Added proper GPL licence header to wildcard library
//
// Revision 1.13  2002/12/03 20:00:56  vltsccm
// acsutil1.13
//
// Revision 1.12  2002/11/25 15:16:31  vltsccm
// acsutil1.12
//
// Revision 1.11  2002/09/23 09:35:32  vltsccm
// acsutil1.11
//
// Revision 1.10  2002/04/05 13:02:56  vltsccm
// acsutil1.10
//
// Revision 1.9  2002/04/04 11:06:25  vltsccm
// acsutil1.9
//
// Revision 1.8  2002/03/27 16:41:49  vltsccm
// acsutil1.8
//
// Revision 1.7  2002/03/14 16:24:47  vltsccm
// acsutil1.7
//
// Revision 1.6  2002/01/08 15:47:24  vltsccm
// acsutil1.6
//
// Revision 1.5  2002/01/08 14:16:17  vltsccm
// acsutil1.5
//
// Revision 1.4  2001/12/07 07:50:23  vltsccm
// acsutil1.4
//
// Revision 1.3  2001/11/12 09:36:56  vltsccm
// acsutil1.3
//
// Revision 1.2  2001/07/12 07:03:13  vltsccm
// acsutil1.2
//
// Revision 1.1  2001/07/11 09:12:35  vltsccm
// acsutil1.1
//
// Revision 1.0  2001/07/11 09:12:34  vltsccm
// acsutil1.0
//
//
// ************************************************************************
