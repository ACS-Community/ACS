#ifndef _acserrOldDescriptions_H_
#define _acserrOldDescriptions_H_
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acserrOldDescriptions.h,v 1.1 2003/11/04 10:59:22 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2003-11-04  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif



const char oldDescriptions[][15][60]={
    {"*OLD* OK"},
    {"*OLD* A regular timer monitor triggered the event.", "*OLD* The value changed by a specified amount or more"},
    {"*OLD* All alarm conditions have disappeared.", "*OLD* At least one alarm condition remains.", "*OLD* Value below alarm LoLo (includes hysteresis).", "*OLD* Value above alarm HiHi (includes hysteresis).", "*OLD* An alarm on the status determined by software.", "*OLD* An alarm on the status from the hardware."},
    {""},
    {""},
    {""},
    {""},
    {""},
    {""},
    {""},
    {"*OLD* File not Found", "*OLD* Out of Bounds", "*OLD* Input Output Error", "*OLD* Unknown Error"},
    {"*OLD* Test error 0", "*OLD* Test error 1", "*OLD* Test error 2", "*OLD* Test error 3", "*OLD* Test error 4", "*OLD* Test error 5", "*OLD* ACSErrTestException0", "*OLD* ACSErrTestException1", "*OLD* ACSErrTestException2", "*OLD* ACSErrTestException3"},
    {""/*TypeTest*/},
    {""/*TICS*/},
    {""/*TicsTCorr*/},
    {"*OLD* Failed to resolve service reference."}
};

#endif /*!_H*/
