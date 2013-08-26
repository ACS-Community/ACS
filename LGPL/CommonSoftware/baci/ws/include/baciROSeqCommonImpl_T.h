#ifndef _baciROSeqCommonImpl_T_H_
#define _baciROSeqCommonImpl_T_H_
/*******************************************************************
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
* "@(#) $Id: baciROSeqCommonImpl_T"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    17/06/2004  created
*/

/** 
 * @file 
 * Header file for BACI Read-only Sequence Common Class.
 * 
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

/**
 * Helper macro for use with template parameters.
 */
#define ACS_RO_SEQ_T(T, CT) ACS::T##Seq*, ACS::CB##T##Seq, ACS::T##SeqSeq, ACS::T##Seq##Seq_out, ACS::Monitor##T, baci::Monitor##T##Seq,  ACS::T##Seq, CT, CT, POA_ACS::RO##T##Seq, ACS::Alarm##T, POA_ACS::CB##T##Seq, const ACS::T##Seq&

#endif



