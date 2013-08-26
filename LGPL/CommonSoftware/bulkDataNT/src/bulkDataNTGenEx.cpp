/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) AUI - Associated Universities Inc., 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************
 * 
 * "@(#) $Id: bulkDataNTGenEx.cpp,v 1.1 2013/02/11 18:37:33 rbourtem Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * jpisano   2007-12-20  created
 */

//
// System stuff
//
#include <sstream>

//
// Local stuff
//
#include "bulkDataNTGenEx.h"

using namespace std;

//------------------------------------------------------------------------------
string BDNTEx::asString() const
{
    stringstream str;

    str << "Exception thrown at " << m_errFileName << ":" << m_errLineNumber << " - " << m_errMsg;

    return str.str();
}

//------------------------------------------------------------------------------
const char *BDNTEx::asCString() const
{
    return asString().c_str();
}

/*___oOo___*/
