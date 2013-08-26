/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 *
 * "@(#) "
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * javarias  Jun 2, 2010  	 created
 */

#ifndef LOGGINGACSHIERARCHYMANTAINER_H_
#define LOGGINGACSHIERARCHYMANTAINER_H_

#define LOG4CPP_FIX_ERROR_COLLISION 1

#include <log4cpp/HierarchyMaintainer.hh>
#include <log4cpp/Category.hh>

namespace logging {
/**
 * Class used as part of the ACS logging log4cpp port. It is used by ACSCategory
 * class to maintain the Categories (loggers) already created.
 */
class ACSHierarchyMaintainer: public virtual log4cpp::HierarchyMaintainer {

public:
	ACSHierarchyMaintainer();
	virtual ~ACSHierarchyMaintainer();
	static ACSHierarchyMaintainer& getDefaultMaintainer();

protected:
	log4cpp::Category& _getInstance(const std::string& name);
};
}
#endif /* LOGGINGACSHIERARCHYMANTAINER_H_ */
