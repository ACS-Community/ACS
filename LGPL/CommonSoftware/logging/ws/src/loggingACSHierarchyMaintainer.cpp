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
 * javarias  Jun 3, 2010  	 created
 */

#include "loggingACSHierarchyMaintainer.h"
#include "loggingACSCategory.h"

using namespace logging;

ACSHierarchyMaintainer::ACSHierarchyMaintainer() :
	log4cpp::HierarchyMaintainer() {

}

ACSHierarchyMaintainer::~ACSHierarchyMaintainer() {
}

/* This method is based in the original
 * logg4cpp::HierarchyMaintainer::_getInstance method
 */
log4cpp::Category& ACSHierarchyMaintainer::_getInstance(const std::string& name) {
	log4cpp::Category* result;
	result = _getExistingInstance(name);

	if (NULL == result) {
		if (name == "") {
			result = new ACSCategory(name, NULL, log4cpp::Priority::INFO);
		} else {
			std::string parentName;
			size_t dotIndex = name.find_last_of('.');
			if (name.length() <= dotIndex) {
				parentName = "";
			} else {
				parentName = name.substr(0, dotIndex);
			}
			log4cpp::Category& parent = _getInstance(parentName);
			result = new ACSCategory(name, &parent, log4cpp::Priority::NOTSET);
		}
		_categoryMap[name] = result;
	}
	return *result;
}
ACSHierarchyMaintainer& ACSHierarchyMaintainer::getDefaultMaintainer() {
	static ACSHierarchyMaintainer defaultMaintainer;

	return defaultMaintainer;
}
