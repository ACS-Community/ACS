#ifndef TIMESTAMP_H
#define TIMESTAMP_H

/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
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
 *******************************************************************************/
#include "utilConstants.h"
#include <string>

namespace acsalarm
{
	/*
	 * Utility class to hold a timestamp. This class is intended primarily for use 
	 * by the cpp laser alarm source library, but it may have other uses. It is 
	 * not very sophisticated, however.
	 */
	class Timestamp
	{
		public:

			// constructors
			Timestamp();
			Timestamp(const Timestamp &);
			Timestamp(long secs, long microSecs);

			// destructor
			virtual ~Timestamp();

			// assignment operator
			Timestamp & operator=(const Timestamp & rhs);

			// equality operator
			int operator==(const Timestamp &rhs) const;

			// accessor for the seconds
			long getSeconds() { return seconds; }

			// mutator for the seconds
			void setSeconds(long newSecs) { seconds = newSecs; }

			// accessor for the microseconds
			long getMicroSeconds() { return microSeconds; }

			// mutator for the microseconds
			void setMicroSeconds(long newMicroSecs) { microSeconds = newMicroSecs; }

			/**
 			 * Returns an XML fragment (NOT a complete document) representing the timestamp, for
 			 * use in the message that is transported from alarm source to alarm server.
 			 *
 			 * @param elementName the element name when generating the XML fragment,
 			 *        for instance in the example below the elementName is "source-timestamp"
 			 *
 			 * For example:
 			 *
 			 * <source-timestamp seconds="1129902763" microseconds="132000"/>
			 *
			 * @param amountToIndent - used to specify a level of indentation (in spaces) for readability
 			 */
			std::string toXML(std::string elementName = USER_TIMESTAMP_ELEMENT_NAME, int amountToIndent = 6);

		private:
			long seconds;
			long microSeconds;
	};
}
#endif
