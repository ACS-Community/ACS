#ifndef TIMESTAMP_H
#define TIMESTAMP_H

#include "utilConstants.h"
#include <string>

using std::string;

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
			string toXML(string elementName = USER_TIMESTAMP_ELEMENT_NAME, int amountToIndent = 6);

		private:
			long seconds;
			long microSeconds;
	};
}
#endif
