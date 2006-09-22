#ifndef PROPERTIES_H
#define PROPERTIES_H

#include <map>
#include <string>
#include <vector>
#include <memory>
#include <stdexcept>

using std::map;
using std::string;
using std::vector;
using std::auto_ptr;
using std::invalid_argument;

namespace laserUtil
{
	typedef map< string, string >::value_type PropertyMapEntryType;

	/*
	 * Utility class containing a collection of properties, which are name/value pairs of strings.
	 */
	class Properties
	{
		private:

			map<string, string> propertiesMap;

		public:

			// constructors
			Properties();
			Properties(const Properties &);

			// destructor
			virtual ~Properties();

			// assignment operator
			Properties & operator=(const Properties & rhs);

			// equality operator
			int operator==(const Properties &rhs) const;

			//Searches for the property with the specified key in this property list.
			string getProperty(string key); 

 			// Returns an enumeration of all the keys in this property list, 
			// including distinct keys in the default property list if a key 
			// of the same name has not already been found from the main properties list.
			auto_ptr<vector<string> > propertyNames();

			// Calls the map method put.
			void setProperty(string key, string value) throw(invalid_argument);

			// Returns an XML fragment (NOT a complete document) representing all of 
			// the properties contained in this table, for use in the message transported
			// from an alarm source to the alarm server.
			// @param amountToIndent - used to specify a level of indentation (in spaces) for readability
			string toXML(int amountToIndent = 6);
	};
}
#endif
