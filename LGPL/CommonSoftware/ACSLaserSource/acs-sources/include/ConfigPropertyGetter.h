#ifndef CONFIG_PROPERTY_GETTER_H
#define CONFIG_PROPERTY_GETTER_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2006 
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
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id$"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2006-07-12  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <expat.h>
#include <list>

#include <maciS.h>
#include "acsErrTypeAlarmSourceFactory.h"

typedef struct {
	std::string key;
	std::string value;
} Property;

/**
 * Object of this class get a property with the given name from the CDB
 * (The property must be into AlarmSystemConfiguration.xml
 */
class ConfigPropertyGetter {
	private:
		// The DAO record with the AS properties
		std::string m_dao;
		// The list of the AS props in the DAO record
		std::list<Property>* m_properties;
	public:
		ConfigPropertyGetter(maci::Manager_ptr manager);
		~ConfigPropertyGetter();
		
		/**
		 * Return the value of a property with the given name
		 * scanning the list of properties
		 * 
		 * It returns an empty string if the property doesn't exist or any other
		 * error occurs
		 */
		std::string getProperty(std::string propName);
	
	private:
		/**
		 * Get the DAO for the AS properties
		 * 
		 * Return an empty string if the DAO doesn't exist
		 * (this is not an error because the missing DAO means to use ACS
		 * implementation)
		 */
		std::string getDAO(maci::Manager_ptr manager);
		
		/**
		 * Parse the DAO building the list of the properties
		 */
		void parseDAO();
		
		/**
	     * The handler for the start element
	     *
	     * @see libexpat documentation for further info
	     */
	    static void start_hndl(void *data, const XML_Char *el, const XML_Char **attr);
	
	    /**
	     * The handler for the end element
	     *
	     * @see libexpat documentation for further info
	     */
	    static void end_hndl(void *data, const XML_Char *el);
	
	   /**
	    * The handler for the char element
	    *
	    * @see libexpat documentation for further info
	    */
	    static void char_hndl(void *data, const XML_Char *s, int len);
		
};

#endif /*!CONFIG_PROPERTY_GETTER_H*/
