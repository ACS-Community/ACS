/*
 * CorbaNotifyUtils.h
 *
 *  Created on: Oct 1, 2014
 *      Author: almamgr
 */

#ifndef CORBANOTIFYUTILS_H_
#define CORBANOTIFYUTILS_H_

#include <string>
#include <ace_wchar.h>

class CorbaNotifyUtils {
public:
	static void getORBOptions(const std::string &appName,const std::string &orbOptions,
			int &orbArgc,ACE_TCHAR*** orbArgv);

	static void delORBOptions(int &orbArgc,ACE_TCHAR*** orbArgv);


};

#endif /* CORBANOTIFYUTILS_H_ */
