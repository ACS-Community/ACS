#ifndef LOGGING_ACEMACROS_H_
#define LOGGING_ACEMACROS_H_

#ifdef ENABLE_LOG4CPP_MACROS

#include "loggingLog4cppMACROS.h"
#include "loggingLog4cpp.h"

#define FIELD_UNAVAILABLE "Unavailable"

#define ACS_DEBUG(routine, text) \
		LOG(log4cpp::Priority::Debug, routine, text);

#define ACS_STATIC_DEBUG(routine, text) \
		STATIC_LOG(log4cpp::Priority::Debug, routine. text);

#define ACS_DEBUG_PARAM(routine, text, param) \
	{ \
		logging::BasicLogInfo tStruct; \
		tStruct = ::logging::Logger::formatLog (log4cpp::Priority::Debug, text, param); \
		LOG(tStruct.priority, routine, tStruct.message); \
	}

#define ACS_STATIC_DEBUG_PARAM(routine, text, param) \
	{ \
		logging::BasicLogInfo tStruct; \
		tStruct = ::logging::Logger::formatLog (log4cpp::Priority::Debug, text, param); \
		STATIC_LOG(tStruct.priority, routine, tStruct.message); \
	}

#define ACS_TRACE(routine) \
		LOG(log4cpp::Priority::TRACE, routine, FIELD_UNAVAILABLE);

#define ACS_STATIC_TRACE(routine) \
		STATIC_LOG(log4cpp::Priority::Trace, routine, FIELD_UNAVAILABLE);

#define ACS_SHORT_LOG(X) \
	{ \
		logging::BasicLogInfo tStruct; \
		tStruct = ::logging::Logger::formatLog X; \
		LOG(tStruct.priority, FIELD_UNAVAILABLE, tStruct.message); \
	}

#define ACS_STATIC_SHORT_LOG(X) \
	{ \
		logging::BasicLogInfo tStruct; \
		tStruct = ::logging::Logger::formatLog X; \
		STATIC_LOG(tStruct.priority, FIELD_UNAVAILABLE, tStruct.message); \
	}

#define ACS_LOG(flags, routine, X) \
	{\
		logging::BasicLogInfo tStruct; \
		tStruct = ::logging::Logger::formatLog X; \
		LOG(tStruct.priority, routine, tStruct.message); \
	}

#define ACS_STATIC_LOG(flags, routine, X) \
	{\
		logging::BasicLogInfo tStruct; \
		tStruct = ::logging::Logger::formatLog X; \
		STATIC_LOG(tStruct.priority, routine, tStruct.message); \
	}
#endif

#endif
