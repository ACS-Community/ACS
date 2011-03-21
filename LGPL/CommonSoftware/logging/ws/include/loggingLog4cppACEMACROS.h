#ifndef LOGGING_LOG4CPP_ACEMACROS_H_
#define LOGGING_LOG4CPP_ACEMACROS_H_


#include "loggingLog4cppMACROS.h"
#include "loggingLog4cpp.h"

#define FIELD_UNAVAILABLE "Unavailable"

#define LOG4CPP_ACS_DEBUG(routine, text) \
		LOG4CPP_LOG(log4cpp::Priority::Debug, routine, text);

#define LOG4CPP_ACS_STATIC_DEBUG(routine, text) \
		LOG4CPP_STATIC_LOG(log4cpp::Priority::Debug, routine. text);

#define LOG4CPP_ACS_DEBUG_PARAM(routine, text, param) \
	{ \
		logging::BasicLogInfo tStruct; \
		tStruct = ::logging::Logger::formatLog (log4cpp::Priority::Debug, text, param); \
		LOG4CPP_LOG(tStruct.priority, routine, tStruct.message); \
	}

#define LOG4CPP_ACS_STATIC_DEBUG_PARAM(routine, text, param) \
	{ \
		logging::BasicLogInfo tStruct; \
		tStruct = ::logging::Logger::formatLog (log4cpp::Priority::Debug, text, param); \
		LOG4CPP_STATIC_LOG(tStruct.priority, routine, tStruct.message); \
	}

#define LOG4CPP_ACS_TRACE(routine) \
		LOG4CPP_LOG(log4cpp::Priority::Trace, routine, FIELD_UNAVAILABLE);

#define LOG4CPP_ACS_STATIC_TRACE(routine) \
		LOG4CPP_STATIC_LOG(log4cpp::Priority::Trace, routine, FIELD_UNAVAILABLE);

#define LOG4CPP_ACS_SHORT_LOG(X) \
	{ \
		logging::BasicLogInfo tStruct; \
		tStruct = ::logging::Logger::formatLog X; \
		LOG4CPP_LOG(tStruct.priority, FIELD_UNAVAILABLE, tStruct.message); \
	}

#define LOG4CPP_ACS_STATIC_SHORT_LOG(X) \
	{ \
		logging::BasicLogInfo tStruct; \
		tStruct = ::logging::Logger::formatLog X; \
		LOG4CPP_STATIC_LOG(tStruct.priority, FIELD_UNAVAILABLE, tStruct.message); \
	}

#define LOG4CPP_ACS_LOG(flags, routine, X) \
	{\
		logging::BasicLogInfo tStruct; \
		tStruct = ::logging::Logger::formatLog X; \
		LOG4CPP_LOG(tStruct.priority, routine, tStruct.message); \
	}

#define LOG4CPP_ACS_STATIC_LOG(flags, routine, X) \
	{\
		logging::BasicLogInfo tStruct; \
		tStruct = ::logging::Logger::formatLog X; \
		LOG4CPP_STATIC_LOG(tStruct.priority, routine, tStruct.message); \
	}
#endif
