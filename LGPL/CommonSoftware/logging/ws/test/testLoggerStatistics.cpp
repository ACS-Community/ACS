#include <iostream>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCase.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestCaller.h>
#include <cppunit/ui/text/TestRunner.h>

#include "loggingACSLogger.h"
#include "loggingHandler.h"
#include "logging.h"

class LoggingUnitTest: public CppUnit::TestFixture {
    public:
      static CppUnit::Test* suite();
    
      LoggingUnitTest(): CppUnit::TestFixture() {}
      LoggingUnitTest(const LoggingUnitTest& toCopy) { *this = toCopy; }

      ~LoggingUnitTest() {}

      LoggingUnitTest& operator = (const LoggingUnitTest&){ return *this; } 

      void setUp(void);
      void tearDown(void);

      void checkDefaultStatusTest(void);
    private:
      LoggingProxy* m_logger;
};

void LoggingUnitTest::setUp(void) {
    ACS_CHECK_LOGGER;

    this->m_logger = new LoggingProxy(5, 4, 8, 0, 0, 5);
    LoggingProxy::init (this->m_logger);
}

void LoggingUnitTest::tearDown(void) {
    LoggingProxy::done();
    delete this->m_logger;
}

void LoggingUnitTest::checkDefaultStatusTest(void) {
    Logging::Logger::LoggerSmartPtr testLoggerSmartPtr = getLogger();
    testLoggerSmartPtr->setName("testLogger");
    
    if(getenv("LOCATION")) { CPPUNIT_ASSERT(!testLoggerSmartPtr->stats.getDisableStatistics()); }
    else { CPPUNIT_ASSERT(testLoggerSmartPtr->stats.getDisableStatistics()); }
}

CppUnit::Test* LoggingUnitTest::suite() {
    CppUnit::TestSuite *testSuite = new CppUnit::TestSuite("LoggingUnitTest");

    testSuite->addTest(new CppUnit::TestCaller<LoggingUnitTest>("checkDefaultStatusTest", &LoggingUnitTest::checkDefaultStatusTest));

    return testSuite;
}

int main(int argc, char const* argv[]) {
    CppUnit::TextUi::TestRunner runner;
    runner.addTest(LoggingUnitTest::suite());
    runner.run();

    return 0;
}
