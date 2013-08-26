#ifndef acstestcompImpl_h
#define acstestcompImpl_h

/*
 * "@(#) $Id: acstestcompTimingExplorerImpl.h,v 1.4 2008/10/09 08:07:23 cparedes Exp $"
 *
 * $Log: acstestcompTimingExplorerImpl.h,v $
 * Revision 1.4  2008/10/09 08:07:23  cparedes
 * Remove cpp exception declarations
 *
 * Revision 1.3  2008/10/01 05:33:43  cparedes
 * Removing exception declaration from cpp implementation
 *
 * Revision 1.2  2008/07/25 07:36:17  cparedes
 * Removing use namespace from included files and updating
 * the files where the use namespace was assumed
 *
 * Revision 1.1  2006/09/28 16:02:57  gchiozzi
 * Added second test component.
 * Some cleanup and refactoring.
 *
 * Revision 1.1  2006/09/14 14:54:34  gchiozzi
 * First checkin of new module with CPP test components.
 *
 *
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acscomponentImpl.h>

#include <acstestcompS.h>
#include <maciContainerServices.h>

/**
 * Implementation class in CPP for the TimingExplorer
 * IDL interface.
 */
class TimingExplorerImpl : public virtual acscomponent::ACSComponentImpl,
			   public virtual POA_acstestcomp::TimingExplorer

{
  public:
    /**
     * Constructor
     * @param containerServices ContainerServices which are needed for 
     * various component related methods.
     * @param name component name
     */
    TimingExplorerImpl(const ACE_CString& name, 
		       maci::ContainerServices* containerServices);

    /**
     * Destructor
     */
    virtual ~TimingExplorerImpl() {};

		
/* ------------------- [ Lifecycle START interface ] --------------------- */

    /**
     * Lifecycle method called at component initialization state.
     * What is peculiar of this implementation is that it checks
     * if the name of the component is "HangOnInit". In this case
     * it hangs forever.
     * @throw acsErrTypeLifeCycle::acsErrTypeLifeCycleExImpl
     * This is used to make tests on initialisation problems.
     */
    virtual void initialize(void);

/* ------------------- [ Lifecycle END interface ] --------------- */

/* --------------------- [ CORBA START interface ] ----------------*/

    /**
     * This method waits for the given number of seconds, then 
     * sends a reply
     * @param waitTimeSec time to wait in seconds before sending reply
     */
    virtual void waitToReply(CORBA::Long waitTimeSec) ;

    /**
     * Sends a number over replies evenly
     * distributed in time.
     * @param repetitions number of replies to send
     *                    0 means reply forever.
     * @param waitTimeSec time to wait before
     *                    sending a reply.
     *                    Units are seconds.
     *                    0 means never reply.
     * @param cb          Callback object for the replies
     * @param desc        Input parameters sent by the caller
     */
    virtual void multipleReplies(CORBA::Long repetitions, CORBA::Long waitTimeSec, ACS::CBvoid_ptr cb, const ACS::CBDescIn& desc);


/* --------------------- [ CORBA END interface ] ----------------*/

/* ----------------------------------------------------------------*/

  private:
    /**
     * This is a counter for how many times multipleReplies()
     * has been called
     * In this way we can keep track of each invocation.
     */
    int multipleRepliesCounter_m;

    /**
     * Copy constructor is not allowed following the ACS desgin rules.
     */
    TimingExplorerImpl(const TimingExplorerImpl&);

    /**
     * Assignment operator is not allowed due to ACS design rules.
     */
    void operator=(const TimingExplorerImpl&);


/* --------------------- [ Constants START ] ----------------------*/

};


#endif /* acstestcompImpl_h */

