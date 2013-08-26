#ifndef SimpleBACIComponentImpl_h
#define SimpleBACIComponentImpl_h

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "BasePerfCompImpl.h"
#include "perftestS.h"

#include <baciSmartPropertyPointer.h>
#include <baciRWlong.h>

#define ACTION 0

class SimpleBACIComponent: public virtual BasePerfCompImpl,     
			   public virtual POA_perftest::SimpleBACIComponent,
			   public baci::ActionImplementator
{
  public:
    
    SimpleBACIComponent(const ACE_CString& name,
    maci::ContainerServices *containerServices);
    virtual ~SimpleBACIComponent();  
    
    virtual baci::ActionRequest 
    invokeAction(int function,
		 baci::BACIComponent *component_p, 
		 const int &callbackID, 
		 const CBDescIn &descIn, 
		 baci::BACIValue *value_p, 
		 Completion &completion, 
		 CBDescOut &descOut);
    
    virtual void 
    action(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    
    
    virtual ACS::RWlong_ptr 
    property();

  private:

    baci::SmartPropertyPointer<baci::RWlong>m_property_sp;

    void operator=(const SimpleBACIComponent&);
};

#endif   /* SimpleBACIComponentImpl_h */
