#ifndef IDL_COMPILATION_TIME_COMPONENT_IMPL_H
#define IDL_COMPILATION_TIME_COMPONENT_IMPL_H

//Simple IDL file used to test IDL compilation time.
//Do not add or remove anything from this file!

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciCharacteristicComponentImpl.h>
#include <IdlCompilationTimeS.h>
#include <baciRWlong.h>

/**
 * One of these function IDs will be passed to invokeAction().
 */
#define ACTION 0

class IdlCompilationTimeComponentImpl: public CharacteristicComponentImpl,     //Standard component superclass
			   public virtual POA_IdlCompilationTime::IdlCompilationTimeComponent,    //CORBA servant stub
			   public ActionImplementator    //Asynchronous method helper class
{
  public:
    IdlCompilationTimeComponentImpl(const ACE_CString& name,
			maci::ContainerServices *containerServices);
    virtual ~IdlCompilationComponentTimeImpl();  
    
    virtual ActionRequest invokeAction(int function,
				       BACIComponent *component_p, 
				       const int &callbackID, 
				       const CBDescIn &descIn, 
				       BACIValue *value_p, 
				       Completion &completion, 
				       CBDescOut &descOut);
    
    virtual void method();
    virtual void action(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual ACS::RWlong_ptr property();

  private:
    RWlong	*m_property;
    void operator=(const IdlCompilationTimeComponentImpl&);
};

#endif
