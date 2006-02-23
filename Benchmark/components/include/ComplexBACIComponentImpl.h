#ifndef ComplexBACIComponentImpl_h
#define ComplexBACIComponentImpl_h

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <BasePerfCompImpl.h>

#include "perftestS.h"

#include <baciRWlong.h>
#include <baciSmartPropertyPointer.h>


#define ACTION 0

class ComplexBACIComponent: public BasePerfCompImpl,     //Standard component superclass
			    public virtual POA_perftest::ComplexBACIComponent,    //CORBA servant stub
			    public ActionImplementator    //Asynchronous method helper class
{
  public:

    ComplexBACIComponent(const ACE_CString& name,
			 maci::ContainerServices *containerServices);
  

    virtual ~ComplexBACIComponent();  
    

    virtual ActionRequest invokeAction(int function,
		  BACIComponent *component_p, 
		  const int &callbackID, 
		  const CBDescIn &descIn, 
		  BACIValue *value_p, 
		  Completion &completion, 
		  CBDescOut &descOut);
    
    /* --------------------- [ CORBA interface ] ----------------------*/

    virtual void action01(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action02(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action03(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action04(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action05(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action06(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action07(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action08(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action09(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action10(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action11(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action12(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action13(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action14(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action15(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    virtual void action16(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException);
    
    virtual ACS::RWlong_ptr property01() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property02() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property03() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property04() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property05() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property06() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property07() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property08() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property09() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property10() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property11() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property12() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property13() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property14() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property15() throw (CORBA::SystemException);
    virtual ACS::RWlong_ptr property16() throw (CORBA::SystemException);

  private:
    SmartPropertyPointer<RWlong>m_property01;
    SmartPropertyPointer<RWlong>m_property02;
    SmartPropertyPointer<RWlong>m_property03;
    SmartPropertyPointer<RWlong>m_property04;
    SmartPropertyPointer<RWlong>m_property05;
    SmartPropertyPointer<RWlong>m_property06;
    SmartPropertyPointer<RWlong>m_property07;
    SmartPropertyPointer<RWlong>m_property08;
    SmartPropertyPointer<RWlong>m_property09;
    SmartPropertyPointer<RWlong>m_property10;
    SmartPropertyPointer<RWlong>m_property11;
    SmartPropertyPointer<RWlong>m_property12;
    SmartPropertyPointer<RWlong>m_property13;
    SmartPropertyPointer<RWlong>m_property14;
    SmartPropertyPointer<RWlong>m_property15;
    SmartPropertyPointer<RWlong>m_property16;

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const ComplexBACIComponent&);
};

#endif   /* ComplexBACIComponentImpl_h */
