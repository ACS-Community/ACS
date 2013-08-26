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
			    public baci::ActionImplementator    //Asynchronous method helper class
{
  public:

    ComplexBACIComponent(const ACE_CString& name,
			 maci::ContainerServices *containerServices);
  

    virtual ~ComplexBACIComponent();  
    

    virtual baci::ActionRequest invokeAction(int function,
		  baci::BACIComponent *component_p, 
		  const int &callbackID, 
		  const CBDescIn &descIn, 
		  baci::BACIValue *value_p, 
		  Completion &completion, 
		  CBDescOut &descOut);
    
    /* --------------------- [ CORBA interface ] ----------------------*/

    virtual void action01(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action02(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action03(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action04(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action05(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action06(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action07(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action08(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action09(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action10(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action11(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action12(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action13(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action14(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action15(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    virtual void action16(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc);
    
    virtual ACS::RWlong_ptr property01();
    virtual ACS::RWlong_ptr property02();
    virtual ACS::RWlong_ptr property03();
    virtual ACS::RWlong_ptr property04();
    virtual ACS::RWlong_ptr property05();
    virtual ACS::RWlong_ptr property06();
    virtual ACS::RWlong_ptr property07();
    virtual ACS::RWlong_ptr property08();
    virtual ACS::RWlong_ptr property09();
    virtual ACS::RWlong_ptr property10();
    virtual ACS::RWlong_ptr property11();
    virtual ACS::RWlong_ptr property12();
    virtual ACS::RWlong_ptr property13();
    virtual ACS::RWlong_ptr property14();
    virtual ACS::RWlong_ptr property15();
    virtual ACS::RWlong_ptr property16();

  private:
    baci::SmartPropertyPointer<baci::RWlong>m_property01;
    baci::SmartPropertyPointer<baci::RWlong>m_property02;
    baci::SmartPropertyPointer<baci::RWlong>m_property03;
    baci::SmartPropertyPointer<baci::RWlong>m_property04;
    baci::SmartPropertyPointer<baci::RWlong>m_property05;
    baci::SmartPropertyPointer<baci::RWlong>m_property06;
    baci::SmartPropertyPointer<baci::RWlong>m_property07;
    baci::SmartPropertyPointer<baci::RWlong>m_property08;
    baci::SmartPropertyPointer<baci::RWlong>m_property09;
    baci::SmartPropertyPointer<baci::RWlong>m_property10;
    baci::SmartPropertyPointer<baci::RWlong>m_property11;
    baci::SmartPropertyPointer<baci::RWlong>m_property12;
    baci::SmartPropertyPointer<baci::RWlong>m_property13;
    baci::SmartPropertyPointer<baci::RWlong>m_property14;
    baci::SmartPropertyPointer<baci::RWlong>m_property15;
    baci::SmartPropertyPointer<baci::RWlong>m_property16;

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const ComplexBACIComponent&);
};

#endif   /* ComplexBACIComponentImpl_h */
