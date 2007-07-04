/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) Associated Universities Inc., 2007 
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
 * "@(#) $Id: acsncServiceChecker.cpp,v 1.1 2007/07/04 13:08:23 nbarriga Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * nbarriga  2007-07-04  created 
 */

/************************************************************************
 *   NAME
 *   
 * 
 *   SYNOPSIS
 *   
 * 
 *   DESCRIPTION
 *
 *   FILES
 *
 *   ENVIRONMENT
 *
 *   COMMANDS
 *
 *   RETURN VALUES
 *
 *   CAUTIONS 
 *
 *   EXAMPLES
 *
 *   SEE ALSO
 *
 *   BUGS   
 * 
 *------------------------------------------------------------------------
 */

#include <maciSimpleClient.h>
#include <acsncHelper.h>

class ServiceChecker: public nc::Helper{
        public:
                ServiceChecker(CORBA::ORB_ptr orb):
                        nc::Helper("DummyChannelName"){
                                try{
                                        resolveNamingService(orb);
                                }catch(CORBAProblemEx &ex){
                                        CORBAProblemExImpl ex2(ex);
                                        ex2.log();
                                        return;
                                }catch(CouldntCreateThreadEx &ex){
                                        CouldntCreateThreadExImpl ex2(ex);
                                        ex2.log();
                                        return;
                                }
                        }
                bool check(const string domain){
                        string factoryName;
                        try{
                                if(domain.compare(acscommon::ALMADOMAIN)==0){
                                        factoryName=acscommon::NOTIFICATION_FACTORY_NAME;
                                }else if(domain.compare(acscommon::ARCHIVING_DOMAIN)==0){
                                        factoryName=acscommon::ARCHIVE_NOTIFICATION_FACTORY_NAME;
                                }else if(domain.compare(acscommon::LOGGING_DOMAIN)==0){
                                        factoryName=acscommon::LOGGING_NOTIFICATION_FACTORY_NAME;
                                }else{
                                        ACS_SHORT_LOG((LM_NOTICE,"%s notification service domain unknown",domain.c_str()));
                                        return false;
                                }
                                resolveNotificationFactory(factoryName);
                        }catch(CORBAProblemEx &ex){
                                ACS_SHORT_LOG((LM_CRITICAL,"%s notification service unavailable",domain.c_str()));
                                return false;
                        }
                        return true;
                }
        private:
                void resolveNotificationFactory(const string factoryName)
                        throw (CORBAProblemEx)
                        {
                                ACS_TRACE("ServiceChecker::resolveNotificationFactory");

                                CosNaming::Name name(1);
                                name.length(1);
                                name[0].id = factoryName.c_str();

                                //first a simple sanity check to ensure the naming service is up and running
                                if(CORBA::is_nil(namingContext_m.in()) == true)
                                {
                                        CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"ServiceChecker::resolveNotificationFactory");
                                        throw err.getCORBAProblemEx();
                                }

                                try
                                {
                                        //try to resolve the object with the naming service.  a few exceptions can be
                                        //thrown by this
                                        CORBA::Object_var corbaObj = namingContext_m->resolve(name);
                                        //double-check to ensure it's not a nil reference
                                        if(CORBA::is_nil(corbaObj.in()) == true)
                                        {
                                                CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"ServiceChecker::resolveNotificationFactory");
                                                throw err.getCORBAProblemEx();
                                        }
                                        //now try to narrow the notification service reference
                                        notifyFactory_m = CosNotifyChannelAdmin::EventChannelFactory::_narrow(corbaObj.in());
                                        //double-check to ensure it's not a nil reference
                                        if(CORBA::is_nil(notifyFactory_m.in()) == true)
                                        {
                                                CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"ServiceChecker::resolveNotificationFactory");
                                                throw err.getCORBAProblemEx();
                                        }
                                }
                                catch(...)
                                {
                                        //most likely some exception like the notification service is not registered
                                        //with the naming service.  nothing can be done
                                        CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"ServiceChecker::resolveNotificationFactory");
                                        throw err.getCORBAProblemEx();
                                }
                        }


};
int main(int argc, char *argv[])
{
        SimpleClient client;

        // Creates and initializes the SimpleClient object

        if (client.init(argc,argv) == 0)
        {
                return -1;
        }
        else
        {
                //Must log into manager before we can really do anything
                client.login();
        }

        ServiceChecker SC(client.getORB());
        while(1){
                if(SC.check(acscommon::ALMADOMAIN))ACS_SHORT_LOG((LM_DEBUG,"%s notification service operational",acscommon::ALMADOMAIN));
                if(SC.check(acscommon::ARCHIVING_DOMAIN))ACS_SHORT_LOG((LM_DEBUG,"%s notification service operational",acscommon::ARCHIVING_DOMAIN));
                if(SC.check(acscommon::LOGGING_DOMAIN))ACS_SHORT_LOG((LM_DEBUG,"%s notification service operational",acscommon::LOGGING_DOMAIN));
                ACE_OS::sleep(5);
        }
        client.disconnect();
        return 0;
}
