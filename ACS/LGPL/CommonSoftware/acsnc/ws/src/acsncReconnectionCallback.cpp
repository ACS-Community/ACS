#include <orbsvcs/NotifyExtC.h>
#include <iostream>

#include <maciContainerImpl.h>
#include <acsContainerServices.h>

#include "acsncReconnectionCallback.h"

using namespace nc;

ReconnectionCallback::ReconnectionCallback(nc::Helper *sub):
   sub_(sub),
   id_is_valid_(false),
   root_poa_(0),
   services_(0)
{
   if (::maci::ContainerImpl::getContainer() != NULL)
      services_ = ::maci::ContainerImpl::getContainer()->getContainerServices();
}

bool ReconnectionCallback::is_alive()
{
   return true;
}

void ReconnectionCallback::reconnect(::CORBA::Object_ptr new_connection)
{
   ecf_ = NotifyMonitoringExt::EventChannelFactory::_narrow(new_connection);
   if(!::CORBA::is_nil(ecf_))
      sub_->reconnect(ecf_);
}

void ReconnectionCallback::init( CORBA::ORB_ptr orb_mp,
      NotifyMonitoringExt::EventChannelFactory_ptr ecf)
{
   if (::CORBA::is_nil(ecf)){
	  std::cout << "-- ECF is nil :( --" << std::endl;
      return;
   }

   ecf_ =  NotifyMonitoringExt::EventChannelFactory::_duplicate(ecf);
   NotifyExt::ReconnectionCallback_var callback;

   if (!::CORBA::is_nil(orb_mp)){
        ::CORBA::Object_ptr poa = orb_mp->resolve_initial_references("RootPOA");
        root_poa_ = PortableServer::POA::_narrow (poa);
        callback_obj_id_ = root_poa_->activate_object(this);

        CORBA::Object_var obj =
        root_poa_->id_to_reference(callback_obj_id_.in());
        callback = NotifyExt::ReconnectionCallback::_narrow(obj);
   }
   else if (services_ != NULL){
      callback = NotifyExt::ReconnectionCallback::_narrow(
            services_->activateOffShoot(this));
   }
   else
      return;

   if (::CORBA::is_nil(callback))
      std::cout << "Callback not initializated" << std::endl;

   NotifyExt::ReconnectionRegistry_var registry =
      NotifyExt::ReconnectionRegistry::_narrow(ecf_);

   callback_id_ = registry->register_callback(callback);
   id_is_valid_ = true;

}

void ReconnectionCallback::disconnect()
{
   if (id_is_valid_){
      NotifyExt::ReconnectionRegistry_var registry =
         NotifyExt::ReconnectionRegistry::_narrow(ecf_);
      registry->unregister_callback(callback_id_);
      if (!::CORBA::is_nil(root_poa_))
         root_poa_->deactivate_object(callback_obj_id_);
      else
        services_->deactivateOffShoot(this);
      id_is_valid_ = false;
   }
}

ReconnectionCallback::~ReconnectionCallback()
{
}
