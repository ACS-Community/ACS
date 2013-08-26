#ifndef RECONNECTION_CALLBACK_H
#define RECONNECTION_CALLBACK_H

#include <orbsvcs/CosNotificationC.h>
#include <orbsvcs/Notify/MonitorControlExt/NotifyMonitoringExtC.h>

#include <acsncS.h>

#include "acsncHelper.h"

namespace nc{

   class Helper;

   class ReconnectionCallback: virtual public POA_acsnc::OSReconnectionCallback
   {
      public:
         ReconnectionCallback(nc::Helper *sub);
         ~ReconnectionCallback();
         bool is_alive();
         void reconnect(::CORBA::Object_ptr new_connection);
         void init(CORBA::ORB_ptr orb,
               NotifyMonitoringExt::EventChannelFactory_ptr ecf);
         void disconnect();
      
      private: 
         NotifyMonitoringExt::EventChannelFactory_ptr ecf_;
         nc::Helper *sub_;
         int callback_id_;
         bool id_is_valid_;
         PortableServer::POA_var root_poa_;
         PortableServer::ObjectId_var callback_obj_id_;

		protected:
         maci::ContainerServices *services_;

		friend class Consumer;
   };
};
#endif
