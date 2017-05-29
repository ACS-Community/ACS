#include "QoSProps.h"
#include <sstream>
#include <stdexcept>
#include <orbsvcs/Notify/Notify_Extensions.h>

QoSProps::QoSProps(const std::string &config)
{
    parseProperties(config);
}

QoSProps::~QoSProps()
{}

const CosNotification::QoSProperties& QoSProps::getEventProps() const
{
    return eventProps;
}

const CosNotification::QoSProperties& QoSProps::getProxyProps() const
{
    return proxyProps;
}

const CosNotification::QoSProperties& QoSProps::getAdminProps() const
{
    return adminProps;
}

const CosNotification::QoSProperties& QoSProps::getChannelQoSProps() const
{
    return channelQoSProps;
}

const CosNotification::AdminProperties& QoSProps::getChannelAdminProps() const
{
    return channelAdminProps;
}

void QoSProps::parseProperties(const std::string &config)
{
    std::vector<std::string> parts;
    parseItems(config, ';', parts);

    for(uint32_t i = 0;i < parts.size();++i)
    {
        std::vector<std::string> fieldProps;
        std::vector<std::string> props;
        parseItems(parts[i], ':', fieldProps);

        if(fieldProps.size() == 2)
        {
            parseItems(fieldProps[1], ',', props);

            CosNotification::PropertySeq *qosProps = NULL;
            if (fieldProps[0] == "Event")
            {
                qosProps = &eventProps;
            } else if(fieldProps[0] == "Proxy") {
                qosProps = &proxyProps;
            } else if(fieldProps[0] == "Admin") {
                qosProps = &adminProps;
            } else if(fieldProps[0] == "ChannelQoS") {
                qosProps = &channelQoSProps;
            } else if(fieldProps[0] == "ChannelAdmin") {
                qosProps = &channelAdminProps;
            } else {
                throw std::runtime_error(std::string("Wrong QoS level: ") + fieldProps[0]);
            }

            getProperties(props, '=', *qosProps);

        } else {
            throw std::runtime_error("Wrong QoS properties format");
        }
    }
}

void QoSProps::parseItems(const std::string &str,char sep,std::vector<std::string> &items)
{
    std::string s = str;
    std::size_t pos = s.find_first_of(sep);
    while(pos != std::string::npos)
    {
        items.push_back(s.substr(0, pos));
        s = s.substr(pos+1);
        pos = s.find_first_of(sep);
    }
    if(s.size() > 0)
    {
        items.push_back(s);
    }
}

void QoSProps::getProperties(const std::vector<std::string> &propsList,char sep,CosNotification::PropertySeq &props)
{
    std::size_t pos;
    std::string field;
    std::string value;
    //std::string s = strProps;

    props.length(propsList.size());

    for(uint32_t i = 0;i < propsList.size();++i)
    {
        pos = propsList[i].find_first_of(sep);
        if(pos == std::string::npos)
        {
            throw std::runtime_error("Wrong QoS properties format");
        } else {
            field = propsList[i].substr(0, pos);
            value = propsList[i].substr(pos + 1);

            if(field == TAO_Notify_Extensions::BlockingPolicy)
            {
                props[i].name = CORBA::string_dup(TAO_Notify_Extensions::BlockingPolicy);
                uint64_t blockingPolicy = 0;
                std::stringstream(value) >> blockingPolicy;
                props[i].value <<= blockingPolicy;
                ACE_DEBUG((LM_INFO, "%T QoS BlockingPolicy: %ju\n", blockingPolicy));

            } else if(field == CosNotification::ConnectionReliability) {
                props[i].name = CORBA::string_dup(CosNotification::ConnectionReliability);
                if(value == "BestEffort")
                {
                    props[i].value <<= CosNotification::BestEffort;
                    ACE_DEBUG((LM_INFO, "%T QoS ConnectionReliability: BestEffort\n"));
                } else if(value == "Persistent") {
                    props[i].value <<= CosNotification::Persistent;
                    ACE_DEBUG((LM_INFO, "%T QoS ConnectionReliability: Persistent\n"));
                } else {
                    throw std::runtime_error("Wrong ConnectionReliability value");
                }

            } else if(field == CosNotification::DiscardPolicy) {
                props[i].name = CORBA::string_dup(CosNotification::DiscardPolicy);
                if(value == "AnyOrder") {
                    props[i].value <<= CosNotification::AnyOrder;
                    ACE_DEBUG((LM_INFO, "%T QoS DiscardPolicy: AnyOrder\n"));
                } else if(value == "FifoOrder") {
                    props[i].value <<= CosNotification::FifoOrder;
                    ACE_DEBUG((LM_INFO, "%T QoS DiscardPolicy: FifoOrder\n"));
                } else if(value == "LifoOrder") {
                    props[i].value <<= CosNotification::LifoOrder;
                    ACE_DEBUG((LM_INFO, "%T QoS DiscardPolicy: LifeOrder\n"));
                } else if(value == "PriorityOrder") {
                    props[i].value <<= CosNotification::PriorityOrder;
                    ACE_DEBUG((LM_INFO, "%T QoS DiscardPolicy: PriorityOrder\n"));
                } else if(value == "DeadlineOrder") {
                    props[i].value <<= CosNotification::DeadlineOrder;
                    ACE_DEBUG((LM_INFO, "%T QoS DiscardPolicy: DeadlineOrder\n"));
                } else {
                    throw std::runtime_error("Wrong DiscardPolicy value");
                }
            } else if(field == CosNotification::EventReliability) {
                props[i].name = CORBA::string_dup(CosNotification::EventReliability);
                if(value == "BestEffort")
                {
                    props[i].value <<= CosNotification::BestEffort;
                    ACE_DEBUG((LM_INFO, "%T QoS EventReliability: BestEffort\n"));
                } else if(value == "Persistent") {
                    props[i].value <<= CosNotification::Persistent;
                    ACE_DEBUG((LM_INFO, "%T QoS EventReliability: Persistent\n"));
                } else {
                    throw std::runtime_error("Wrong EventReliability value");
                }
               
            } else if(field == CosNotification::MaxConsumers) {
                props[i].name = CORBA::string_dup(CosNotification::MaxConsumers);
                int32_t maxConsumers = 0;
                std::stringstream(value) >> maxConsumers;
                props[i].value <<= maxConsumers;
                ACE_DEBUG((LM_INFO, "%T QoS MaxConsumers: %d\n", maxConsumers));

            } else if(field == CosNotification::MaxEventsPerConsumer) {
                props[i].name = CORBA::string_dup(CosNotification::MaxEventsPerConsumer);
                int32_t maxEventsPerConsumer = 0;
                std::stringstream(value) >> maxEventsPerConsumer;
                props[i].value <<= maxEventsPerConsumer;
                ACE_DEBUG((LM_INFO, "%T QoS MaxEventsPerConsumer: %d\n", maxEventsPerConsumer));

            } else if(field == CosNotification::MaximumBatchSize) {
                props[i].name = CORBA::string_dup(CosNotification::MaximumBatchSize);
                int32_t maximumBatchSize = 0;
                std::stringstream(value) >> maximumBatchSize;
                props[i].value <<= maximumBatchSize;
                ACE_DEBUG((LM_INFO, "%T QoS MaximumBatchSize: %d\n", maximumBatchSize));

            } else if(field == CosNotification::MaxQueueLength) {
                props[i].name = CORBA::string_dup(CosNotification::MaxQueueLength);
                int32_t maxQueueLength = 0;
                std::stringstream(value) >> maxQueueLength;
                props[i].value <<= maxQueueLength;
                ACE_DEBUG((LM_INFO, "%T QoS MaxQueueLength: %d\n", maxQueueLength));

            } else if(field == CosNotification::MaxSuppliers) {
                props[i].name = CORBA::string_dup(CosNotification::MaxSuppliers);
                int32_t maxSuppliers = 0;
                std::stringstream(value) >> maxSuppliers;
                props[i].value <<= maxSuppliers;
                ACE_DEBUG((LM_INFO, "%T QoS MaxSuppliers: %d\n", maxSuppliers));

            } else if(field == CosNotification::OrderPolicy) {
                props[i].name = CORBA::string_dup(CosNotification::OrderPolicy);
                if(value == "AnyOrder") {
                    props[i].value <<= CosNotification::AnyOrder;
                    ACE_DEBUG((LM_INFO, "%T QoS OrderPolicy: AnyOrder\n"));
                } else if(value == "FifoOrder") {
                    props[i].value <<= CosNotification::FifoOrder;
                    ACE_DEBUG((LM_INFO, "%T QoS OrderPolicy: FifoOrder\n"));
                } else if(value == "LifoOrder") {
                    props[i].value <<= CosNotification::LifoOrder;
                    ACE_DEBUG((LM_INFO, "%T QoS OrderPolicy: LifeOrder\n"));
                } else if(value == "PriorityOrder") {
                    props[i].value <<= CosNotification::PriorityOrder;
                    ACE_DEBUG((LM_INFO, "%T QoS OrderPolicy: PriorityOrder\n"));
                } else if(value == "DeadlineOrder") {
                    props[i].value <<= CosNotification::DeadlineOrder;
                    ACE_DEBUG((LM_INFO, "%T QoS OrderPolicy: DeadlineOrder\n"));
                } else {
                    throw std::runtime_error("Wrong OrderPolicy value");
                }              

            } else if(field == CosNotification::PacingInterval) {
                props[i].name = CORBA::string_dup(CosNotification::PacingInterval);
                uint64_t pacingInterval = 0;
                std::stringstream(value) >> pacingInterval;
                props[i].value <<= pacingInterval;
                ACE_DEBUG((LM_INFO, "%T QoS PacingInterval: %ju\n", pacingInterval));

            } else if(field == CosNotification::Priority) {
                props[i].name = CORBA::string_dup(CosNotification::Priority);
                int32_t priority = 0;
                std::stringstream(value) >> priority;
                props[i].value <<= priority;
                ACE_DEBUG((LM_INFO, "%T QoS Priority: %d\n", priority));

            } else if(field == CosNotification::RejectNewEvents) {
                props[i].name = CORBA::string_dup(CosNotification::RejectNewEvents);
                props[i].value <<= getBooleanValue(value);
                ACE_DEBUG((LM_INFO, "%T QoS RejectNewEvents: %d\n", getBooleanValue(value)));

            } else if(field == CosNotification::Timeout) {
                props[i].name = CORBA::string_dup(CosNotification::Timeout);
                uint64_t timeout = 0;
                std::stringstream(value) >> timeout;
                props[i].value <<= timeout;
                ACE_DEBUG((LM_INFO, "%T QoS Timeout: %ju\n", timeout));
            } else {
                throw std::runtime_error(std::string("Unknown QoS property: ") + field);
            }
        }

    }
}


bool QoSProps::getBooleanValue(const std::string &value)
{
    if(value == "1" || value == "true" || value == "t")
    {
        return true;
    } 
    return false;
}
