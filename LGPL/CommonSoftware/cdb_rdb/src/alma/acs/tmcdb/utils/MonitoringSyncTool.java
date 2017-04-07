/*
 * ALMA - Atacama Large Millimeter Array
 * (c) European Southern Observatory, 2002
 * (c) Associated Universities Inc., 2002
 * Copyright by ESO (in the framework of the ALMA collaboration),
 * Copyright by AUI (in the framework of the ALMA collaboration),
 * All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY, without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307  USA
 *
 * "@(#) $Id: AssemblyDataLoader.java,v 1.18 2011/03/04 21:21:29 sharring Exp $"
 */
package alma.tmcdb.utils;


import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.exolab.castor.xml.XMLException;
import org.hibernate.type.StringType;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.Transaction;

import alma.acs.tmcdb.Assembly;
import alma.acs.tmcdb.AssemblyType;
import alma.acs.tmcdb.BACIPropArchMech;
import alma.acs.tmcdb.BACIProperty;
import alma.acs.tmcdb.Component;
import alma.acs.tmcdb.ComponentType;
import alma.acs.tmcdb.Configuration;
import alma.acs.tmcdb.HWConfiguration;
import alma.acs.tmcdb.LRUType;
import alma.acs.tmcdb.MonitorPoint;
import alma.acs.tmcdb.MonitorPointDatatype;
import alma.archive.database.helpers.wrappers.DbConfigException;
import alma.archive.database.helpers.wrappers.TmcdbDbConfig;
import alma.tmcdb.generated.lrutype.BaciPropertyT;
import alma.tmcdb.generated.lrutype.LruType;
import alma.tmcdb.generated.lrutype.MonitorPointT;

public class MonitoringSyncTool {

    private static final String TMCDB_CONFIGURATION_NAME = "TMCDB_CONFIGURATION_NAME";

    private static Logger logger =
    	TmcdbLoggerFactory.getLogger(MonitoringSyncTool.class.getName());
        
    private class AttChange {
        
        private String attName;
        
        private String originalValue;
        
        private String newValue;
        
        public AttChange(String attName, String originalValue,
                String newValue) {
            this.attName = attName;
            this.originalValue = originalValue;
            this.newValue = newValue;
        }
        
        public String getAttName() {
            return attName;
        }
        
        public void setAttName(String attName) {
            this.attName = attName;
        }
        
        public String getOriginalValue() {
            return originalValue;
        }
        
        public void setOriginalValue(String originalValue) {
            this.originalValue = originalValue;
        }
        
        public String getNewValue() {
            return newValue;
        }
        
        public void setNewValue(String newValue) {
            this.newValue = newValue;
        }
        
        public String getDescription() {
            return attName + " differ: original value '" + originalValue +
                "', new value '" + newValue + "'";
        }
    }
    
    private boolean commit = false;
    
    private boolean addMp = false;
    
    private String configuration;
    
    private String component;
    
    private String componentType;
    
    public boolean getCommit() {
        return commit;
    }

    public void setCommit(boolean commit) {
        this.commit = commit;
    }

    public boolean isAddMp() {
        return addMp;
    }

    public void setAddMp(boolean addMp) {
        this.addMp = addMp;
    }

    public String getConfiguration() {
        return configuration;
    }

    public void setConfiguration(String configuration) {
        this.configuration = configuration;
    }

    public String getComponent() {
        return component;
    }

    public void setComponent(String component) {
        this.component = component;
    }

    public String getComponentType() {
        return componentType;
    }

    public void setComponentType(String componentType) {
        this.componentType = componentType;
    }

    /**
     * Synchronizes BACIProperties and MonitorPoints with respect to the information
     * found in the code-generated TMCDBXXXAdd.xml files. If the member variable 
     * component is null, it will iterate through all the components found in the
     * Configuration.
     * 
     * By default it won't do anything in the database. In order to synchronize
     * the database, the tool needs to be executed with the --commit option. The
     * rationale for this is that the log file should be examined carefully
     * before deciding to commit the changes in the database.
     * 
     * There are three cases both for the BACIProperties and MonitorPoints:
     * 
     * 1) if a BACIProperty or MonitorPoint is in both the database and the the
     * XML file, the attributes are compared. If any difference is found, the
     * database is updated with the values found in the XML file.
     * 
     * 2) if a BACIProperty from the XML file is not found in the database, then
     * it is added. On the contrary, if a MonitorPoint is in the XML file but
     * not in the database, then there's no action, as the record should be
     * added by the Blobber during initialization.
     * 
     * 3) If the BACIProperty is in the database, but not in the XML file, then
     * both the BACIProperty and its MonitorPoints are deleted from the
     * database. Note that if there are MonitorData records pointing to the
     * MonitorPoint, the transaction will fail. In this case the configuration
     * should be cloned and the tool run again. In the same way, if a
     * MonitorPoint is in the database but not in the XML file, the tool will
     * try to delete it. This will fail if there is associated MonitorData.
     * 
     * Nothing is being done yet to the DefaultBACIProperty and
     * DefaultMonitorPoint. Now that I think about it, may be I should include
     * this. The part that is missing, I think, is update DefaultMonitorPoint
     * with the information from the XML so the Blobber creates the correct
     * records afterwards.
     * 
     * @throws XMLException
     * @throws IOException
     * @throws TmcdbException
     * @throws DbConfigException
     */
    public void synchronizeProperties()
        throws XMLException, IOException, TmcdbException, DbConfigException {
        String confName;
        if (configuration == null) {
            confName = System.getenv(TMCDB_CONFIGURATION_NAME);
            if (confName == null) {
                TmcdbException ex = new TmcdbException("Null TMCDB_CONFIGURATION_NAME environment variable");
                throw ex;
            }
        } else {
            confName = configuration;
        }
        logger.info("using configuration " + confName);
        
        TmcdbDbConfig dbconf = null;
        try {
            dbconf = new TmcdbDbConfig(logger);
        } catch (Exception ex) { 
            logger.warning("Cannot create TmcdbDbConfig"); 
            ex.printStackTrace();
        }
        HibernateUtil.createAcsConfigurationFromDbConfig(dbconf);
        Session session = HibernateUtil.getSessionFactory().openSession();
        Transaction tx = session.beginTransaction();
                
        Configuration configuration = getSwConfiguration(confName, session);
        
        // Parse all the TMCDBXXXAdd.xml files.
        String[] hwConfFiles = LruLoader.findTmcdbHwConfigFiles();
        Map<String, LruType> lruTypes = new HashMap<String, LruType>();
        for (String hwConfFile : hwConfFiles) {
            logger.info("parsing " + hwConfFile);
            LruType xmllru = LruType.unmarshalLruType(new FileReader(hwConfFile));
            lruTypes.put(xmllru.getName(), xmllru);
        }
        
        String qstr;
        List<Component> components = null;
        if ( (component == null) && (componentType == null) ) {
            // Update all Components under the Configuration.
            qstr = "FROM Component WHERE configuration = :conf";
            Query query = session.createQuery(qstr);
            query.setParameter("conf", configuration, HibernateUtil.getSessionFactory().getTypeHelper().entity(Component.class));
            components = query.list();
        } else if ( (component == null) && (componentType != null) ) {
            // Update all Components which have the given ComponentType and
            // Configuration.
            qstr = "FROM ComponentType WHERE IDL LIKE :idl";
            Query query = session.createQuery(qstr);
            query.setParameter("idl", componentType, StringType.INSTANCE);
            List<ComponentType> compTypes = query.list();            
            if ( compTypes.size() == 0 ) {
                TmcdbException ex =
                    new TmcdbException(String.format("component type '%s' not found", componentType));
                throw ex;
            }
            components = new ArrayList<Component>();
            for (ComponentType compType : compTypes) {
                qstr = "FROM Component WHERE componentType = :type AND configuration = :conf";
                query = session.createQuery(qstr);
                query.setParameter("type", compType, HibernateUtil.getSessionFactory().getTypeHelper().entity(ComponentType.class));
                query.setParameter("conf", configuration, HibernateUtil.getSessionFactory().getTypeHelper().entity(Component.class));
                components.addAll(query.list());
            }
        } else {
            // Update only the given Component.
            int idx = component.lastIndexOf('/');
            String compname = component.substring(idx+1);
            String path = component.substring(0, idx);
            qstr = "FROM Component WHERE configuration = :conf AND componentName = :name AND path = :path";
            Query query = session.createQuery(qstr);
            query.setParameter("conf", configuration, HibernateUtil.getSessionFactory().getTypeHelper().entity(Component.class));
            query.setParameter("name", compname, StringType.INSTANCE);
            query.setParameter("path", path, StringType.INSTANCE);
            components = query.list();
            if (components.size() == 0) {
                logger.severe("component not found. componentName: " + compname + "; path: " + path);
            }
        }
        String baciQueryStr = "FROM BACIProperty WHERE component = :comp";
        Query baciQuery = session.createQuery(baciQueryStr);
        for (Component comp : components) {
            logger.info("===============================================================================");
            logger.info("synchronizing component " + comp.getPath() + "/" + comp.getComponentName());
                        
            baciQuery.setParameter("comp", comp, HibernateUtil.getSessionFactory().getTypeHelper().entity(BACIProperty.class));
            List<BACIProperty> properties = baciQuery.list();
            
            String allMonPntQyStr = "FROM MonitorPoint WHERE BACIProperty IN (:props)";
            Query allMonPntQy = session.createQuery(allMonPntQyStr);
            allMonPntQy.setParameterList("props", properties, HibernateUtil.getSessionFactory().getTypeHelper().entity(BACIProperty.class));
            List<MonitorPoint> allMonPnts = allMonPntQy.list(); // List with all MP from the configuration
            
            logger.info("# of properties found for this component: " + properties.size());
            
            String type = getLruTypeFromComponentIDL(comp.getComponentType().getIDL());
            logger.fine("using LRU type: " + type);
            LruType lruType = lruTypes.get(type);
            if (lruType != null) {
                // A list to collect all properties in the XML file that have a corresponding
                // property in the database. At the end of processing, if a property in the XML
                // file is not in this list, it means that it was added.
                List<String> xmlFoundBaciProperties = new ArrayList<String>();
                for (BACIProperty prop : properties) {
                    logger.info("-------------------------------------------------------------------------------");
                    logger.info("synchronizing property " + prop.getPropertyName());
                    BaciPropertyT xmlBaciProperty = null;
                    for (BaciPropertyT xmlbp : getXmlBaciProperties(lruType, lruTypes)) {
                        if (xmlbp.getPropertyname().equals(prop.getPropertyName())) {
                            xmlBaciProperty = xmlbp;
                            xmlFoundBaciProperties.add(xmlbp.getPropertyname());
                            break;
                        }
                    }
                    if (xmlBaciProperty != null) {
                        BACIProperty newBaciProperty = toBACIProperty(xmlBaciProperty, comp);
                        AttChange[] bpdiffs = updateBACIProperty(prop, newBaciProperty);
                        if ( bpdiffs.length > 0 ) {
                            // The BACIProperty from the database exists in the XML, but
                            // one or more attributes are different. We update the database in this case.
                            logger.warning("updating property " + prop.getPropertyName());
                            for (AttChange diff : bpdiffs) {
                                logger.warning("  " + diff.getDescription());
                            }
                            if (commit) {
                                session.saveOrUpdate(prop);
                            }
                        }
                        
                        {
                            // Now check the underlying MonitorPoints
                        	ArrayList<MonitorPoint> monPnts = new ArrayList<MonitorPoint>();
                            for (MonitorPoint mp : allMonPnts) {
                            	if( mp.getBACIProperty() == prop )
                            		monPnts.add(mp);
                            }
                            logger.info("# of monitor points found under this property: " + monPnts.size());
                            // List of MonitorPoints from the XML file that have been found after iterating
                            // over the MonitorPoints in the database. This list is used to detect the MonitorPoints
                            // that are present in the XML file but not in the database, and therefore should
                            // be added in the database.
                            List<String> xmlFoundMonitorPoints = new ArrayList<String>();
                            for (MonitorPoint mp : monPnts) {
                                logger.fine("synchronizing monitor point " + mp.getMonitorPointName());
                                MonitorPointT xmlMonitorPoint = null;
                                for (MonitorPointT xmlmp : xmlBaciProperty.getMonitorPoint()) {
                                    if (xmlmp.getMonitorpointname().equals(mp.getMonitorPointName())) {
                                        xmlMonitorPoint = xmlmp;
                                        xmlFoundMonitorPoints.add(xmlmp.getMonitorpointname());
                                        break;
                                    }
                                }
                                if (xmlMonitorPoint != null) {
                                    // A corresponding MonitorPointT in the XML was found, maybe the
                                    // database MonitorPoint needs to be updated.
                                    AttChange[] mpdiffs = updateMonitorPoint(mp,
                                            toMonitorPointNoIndex(xmlMonitorPoint, prop, mp.getAssembly()));
                                    if ( mpdiffs.length > 0 ) {
                                        logger.warning("monitor point " + mp.getMonitorPointName() +
                                                " will be updated in the database");
                                        for (AttChange diff : mpdiffs) {
                                            logger.warning("  " + diff.getDescription());
                                        }
                                        if (commit) {
                                            session.saveOrUpdate(mp);
                                        }
                                    }
                                } else {
                                    // No MonitorPointT from the XML was found. This means the
                                    // MonitorPoint in the database should be deleted. If there are MonitorData
                                    // records pointing to this MonitorPoint, the transaction will fail.
                                    logger.warning("monitor point " + mp.getMonitorPointName() +
                                            " will be deleted in the database");
                                    if (commit) {
                                        session.delete(mp);
                                    }
                                }
                            }

                            // Actually, new MonitorPoints should not be added by the tool. They should be added
                            // during the Assembly dynamic discovery, currently by the Blobber,
                            // in the future by Control.
                            int index = 0;
                            for (MonitorPointT xmlmp : xmlBaciProperty.getMonitorPoint()) {
                                if (!xmlFoundMonitorPoints.contains(xmlmp.getMonitorpointname())) {
                                    if (addMp) {
                                        // If addMp option is set, add all the missing MonitorPoints
                                        // in the database for all the Assemblies of the requried type.
                                        List<Assembly> assemblies = new ArrayList<Assembly>();
                                        for (HWConfiguration hwConf : getHwConfigurations(configuration, session)) {
                                            assemblies.addAll(getAssembliesFromLruType(session,
                                                    getLruTypeFromComponentIDL(comp.getComponentType().getIDL()), hwConf));
                                            
                                        }
                                        for (Assembly assembly : assemblies) {
                                            MonitorPoint mp = toMonitorPoint(xmlmp, prop, assembly, index++);
                                            logger.warning("monitor point " + xmlmp.getMonitorpointname() +
                                                    " will be added in the database for Assembly " + assembly.getSerialNumber());
                                            if (commit) {
                                                session.save(mp);
                                            }
                                        }                                            
                                    } else {
                                        // The MonitorPoint was added in the XML, but it's not in the database.
                                        logger.fine("monitor point " + xmlmp.getMonitorpointname() +
                                                " is not in the database. The record should be added by the Blobber");
                                    }
                                    
                                }
                            }

                        }
                        
                    } else {
                        // Delete the MonitorPoint records for the BACIProperty. If there are
                        // records in the MonitorData associated with the MonitorPoint, then the
                        // deletion will fail. In this case the Configuration should be cloned
                        // and the tool run again.
                        ArrayList<MonitorPoint> monPnts = new ArrayList<MonitorPoint>();
                        for (MonitorPoint mp : allMonPnts) {
                        	if( mp.getBACIProperty() == prop )
                        		monPnts.add(mp);
                        }
                        for (MonitorPoint mon : monPnts) {
                            if (commit) {
                                session.delete(mon);
                            }
                        }
                        // The BACIProperty is in the database but not in the XML, so
                        // it must have been deleted from the spreadsheet.

                        logger.warning("property will be deleted: " + prop.getPropertyName());
                        if (commit) {
                            session.delete(prop);
                        }
                    }
                }
                for (BaciPropertyT xmlbp : getXmlBaciProperties(lruType, lruTypes)) {
                    if (!xmlFoundBaciProperties.contains(xmlbp.getPropertyname())) {
                        // This property was added in the XML, so it should be added in the
                        // database.
                        BACIProperty newBp = toBACIProperty(xmlbp, comp);
                        logger.warning("property will be added: " + xmlbp.getPropertyname());
                        if (commit) {
                            session.save(newBp);
                        }
                        
                        // The monitor points would be added by the Blobber or the upcoming
                        // Control dynamic discovery unless the addMp option is specified.
                        int index = 0;
                        if (addMp) {
                            for (MonitorPointT xmlmp : xmlbp.getMonitorPoint()) {
                                // If addMp option is set, add all the missing MonitorPoints
                                // in the database for all the Assemblies of the requried type.
                                List<Assembly> assemblies = new ArrayList<Assembly>();
                                for (HWConfiguration hwConf : getHwConfigurations(configuration, session)) {
                                    assemblies.addAll(getAssembliesFromLruType(session,
                                            getLruTypeFromComponentIDL(comp.getComponentType().getIDL()), hwConf));
                                    
                                }
                                for (Assembly assembly : assemblies) {
                                    MonitorPoint mp = toMonitorPoint(xmlmp, newBp, assembly, index++);
                                    logger.warning("monitor point " + xmlmp.getMonitorpointname() +
                                            " will be added in the database for Assembly " + assembly.getSerialNumber());
                                    if (commit) {
                                        session.save(mp);
                                    }
                                }                                    
                            }
                        }
                    }
                }
            } else {
                logger.info("no XML file was found for this type: " + type);
            }
            
        }
        if (commit) {
            tx.commit();
        } else {
            tx.rollback();
        }
        session.close();
    }

    /**
     * Retrieves the Software Configuration from DB.
     * 
     * @param confName Configuration name
     * @param session Hibernate Session
     * @return Software Configuration Hibernate POJO
     * @throws TmcdbException If the Configuration is not found in the DB.
     */
    private Configuration getSwConfiguration(String confName,
            Session session) throws TmcdbException {
        String qstr = "FROM Configuration WHERE configurationname = '"
                + confName + "'";
        Configuration configuration = (Configuration) session.createQuery(qstr)
                .uniqueResult();
        if (configuration == null) {
            throw new TmcdbException("Configuration not found in TMCDB: "
                    + confName);
        }
        return configuration;
    }
    
    /**
     * Retrieves all the HWConfigurations associated with a Software Configuration from DB.
     * 
     * @param conf Software Configuration
     * @param session Hibernate Session
     * @return List of HW Configurations
     */
    private List<HWConfiguration> getHwConfigurations(Configuration conf, Session session) {
        String qstr = "FROM HWConfiguration where configuration = :conf";
        Query query = session.createQuery(qstr);
        query.setParameter("conf", conf, HibernateUtil.getSessionFactory().getTypeHelper().entity(Configuration.class));
        return query.list();
    }
    
    /**
     * Returns the BaciPropertyT properties for a given LruType, which is created from the
     * TMCDBAddXXX.xml files, adding the inherited properties if necessary.
     * @param type
     * @return
     */
    private List<BaciPropertyT> getXmlBaciProperties(LruType type, Map<String, LruType> types) {
        List<BaciPropertyT> retVal = new ArrayList<BaciPropertyT>();
        for (BaciPropertyT p : type.getAssemblyType().getBaciProperty()) {
            retVal.add(p);
        }
        if (type.getName().equals("MountAEM")) {
            retVal.addAll(getXmlBaciProperties(types.get("Mount"), types));
        } else if (type.getName().equals("MountVertex")) {
            retVal.addAll(getXmlBaciProperties(types.get("Mount"), types));            
        } else if (type.getName().equals("MountA7M")) {
            retVal.addAll(getXmlBaciProperties(types.get("MountACACommon"), types));            
        } else if (type.getName().equals("MountACA")) {
            retVal.addAll(getXmlBaciProperties(types.get("MountACACommon"), types));            
        } else if (type.getName().equals("MountVertex")) {
            retVal.addAll(getXmlBaciProperties(types.get("Mount"), types));            
        } else if (type.getName().equals("MountACACommon")) {
            retVal.addAll(getXmlBaciProperties(types.get("Mount"), types));            
        } else if (type.getName().equals("PSA")) {
            retVal.addAll(getXmlBaciProperties(types.get("PSU"), types));            
        } else if (type.getName().equals("PSCR")) {
            retVal.addAll(getXmlBaciProperties(types.get("PSU"), types));            
        } else if (type.getName().equals("PSD")) {
            retVal.addAll(getXmlBaciProperties(types.get("PSU"), types));            
        } else if (type.getName().equals("PSLLC")) {
            retVal.addAll(getXmlBaciProperties(types.get("PSU"), types));            
        } else if (type.getName().equals("PSSAS")) {
            retVal.addAll(getXmlBaciProperties(types.get("PSU"), types));            
        } else if (type.getName().equals("LORTM")) {
            retVal.addAll(getXmlBaciProperties(types.get("LSCommon"), types));            
        } else if (type.getName().equals("LS")) {
            retVal.addAll(getXmlBaciProperties(types.get("LSCommon"), types));            
        } else if (type.getName().equals("LSPP")) {
            retVal.addAll(getXmlBaciProperties(types.get("LSCommon"), types));            
        } else if (type.getName().equals("Cryostat")) {
            retVal.addAll(getXmlBaciProperties(types.get("FEMC"), types));
        } else if (type.getName().equals("IFSwitch")) {
            retVal.addAll(getXmlBaciProperties(types.get("FEMC"), types));
        } else if (type.getName().equals("LPR")) {
            retVal.addAll(getXmlBaciProperties(types.get("FEMC"), types));
        } else if (type.getName().equals("ColdCart1")) {
            retVal.addAll(getXmlBaciProperties(types.get("ColdCart"), types));
        } else if (type.getName().equals("ColdCart2")) {
            retVal.addAll(getXmlBaciProperties(types.get("ColdCart"), types));
        } else if (type.getName().equals("ColdCart3")) {
            retVal.addAll(getXmlBaciProperties(types.get("ColdCartSIS"), types));
        } else if (type.getName().equals("ColdCart4")) {
            retVal.addAll(getXmlBaciProperties(types.get("ColdCartSIS"), types));
        } else if (type.getName().equals("ColdCart5")) {
            retVal.addAll(getXmlBaciProperties(types.get("ColdCartSIS"), types));
	} else if (type.getName().equals("ColdCart6")) {
            retVal.addAll(getXmlBaciProperties(types.get("ColdCartSIS"), types));
        } else if (type.getName().equals("ColdCart7")) {
            retVal.addAll(getXmlBaciProperties(types.get("ColdCartSIS"), types));
        } else if (type.getName().equals("ColdCart8")) {
            retVal.addAll(getXmlBaciProperties(types.get("ColdCartSIS"), types));
        } else if (type.getName().equals("ColdCart9")) {
            retVal.addAll(getXmlBaciProperties(types.get("ColdCartSIS"), types));
	} else if (type.getName().equals("ColdCart10")) {
            retVal.addAll(getXmlBaciProperties(types.get("ColdCartSIS"), types));
        } else if (type.getName().equals("PowerDist1")) {
            retVal.addAll(getXmlBaciProperties(types.get("PowerDist"), types));
        } else if (type.getName().equals("PowerDist2")) {
            retVal.addAll(getXmlBaciProperties(types.get("PowerDist"), types));
        } else if (type.getName().equals("PowerDist3")) {
            retVal.addAll(getXmlBaciProperties(types.get("PowerDist"), types));
        } else if (type.getName().equals("PowerDist4")) {
            retVal.addAll(getXmlBaciProperties(types.get("PowerDist"), types));
	} else if (type.getName().equals("PowerDist5")) {
            retVal.addAll(getXmlBaciProperties(types.get("PowerDist"), types));
        } else if (type.getName().equals("PowerDist6")) {
            retVal.addAll(getXmlBaciProperties(types.get("PowerDist"), types));
        } else if (type.getName().equals("PowerDist7")) {
            retVal.addAll(getXmlBaciProperties(types.get("PowerDist"), types));
        } else if (type.getName().equals("PowerDist8")) {
            retVal.addAll(getXmlBaciProperties(types.get("PowerDist"), types));
        } else if (type.getName().equals("PowerDist9")) {
            retVal.addAll(getXmlBaciProperties(types.get("PowerDist"), types));
        } else if (type.getName().equals("PowerDist10")) {
            retVal.addAll(getXmlBaciProperties(types.get("PowerDist"), types));
        } else if (type.getName().equals("WCA1")) {
            retVal.addAll(getXmlBaciProperties(types.get("WCA"), types));
        } else if (type.getName().equals("WCA2")) {
            retVal.addAll(getXmlBaciProperties(types.get("WCA"), types));
	} else if (type.getName().equals("WCA3")) {
            retVal.addAll(getXmlBaciProperties(types.get("WCA"), types));
        } else if (type.getName().equals("WCA4")) {
            retVal.addAll(getXmlBaciProperties(types.get("WCA"), types));
        } else if (type.getName().equals("WCA5")) {
            retVal.addAll(getXmlBaciProperties(types.get("WCA"), types));
	} else if (type.getName().equals("WCA6")) {
            retVal.addAll(getXmlBaciProperties(types.get("WCA"), types));
        } else if (type.getName().equals("WCA7")) {
            retVal.addAll(getXmlBaciProperties(types.get("WCA"), types));
        } else if (type.getName().equals("WCA8")) {
            retVal.addAll(getXmlBaciProperties(types.get("WCA"), types));
        } else if (type.getName().equals("WCA9")) {
            retVal.addAll(getXmlBaciProperties(types.get("WCA"), types));
        } else if (type.getName().equals("WCA10")) {
            retVal.addAll(getXmlBaciProperties(types.get("WCA"), types));
        } else if (type.getName().equals("ColdCartSIS")) {
            retVal.addAll(getXmlBaciProperties(types.get("ColdCart"), types));
	} else if (type.getName().equals("ColdCart")) {
            retVal.addAll(getXmlBaciProperties(types.get("FEMC"), types));
        } else if (type.getName().equals("PowerDist")) {
            retVal.addAll(getXmlBaciProperties(types.get("FEMC"), types));
        } else if (type.getName().equals("WCA")) {
            retVal.addAll(getXmlBaciProperties(types.get("FEMC"), types));
        }
        return retVal;
    }
    
    /**
     * Updates a BACIProperty according to the changes represented in another
     * BACIProperty.
     * @param origBp Original BACIProperty
     * @param updBp Updated BACIProperty
     * @return True if the original BACIProperty was changed, False if there were no
     * changes.
     */
    private AttChange[] updateBACIProperty(BACIProperty origBp, BACIProperty updBp) {
        List<AttChange> attchs = new ArrayList<AttChange>();
        if ((origBp.getDescription() != null) && !origBp.getDescription().equals(updBp.getDescription())) {
            attchs.add(new AttChange("Description", origBp.getDescription(), updBp.getDescription()));
            origBp.setDescription(updBp.getDescription());
        }
        if ((origBp.getFormat() != null) && !origBp.getFormat().equals(updBp.getFormat())) {
            attchs.add(new AttChange("Format", origBp.getFormat(), updBp.getFormat()));
            origBp.setFormat(updBp.getFormat());
        }
        if ((origBp.getUnits() != null) && !origBp.getUnits().equals(updBp.getUnits())) {
            attchs.add(new AttChange("Units", origBp.getUnits(), updBp.getUnits()));
            origBp.setUnits(updBp.getUnits());
        }
        if ((origBp.getResolution() != null) && !origBp.getResolution().equals(updBp.getResolution())) {
            attchs.add(new AttChange("Resolution", origBp.getResolution(), updBp.getResolution()));
            origBp.setResolution(updBp.getResolution());
        }
        if ((origBp.getArchive_priority() != null) && !origBp.getArchive_priority().equals(updBp.getArchive_priority())) {
            attchs.add(new AttChange("Archive_priority", origBp.getArchive_priority().toString(),
                    updBp.getArchive_priority().toString()));
            origBp.setArchive_priority(updBp.getArchive_priority());
        }        
        if ((origBp.getArchive_min_int() != null) && !origBp.getArchive_min_int().equals(updBp.getArchive_min_int())) {
            attchs.add(new AttChange("Archive_min_int", origBp.getArchive_min_int().toString(),
                    updBp.getArchive_min_int().toString()));
            origBp.setArchive_min_int(updBp.getArchive_min_int());
        }
        if ((origBp.getArchive_max_int() != null) && !origBp.getArchive_max_int().equals(updBp.getArchive_max_int())) {
            attchs.add(new AttChange("Archive_max_int", origBp.getArchive_max_int().toString(),
                    updBp.getArchive_max_int().toString()));
            origBp.setArchive_max_int(updBp.getArchive_max_int());
        }
        if ((origBp.getDefault_timer_trig() != null) && !origBp.getDefault_timer_trig().equals(updBp.getDefault_timer_trig())) {
            attchs.add(new AttChange("Default_timer_trig", origBp.getDefault_timer_trig().toString(),
                    updBp.getDefault_timer_trig().toString()));
            origBp.setDefault_timer_trig(updBp.getDefault_timer_trig());
        }        
        if ((origBp.getMin_timer_trig() != null) && !origBp.getMin_timer_trig().equals(updBp.getMin_timer_trig())) {
            attchs.add(new AttChange("Min_timer_trig", origBp.getMin_timer_trig().toString(),
                    updBp.getMin_timer_trig().toString()));
            origBp.setMin_timer_trig(updBp.getMin_timer_trig());
        }
        if ((origBp.getInitialize_devio() != null) && !origBp.getInitialize_devio().equals(updBp.getInitialize_devio())) {
            attchs.add(new AttChange("Initialize_devio", origBp.getInitialize_devio().toString(),
                    updBp.getInitialize_devio().toString()));
            origBp.setInitialize_devio(updBp.getInitialize_devio());
        }
        if ((origBp.getMin_delta_trig() != null) && !origBp.getMin_delta_trig().equals(updBp.getMin_delta_trig())) {
            attchs.add(new AttChange("Min_delta_trig", origBp.getMin_delta_trig().toString(),
                    updBp.getMin_delta_trig().toString()));
            origBp.setMin_delta_trig(updBp.getMin_delta_trig());
        }
        if ((origBp.getDefault_value() != null) && !origBp.getDefault_value().equals(updBp.getDefault_value())) {
            attchs.add(new AttChange("Default_value", origBp.getDefault_value(), updBp.getDefault_value()));
            origBp.setDefault_value(updBp.getDefault_value());
        }
        if ((origBp.getGraph_min() != null) && !origBp.getGraph_min().equals(updBp.getGraph_min())) {
            attchs.add(new AttChange("Graph_min", origBp.getGraph_min().toString(),
                    updBp.getGraph_min().toString()));
            origBp.setGraph_min(updBp.getGraph_min());
        }        
        if ((origBp.getGraph_max() != null) && !origBp.getGraph_max().equals(updBp.getGraph_max())) {
            attchs.add(new AttChange("Graph_max", origBp.getGraph_max().toString(),
                    updBp.getGraph_max().toString()));
            origBp.setGraph_max(updBp.getGraph_max());
        }
        if ((origBp.getMin_step() != null) && !origBp.getMin_step().equals(updBp.getMin_step())) {
            attchs.add(new AttChange("Min_step", origBp.getMin_step().toString(),
                    updBp.getMin_step().toString()));
            origBp.setMin_step(updBp.getMin_step());
        }
        if ((origBp.getArchive_delta() != null) && !origBp.getArchive_delta().equals(updBp.getArchive_delta())) {
            attchs.add(new AttChange("Archive_delta", origBp.getArchive_delta().toString(),
                    updBp.getArchive_delta().toString()));
            origBp.setArchive_delta(updBp.getArchive_delta());
        }        
        if ((origBp.getAlarm_high_on() != null) && !origBp.getAlarm_high_on().equals(updBp.getAlarm_high_on())) {
            attchs.add(new AttChange("Alarm_high_on", origBp.getAlarm_high_on().toString(),
                    updBp.getAlarm_high_on().toString()));
            origBp.setAlarm_high_on(updBp.getAlarm_high_on());
        }
        if ((origBp.getAlarm_low_on() != null) && !origBp.getAlarm_low_on().equals(updBp.getAlarm_low_on())) {
            attchs.add(new AttChange("Alarm_low_on", origBp.getAlarm_low_on().toString(),
                    updBp.getAlarm_low_on().toString()));
            origBp.setAlarm_low_on(updBp.getAlarm_low_on());
        }
        if ((origBp.getAlarm_high_off() != null) && !origBp.getAlarm_high_off().equals(updBp.getAlarm_high_off())) {
            attchs.add(new AttChange("Alarm_high_off", origBp.getAlarm_high_off().toString(),
                    updBp.getAlarm_high_off().toString()));
            origBp.setAlarm_high_off(updBp.getAlarm_high_off());
        }
        if ((origBp.getAlarm_low_off() != null) && !origBp.getAlarm_low_off().equals(updBp.getAlarm_low_off())) {
            attchs.add(new AttChange("Alarm_low_off", origBp.getAlarm_low_off().toString(),
                    updBp.getAlarm_low_off().toString()));
            origBp.setAlarm_low_off(updBp.getAlarm_low_off());
        }
        if ((origBp.getAlarm_timer_trig() != null) && !origBp.getAlarm_timer_trig().equals(updBp.getAlarm_timer_trig())) {
            attchs.add(new AttChange("Alarm_timer_trig", origBp.getAlarm_timer_trig().toString(),
                    updBp.getAlarm_timer_trig().toString()));
            origBp.setAlarm_timer_trig(updBp.getAlarm_timer_trig());
        }        
        if ((origBp.getMin_value() != null) && !origBp.getMin_value().equals(updBp.getMin_value())) {
            attchs.add(new AttChange("Min_value", origBp.getMin_value().toString(),
                    updBp.getMin_value().toString()));
            origBp.setMin_value(updBp.getMin_value());
        }
        if ((origBp.getMax_value() != null) && !origBp.getMax_value().equals(updBp.getMax_value())) {
            attchs.add(new AttChange("Max_value", origBp.getMax_value().toString(),
                    updBp.getMax_value().toString()));
            origBp.setMax_value(updBp.getMax_value());
        }
        if ((origBp.getBitDescription() != null) && !origBp.getBitDescription().equals(updBp.getBitDescription())) {
            attchs.add(new AttChange("BitDescription", origBp.getBitDescription(), updBp.getBitDescription()));
            origBp.setBitDescription(updBp.getBitDescription());
        }        
        if ((origBp.getWhenSet() != null) && !origBp.getWhenSet().equals(updBp.getWhenSet())) {
            attchs.add(new AttChange("WhenSet", origBp.getWhenSet(), updBp.getWhenSet()));
            origBp.setWhenSet(updBp.getWhenSet());
        }
        if ((origBp.getWhenCleared() != null) && !origBp.getWhenCleared().equals(updBp.getWhenCleared())) {
            attchs.add(new AttChange("WhenCleared", origBp.getWhenCleared(), updBp.getWhenCleared()));
            origBp.setWhenCleared(updBp.getWhenCleared());
        }
        if ((origBp.getStatesDescription() != null) && !origBp.getStatesDescription().equals(updBp.getStatesDescription())) {
            attchs.add(new AttChange("StatesDescription", origBp.getStatesDescription(), updBp.getStatesDescription()));
            origBp.setStatesDescription(updBp.getStatesDescription());
        }
        if ((origBp.getCondition() != null) && !origBp.getCondition().equals(updBp.getCondition())) {
            attchs.add(new AttChange("Condition", origBp.getCondition(), updBp.getCondition()));
            origBp.setCondition(updBp.getCondition());
        }
        if ((origBp.getAlarm_on() != null) && !origBp.getAlarm_on().equals(updBp.getAlarm_on())) {
            attchs.add(new AttChange("Alarm_on", origBp.getAlarm_on(), updBp.getAlarm_on()));
            origBp.setAlarm_on(updBp.getAlarm_on());
        }        
        if ((origBp.getAlarm_off() != null) && !origBp.getAlarm_off().equals(updBp.getAlarm_off())) {
            attchs.add(new AttChange("Alarm_off", origBp.getAlarm_off(), updBp.getAlarm_off()));
            origBp.setAlarm_off(updBp.getAlarm_off());
        }
        if ((origBp.getData() != null) && !origBp.getData().equals(updBp.getData())) {
            attchs.add(new AttChange("Data", origBp.getData(), updBp.getData()));
            origBp.setData(updBp.getData());
        }
        if ((origBp.getAlarm_fault_family() != null) && !origBp.getAlarm_fault_family().equals(updBp.getAlarm_fault_family())) {
            attchs.add(new AttChange("Alarm_fault_family", origBp.getAlarm_fault_family(), updBp.getAlarm_fault_family()));
            origBp.setAlarm_fault_family(updBp.getAlarm_fault_family());
        }        
        if ((origBp.getAlarm_fault_member() != null) && !origBp.getAlarm_fault_member().equals(updBp.getAlarm_fault_member())) {
            attchs.add(new AttChange("Alarm_fault_member", origBp.getAlarm_fault_member(), updBp.getAlarm_fault_member()));
            origBp.setAlarm_fault_member(updBp.getAlarm_fault_member());
        }        
        if ((origBp.getAlarm_level() != null) && !origBp.getAlarm_level().equals(updBp.getAlarm_level())) {
            attchs.add(new AttChange("Alarm_level", origBp.getAlarm_level().toString(),
                    updBp.getAlarm_level().toString()));
            origBp.setAlarm_level(updBp.getAlarm_level());
        }
        if ((origBp.getArchive_suppress() != null) && !origBp.getArchive_suppress().equals(updBp.getArchive_suppress())) {
            attchs.add(new AttChange("Archive_suppress", origBp.getArchive_suppress().toString(),
                    updBp.getArchive_suppress().toString()));
            origBp.setArchive_suppress(updBp.getArchive_suppress());
        }
        if ((origBp.getArchive_mechanism() != null) && !origBp.getArchive_mechanism().equals(updBp.getArchive_mechanism())) {
            attchs.add(new AttChange("Archive_mechanism", origBp.getArchive_mechanism().toString(), updBp.getArchive_mechanism().toString()));
            origBp.setArchive_mechanism(updBp.getArchive_mechanism());
        }
        return attchs.toArray(new AttChange[0]);
    }
    
    private BACIProperty toBACIProperty(BaciPropertyT xmlbp, Component component) {
        BACIProperty bp = new BACIProperty();
        bp.setComponent(component);
        bp.setPropertyName(xmlbp.getPropertyname());
        bp.setDescription(xmlbp.getDescription());
        bp.setFormat(xmlbp.getFormat());
        bp.setUnits(xmlbp.getUnits());
        bp.setResolution(xmlbp.getResolution());
        bp.setArchive_priority(Integer.decode(xmlbp.getArchivePriority()));
        bp.setArchive_min_int(Double.valueOf(xmlbp.getArchiveMinInt()));
        bp.setArchive_max_int(Double.valueOf(xmlbp.getArchiveMaxInt()));
        bp.setDefault_timer_trig(Double.valueOf(xmlbp.getDefaultTimerTrig()));
        bp.setMin_timer_trig(Double.valueOf(xmlbp.getMinTimerTrig()));
        bp.setInitialize_devio(xmlbp.getInitializeDevio().equals("0") ? false : true);
        bp.setMin_delta_trig(Double.valueOf(xmlbp.getMinDeltaTrig()));
        bp.setDefault_value(xmlbp.getDefaultValue());
        bp.setGraph_min(Double.valueOf(xmlbp.getGraphMin()));
        bp.setGraph_max(Double.valueOf(xmlbp.getGraphMax()));
        bp.setMin_step(Double.valueOf(xmlbp.getMinStep()));
        bp.setArchive_delta(Double.valueOf(xmlbp.getArchiveDelta()));
        bp.setAlarm_high_on(Double.valueOf(xmlbp.getAlarmHighOn()));
        bp.setAlarm_low_on(Double.valueOf(xmlbp.getAlarmLowOn()));
        bp.setAlarm_high_off(Double.valueOf(xmlbp.getAlarmHighOff()));
        bp.setAlarm_low_off(Double.valueOf(xmlbp.getAlarmLowOff()));
        bp.setAlarm_timer_trig(Double.valueOf(xmlbp.getAlarmTimerTrig()));
        bp.setMin_value(Double.valueOf(xmlbp.getMinValue()));
        bp.setMax_value(Double.valueOf(xmlbp.getMaxValue()));
        bp.setBitDescription(xmlbp.getBitdescription());
        bp.setWhenSet(xmlbp.getWhenset());
        bp.setWhenCleared(xmlbp.getWhencleared());
        bp.setStatesDescription(xmlbp.getStatedescription());
        bp.setCondition(xmlbp.getCondition());
        bp.setAlarm_on(xmlbp.getAlarmOn());
        bp.setAlarm_off(xmlbp.getAlarmOff());
        bp.setData(xmlbp.getData());
        bp.setAlarm_fault_family(xmlbp.getAlarmFaultFamily());
        bp.setAlarm_fault_member(xmlbp.getAlarmFaultMember());
        bp.setAlarm_level(Integer.decode(xmlbp.getAlarmLevel()));
        bp.setArchive_suppress(xmlbp.getArchiveSuppress().equals("0") ? false : true);
        bp.setArchive_mechanism(BACIPropArchMech.valueOfForEnum(xmlbp.getArchiveMechanism()));
        return bp;
    }

    /**
     * Updates a MonitorPoint according the values of another MonitorPoint. Changes need to be
     * done in-place in the original MonitorPoint in order for Hibernate to realize that the
     * managed object needs to be updated in the database.
     * 
     * @param origMp Original MonitorPoint
     * @param updMp Update MonitorPoint
     * @return True if a difference was detected and performed over the original MonitorPoint.
     * False otherwise.
     */
    private AttChange[] updateMonitorPoint(MonitorPoint origMp, MonitorPoint updMp) {
        List<AttChange> attchs = new ArrayList<AttChange>();
        if ((origMp.getMonitorPointName() != null) && !origMp.getMonitorPointName().equals(updMp.getMonitorPointName())) {
            attchs.add(new AttChange("getMonitorPointName", origMp.getMonitorPointName(),
                    updMp.getMonitorPointName()));
            origMp.setMonitorPointName(updMp.getMonitorPointName());
        }
        if ((origMp.getDataType() != null) && !origMp.getDataType().equals(updMp.getDataType())) {
            attchs.add(new AttChange("getDataType", origMp.getDataType().toString(),
                    updMp.getDataType().toString()));
            origMp.setDataType(updMp.getDataType());
        }
        if ((origMp.getRCA() != null) && !origMp.getRCA().equals(updMp.getRCA())) {
            attchs.add(new AttChange("getRCA", origMp.getRCA(),
                    updMp.getRCA()));
            origMp.setRCA(updMp.getRCA());
        }
        if ((origMp.getTeRelated() != null) && !origMp.getTeRelated().equals(updMp.getTeRelated())) {
            attchs.add(new AttChange("getTeRelated", origMp.getTeRelated().toString(),
                    updMp.getTeRelated().toString()));
            origMp.setTeRelated(updMp.getTeRelated());
        }
        if ((origMp.getRawDataType() != null) && !origMp.getRawDataType().equals(updMp.getRawDataType())) {
            attchs.add(new AttChange("getRCA", origMp.getRawDataType(),
                    updMp.getRawDataType()));
            origMp.setRawDataType(updMp.getRawDataType());
        }
        if ((origMp.getWorldDataType() != null) && !origMp.getWorldDataType().equals(updMp.getWorldDataType())) {
            attchs.add(new AttChange("getWorldDataType", origMp.getWorldDataType(),
                    updMp.getWorldDataType()));
            origMp.setWorldDataType(updMp.getWorldDataType());
        }
        if ((origMp.getUnits() != null) && !origMp.getUnits().equals(updMp.getUnits())) {
            attchs.add(new AttChange("getUnits", origMp.getUnits(),
                    updMp.getUnits()));
            origMp.setUnits(updMp.getUnits());
        }
        if ((origMp.getScale() != null) && !origMp.getScale().equals(updMp.getScale())) {
            attchs.add(new AttChange("getScale", origMp.getScale().toString(),
                    updMp.getScale().toString()));
            origMp.setScale(updMp.getScale());
        }
        if ((origMp.getOffset() != null) && !origMp.getOffset().equals(updMp.getOffset())) {
            attchs.add(new AttChange("getOffset", origMp.getOffset().toString(),
                    updMp.getOffset().toString()));
            origMp.setOffset(updMp.getOffset());
        }
        if ((origMp.getMinRange() != null) && !origMp.getMinRange().equals(updMp.getMinRange())) {
            attchs.add(new AttChange("getMinRange", origMp.getMinRange(),
                    updMp.getMinRange()));
            origMp.setMinRange(updMp.getMinRange());
        }        
        if ((origMp.getMaxRange() != null) && !origMp.getMaxRange().equals(updMp.getMaxRange())) {
            attchs.add(new AttChange("getMaxRange", origMp.getMaxRange(),
                    updMp.getMaxRange()));
            origMp.setMaxRange(updMp.getMaxRange());
        }        
        return attchs.toArray(new AttChange[0]);
    }
    
    /**
     * Creates a MonitorPointT object from a MonitorPoint type. The former is generated
     * from Control's TMCDBAddXXX.xml files, while MonitorPoint is the Hibernate POJO
     * for 
     * @param xmlmp
     * @param baciProperty
     * @param assembly
     * @return
     */
    private MonitorPoint toMonitorPoint(MonitorPointT xmlmp, BACIProperty baciProperty,
            Assembly assembly, int index) {
        MonitorPoint mp = new MonitorPoint();
        mp.setAssembly(assembly);
        mp.setBACIProperty(baciProperty);
        mp.setMonitorPointName(xmlmp.getMonitorpointname());
        mp.setIndice(index);
        mp.setDataType(MonitorPointDatatype.valueOfForEnum(xmlmp.getDatatype()));
        mp.setRCA(xmlmp.getRca());
        mp.setTeRelated(xmlmp.getTerelated().equals("0") ? false : true);
        mp.setRawDataType(xmlmp.getRawdatatype());
        mp.setWorldDataType(xmlmp.getWorlddatatype());
        mp.setUnits(xmlmp.getUnits());
        mp.setScale(Double.valueOf(xmlmp.getScale()));
        mp.setOffset(Double.valueOf(xmlmp.getOffset()));
        mp.setMinRange(xmlmp.getMinrange());
        mp.setMaxRange(xmlmp.getMaxrange());
        mp.setDescription(xmlmp.getDescription());
        return mp;
    }

    private MonitorPoint toMonitorPointNoIndex(MonitorPointT xmlmp, BACIProperty baciProperty,
            Assembly assembly) {
        MonitorPoint mp = new MonitorPoint();
        mp.setAssembly(assembly);
        mp.setBACIProperty(baciProperty);
        mp.setMonitorPointName(xmlmp.getMonitorpointname());
        mp.setDataType(MonitorPointDatatype.valueOfForEnum(xmlmp.getDatatype()));
        mp.setRCA(xmlmp.getRca());
        mp.setTeRelated(xmlmp.getTerelated().equals("0") ? false : true);
        mp.setRawDataType(xmlmp.getRawdatatype());
        mp.setWorldDataType(xmlmp.getWorlddatatype());
        mp.setUnits(xmlmp.getUnits());
        mp.setScale(Double.valueOf(xmlmp.getScale()));
        mp.setOffset(Double.valueOf(xmlmp.getOffset()));
        mp.setMinRange(xmlmp.getMinrange());
        mp.setMaxRange(xmlmp.getMaxrange());
        mp.setDescription(xmlmp.getDescription());
        return mp;
    }

    
    private String getLruTypeFromComponentIDL(String idl) {
        String type = idl.replace("IDL:alma/Control/", "");
        type = type.replace("IDL:alma/Correlator/", "");
        type = type.replace(":1.0", "");
        type = type.replace("CompSimBase", "");
        return type;
    }
    
    private List<Assembly> getAssembliesFromLruType(Session session, String lruType,
            HWConfiguration hwConf) {
        List<Assembly> retVal = new ArrayList<Assembly>();
        String lruTypeQryStr = "FROM LRUType WHERE LRUName = '" + lruType + "'";
        Query lruTypeQry = session.createQuery(lruTypeQryStr);
        LRUType lru = (LRUType) lruTypeQry.uniqueResult();
        for (AssemblyType at : lru.getAssemblyTypes()) {
            // there should be just one AssemblyType
            for (Assembly assbly : at.getAssemblies()) {
                if (assbly.getHWConfiguration().getConfigurationId() == hwConf.getConfigurationId()) {
                    retVal.add(assbly);
                }
            }
        }
        return retVal;
    }
    
    public static void main(String[] args) {
        Options options = new Options();
        Option helpOpt= new Option("h", "help", false, "print this message");
        Option commitOpt= new Option("c", "commit", false, "commits changes into the database");
        Option addMpOpt= new Option("m", "addmp", false, "add MonitorPoints to all Assemblies (CAUTION: only for test environments!)");
        Option verboseOpt= new Option("v", "verbose", false, "outputs more information in the log file (INFO)");
        Option vverboseOpt= new Option("vv", "vverbose", false, "outputs even more information in the log file (ALL)");
        Option logFileOpt = OptionBuilder.withArgName("file")
                                      .hasArg()
                                      .withDescription("output logs in the given file")
                                      .create("logfile");
        Option compNameOpt = OptionBuilder.withArgName("comp_name")
                                          .hasArg()
                                          .withDescription("component to synchronize")
                                          .create("component");
        Option confNameOpt = OptionBuilder.withArgName("conf_name")
                                          .hasArg()
                                          .withDescription("configuration to synchronize")
                                          .create("configuration");
        Option compTypeOpt = OptionBuilder.withArgName("comp_type")
                                          .hasArg()
                                          .withDescription("component type to synchronize (use '%' for wildcard)")
                                          .create("component_type");
        options.addOption(helpOpt);
        options.addOption(commitOpt);
        options.addOption(addMpOpt);
        options.addOption(verboseOpt);
        options.addOption(vverboseOpt);
        options.addOption(logFileOpt);
        options.addOption(compNameOpt);
        options.addOption(confNameOpt);
        options.addOption(compTypeOpt);
        
        boolean commit = false;
        boolean verbose = false;
        boolean vverbose = false;
        boolean addMp = false;
        String logFile = "MonitoringSyncTool.log";
        String component = null;
        String configuration = null;
        String compType = null;
        
        CommandLineParser parser = new GnuParser();
        try {
            CommandLine cli = parser.parse(options, args);
            if (cli.hasOption("help")) {
                HelpFormatter formatter = new HelpFormatter();
                formatter.printHelp("MonitoringSyncTool", options);
                System.exit(0);
            }
            if (cli.hasOption("commit")) {
                commit = true;
            }
            if (cli.hasOption("addmp")) {
                addMp = true;
            }
            if (cli.hasOption("verbose")) {
                verbose = true;
            }
            if (cli.hasOption("vverbose")) {
                vverbose = true;
            }
            if (cli.hasOption("logfile")) {
                logFile = cli.getOptionValue("logfile");
            }
            if (cli.hasOption("component")) {
                component = cli.getOptionValue("component");
            }
            if (cli.hasOption("configuration")) {
                configuration = cli.getOptionValue("configuration");
            }            
            if (cli.hasOption("component_type")) {
                compType = cli.getOptionValue("component_type");
            }            
        } catch (ParseException ex) {
            System.err.println("Error parsing command line options: " + ex.getMessage());
            System.exit(-1);
        }
        
        // The produced log is very important to understand what the tool will do in
        // the database. The tool should be run in "rehearsal" mode first, the log
        // reviewed and then the tool should be run in commit mode.
        try {
            FileHandler logFileHandler = new FileHandler(logFile);
            logFileHandler.setFormatter(new TmcdbLogFormatter());
            for (Handler h : logger.getHandlers()) {
                logger.removeHandler(h);
            }
            for (Handler h : logger.getParent().getHandlers()) {
                logger.getParent().removeHandler(h);
            }
            logger.addHandler(logFileHandler);
            if (verbose) {
                logger.setLevel(Level.INFO);
            } else if (vverbose) {
                logger.setLevel(Level.ALL);
            } else {
                logger.setLevel(Level.WARNING);
            }
        } catch (SecurityException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        // Finally ask the tool to synchronize the properties and monitor points.
        try {
            MonitoringSyncTool tool = new MonitoringSyncTool();
            tool.setCommit(commit);
            tool.setAddMp(addMp);
            tool.setConfiguration(configuration);
            tool.setComponent(component);
            tool.setComponentType(compType);
            tool.synchronizeProperties();
        } catch (XMLException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        } catch (TmcdbException ex) {
            ex.printStackTrace();
        } catch (DbConfigException ex) {
            ex.printStackTrace();
        }
    }
}
