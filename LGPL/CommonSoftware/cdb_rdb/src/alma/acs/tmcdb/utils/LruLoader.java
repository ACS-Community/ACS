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
 * "@(#) $Id: LruLoader.java,v 1.37 2012/12/21 17:08:47 rhiriart Exp $"
 */
package alma.acs.tmcdb.utils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.Reader;
import java.lang.reflect.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.exolab.castor.xml.XMLException;
import org.hibernate.Session;
import org.hibernate.Transaction;

import alma.acs.tmcdb.AssemblyType;
import alma.acs.tmcdb.ComponentType;
//import alma.archive.database.helpers.wrappers.DbConfigException;
//import alma.archive.database.helpers.wrappers.TmcdbDbConfig;
//import alma.tmcdb.domain.BaseElementType; //ICT-9637: got BaseElementType enum here.
import alma.acs.tmcdb.generated.lrutype.AssemblyTypeT;
import alma.acs.tmcdb.generated.lrutype.LruType;


/**
 * Utility to populate the LruType and AssemblyType tables with data from
 * CONTROL TMCDB hardware configuration files.
 */
public class LruLoader {
	
	public enum BaseElementType {
	    Antenna,
	    Pad,
	    FrontEnd,
	    WeatherStationController,
	    CentralLO,
	    AOSTiming,
	    HolographyTower,
	    Array,
	    PhotonicReference,
	    CorrQuadrant,
	    AcaCorrSet
	}

	// Used to store the production and simulation codes for an assembly type
	private static class ATI { // Assembly Type Information
		public String production;
		public String simulation;
		public BaseElementType parentBaseElement;
		public ATI(String prod, String sim, BaseElementType baseElement) {
			production = prod;
			simulation = sim;
			parentBaseElement = baseElement;
		}
	}

	private static Map<String, ATI> assemblyTypesInfo;

	// Hardcoded info for assembly types
	static {

		assemblyTypesInfo = new HashMap<String, ATI>();

		// Antenna
		// TODO: CMPR missing?
		assemblyTypesInfo.put("DRX",              new ATI("DRXImpl",              "DRXCompSimImpl",       BaseElementType.Antenna));
		assemblyTypesInfo.put("DTSR",             new ATI("DTSRImpl",             "DTSRCompSimImpl",      BaseElementType.Antenna));
		assemblyTypesInfo.put("DTX",              new ATI("DTXImpl",              "DTXCompSimImpl",       BaseElementType.Antenna));
		assemblyTypesInfo.put("DGCK",             new ATI("DGCKImpl",             "DGCKCompSim",          BaseElementType.Antenna));
		assemblyTypesInfo.put("FLOOG",            new ATI("FLOOGImpl",            "FLOOGCompSimImpl",     BaseElementType.Antenna));
		assemblyTypesInfo.put("FOAD",             new ATI("FOADImpl",             "FOADCompSimImpl",      BaseElementType.Antenna));
		assemblyTypesInfo.put("HoloDSP",          new ATI("HOLODSP",              "HOLODSPCompSim",       BaseElementType.Antenna));
		assemblyTypesInfo.put("HoloRx",           new ATI("HOLORX",               "HOLORXCompSim",        BaseElementType.Antenna));
		assemblyTypesInfo.put("IFProc",           new ATI("IFProcImpl",           "IFProcCompSim",        BaseElementType.Antenna));
		assemblyTypesInfo.put("LLC",              new ATI("LLCImpl",              "LLCCompSimImpl",       BaseElementType.Antenna));
		assemblyTypesInfo.put("LO2",              new ATI("LO2Impl",              "LO2CompSimImpl",       BaseElementType.Antenna));
		assemblyTypesInfo.put("LORR",             new ATI("LORRImpl",             "LORRCompSimImpl",      BaseElementType.Antenna));
		assemblyTypesInfo.put("Mount",            new ATI("",                     "",                     BaseElementType.Antenna)); // TODO: Check this one
		assemblyTypesInfo.put("MountACA",         new ATI("MountACA",             "MountACACompSim",      BaseElementType.Antenna));
		assemblyTypesInfo.put("MountAEM",         new ATI("MountAEM",             "MountAEMCompSim",      BaseElementType.Antenna));
		assemblyTypesInfo.put("MountVertex",      new ATI("MountVertex",          "MountVertexCompSim",   BaseElementType.Antenna));
		assemblyTypesInfo.put("OpticalTelescope", new ATI("OpticalTelescopeImpl", "OpticalTelescopeImpl", BaseElementType.Antenna));
		assemblyTypesInfo.put("PSA",              new ATI("PSAImpl",              "PSACompSimImpl",       BaseElementType.Antenna));
		assemblyTypesInfo.put("PSD",              new ATI("PSDImpl",              "PSDCompSimImpl",       BaseElementType.Antenna));
		assemblyTypesInfo.put("SAS",              new ATI("SASImpl",              "SASCompSimImpl",       BaseElementType.Antenna));
		assemblyTypesInfo.put("WVR",              new ATI("WVRImpl",              "WVRCompSim",           BaseElementType.Antenna));
        assemblyTypesInfo.put("NUTATOR",          new ATI("NUTATORImpl",          "NUTATORCompSimImpl",   BaseElementType.Antenna));


		// Front End
		assemblyTypesInfo.put("ACD",         new ATI("ACDImpl",         "ACDCompSimImpl",         BaseElementType.FrontEnd));
		assemblyTypesInfo.put("ColdCart1",   new ATI("ColdCart1Impl",   "ColdCart1CompSimImpl",   BaseElementType.FrontEnd));
		assemblyTypesInfo.put("ColdCart2",   new ATI("ColdCart2Impl",   "ColdCart2CompSimImpl",   BaseElementType.FrontEnd));
		assemblyTypesInfo.put("ColdCart3",   new ATI("ColdCart3Impl",   "ColdCart3CompSimImpl",   BaseElementType.FrontEnd));
		assemblyTypesInfo.put("ColdCart4",   new ATI("ColdCart4Impl",   "ColdCart4CompSimImpl",   BaseElementType.FrontEnd));
		assemblyTypesInfo.put("ColdCart5",   new ATI("ColdCart5Impl",   "ColdCart5CompSimImpl",   BaseElementType.FrontEnd));
		assemblyTypesInfo.put("ColdCart6",   new ATI("ColdCart6Impl",   "ColdCart6CompSimImpl",   BaseElementType.FrontEnd));
		assemblyTypesInfo.put("ColdCart7",   new ATI("ColdCart7Impl",   "ColdCart7CompSimImpl",   BaseElementType.FrontEnd));
		assemblyTypesInfo.put("ColdCart8",   new ATI("ColdCart8Impl",   "ColdCart8CompSimImpl",   BaseElementType.FrontEnd));
		assemblyTypesInfo.put("ColdCart9",   new ATI("ColdCart9Impl",   "ColdCart9CompSimImpl",   BaseElementType.FrontEnd));
		assemblyTypesInfo.put("ColdCart10",  new ATI("ColdCart10Impl",  "ColdCart10CompSimImpl",  BaseElementType.FrontEnd));
		assemblyTypesInfo.put("Cryostat",    new ATI("CryostatImpl",    "CryostatCompSimImpl",    BaseElementType.FrontEnd));
		assemblyTypesInfo.put("PowerDist1",  new ATI("PowerDist1Impl",  "PowerDist1CompSimImpl",  BaseElementType.FrontEnd));
		assemblyTypesInfo.put("PowerDist2",  new ATI("PowerDist2Impl",  "PowerDist2CompSimImpl",  BaseElementType.FrontEnd));
		assemblyTypesInfo.put("PowerDist3",  new ATI("PowerDist3Impl",  "PowerDist3CompSimImpl",  BaseElementType.FrontEnd));
		assemblyTypesInfo.put("PowerDist4",  new ATI("PowerDist4Impl",  "PowerDist4CompSimImpl",  BaseElementType.FrontEnd));
		assemblyTypesInfo.put("PowerDist5",  new ATI("PowerDist5Impl",  "PowerDist5CompSimImpl",  BaseElementType.FrontEnd));
		assemblyTypesInfo.put("PowerDist6",  new ATI("PowerDist6Impl",  "PowerDist6CompSimImpl",  BaseElementType.FrontEnd));
		assemblyTypesInfo.put("PowerDist7",  new ATI("PowerDist7Impl",  "PowerDist7CompSimImpl",  BaseElementType.FrontEnd));
		assemblyTypesInfo.put("PowerDist8",  new ATI("PowerDist8Impl",  "PowerDist8CompSimImpl",  BaseElementType.FrontEnd));
		assemblyTypesInfo.put("PowerDist9",  new ATI("PowerDist9Impl",  "PowerDist9CompSimImpl",  BaseElementType.FrontEnd));
		assemblyTypesInfo.put("PowerDist10", new ATI("PowerDist10Impl", "PowerDist10CompSimImpl", BaseElementType.FrontEnd));
		assemblyTypesInfo.put("WCA1",        new ATI("WCA1Impl",        "WCA1CompSimImpl",        BaseElementType.FrontEnd));
		assemblyTypesInfo.put("WCA2",        new ATI("WCA2Impl",        "WCA2CompSimImpl",        BaseElementType.FrontEnd));
		assemblyTypesInfo.put("WCA3",        new ATI("WCA3Impl",        "WCA3CompSimImpl",        BaseElementType.FrontEnd));
		assemblyTypesInfo.put("WCA4",        new ATI("WCA4Impl",        "WCA4CompSimImpl",        BaseElementType.FrontEnd));
		assemblyTypesInfo.put("WCA5",        new ATI("WCA5Impl",        "WCA5CompSimImpl",        BaseElementType.FrontEnd));
		assemblyTypesInfo.put("WCA6",        new ATI("WCA6Impl",        "WCA6CompSimImpl",        BaseElementType.FrontEnd));
		assemblyTypesInfo.put("WCA7",        new ATI("WCA7Impl",        "WCA7CompSimImpl",        BaseElementType.FrontEnd));
		assemblyTypesInfo.put("WCA8",        new ATI("WCA8Impl",        "WCA8CompSimImpl",        BaseElementType.FrontEnd));
		assemblyTypesInfo.put("WCA9",        new ATI("WCA9Impl",        "WCA9CompSimImpl",        BaseElementType.FrontEnd));
		assemblyTypesInfo.put("WCA10",       new ATI("WCA10Impl",       "WCA10CompSimImpl",       BaseElementType.FrontEnd));
		assemblyTypesInfo.put("LPR",         new ATI("LPRImpl",         "LPRCompSimImpl",         BaseElementType.FrontEnd));
		assemblyTypesInfo.put("IFSwitch",    new ATI("IFSwitchImpl",    "IFSwitchCompSimImpl",    BaseElementType.FrontEnd));

		// Master Clock (AOSTiming)
		assemblyTypesInfo.put("CRD",  new ATI("CRDImpl",  "CRDCompSimImpl",  BaseElementType.AOSTiming));
		assemblyTypesInfo.put("GPS",  new ATI("GPSImpl",  "GPSImpl",         BaseElementType.AOSTiming)); // TODO: check the sim code
		assemblyTypesInfo.put("PSCR", new ATI("PSCRImpl", "PSCRCompSimImpl", BaseElementType.AOSTiming));
		assemblyTypesInfo.put("LFRD", new ATI("PDAImpl",  "PDACompSimImpl",  BaseElementType.AOSTiming));

		// Central Rack (CentralLO)
		assemblyTypesInfo.put("PSSAS1", new ATI("PSSASImpl", "PSSASCompSimImpl", BaseElementType.CentralLO));
		assemblyTypesInfo.put("PSSAS2", new ATI("PSSASImpl", "PSSASCompSimImpl", BaseElementType.CentralLO));
		assemblyTypesInfo.put("MLD",    new ATI("PDAImpl",   "PDACompSimImpl",   BaseElementType.CentralLO));
		assemblyTypesInfo.put("ML",     new ATI("MLImpl",    "MLCompSimImpl",    BaseElementType.CentralLO));
		assemblyTypesInfo.put("PSLLC1", new ATI("PSLLCImpl", "PSLLCCompSimImpl", BaseElementType.CentralLO));
		assemblyTypesInfo.put("PSLLC2", new ATI("PSLLCImpl", "PSLLCCompSimImpl", BaseElementType.CentralLO));
		assemblyTypesInfo.put("PSLLC3", new ATI("PSLLCImpl", "PSLLCCompSimImpl", BaseElementType.CentralLO));
		assemblyTypesInfo.put("PSLLC4", new ATI("PSLLCImpl", "PSLLCCompSimImpl", BaseElementType.CentralLO));
		assemblyTypesInfo.put("PSLLC5", new ATI("PSLLCImpl", "PSLLCCompSimImpl", BaseElementType.CentralLO));
		assemblyTypesInfo.put("PSLLC6", new ATI("PSLLCImpl", "PSLLCCompSimImpl", BaseElementType.CentralLO));

		// Photonic Reference
		assemblyTypesInfo.put("PRD", new ATI("PDAImpl", "PDACompSimImpl", BaseElementType.PhotonicReference));
		assemblyTypesInfo.put("CVR", new ATI("CVRImpl", "CVRSimImpl",     BaseElementType.PhotonicReference));
		assemblyTypesInfo.put("LS",  new ATI("LSImpl",  "LSCompSimImpl",  BaseElementType.PhotonicReference));

		// Weather Station
		assemblyTypesInfo.put("MeteoOSF", new ATI("WeatherStationImpl", "WeatherStationCompSimImpl", BaseElementType.WeatherStationController));
		assemblyTypesInfo.put("MeteoCentral", new ATI("WeatherStationImpl", "WeatherStationCompSimImpl", BaseElementType.WeatherStationController));
		assemblyTypesInfo.put("MeteoTB2", new ATI("WeatherStationImpl", "WeatherStationCompSimImpl", BaseElementType.WeatherStationController));

		// TODO: Someone should check if this list is complete
	};
	
	private static BaseElementType getBaseElementType(String deviceName) {
		if (assemblyTypesInfo.keySet().contains(deviceName))
			return assemblyTypesInfo.get(deviceName).parentBaseElement;
		return BaseElementType.Antenna;
	}

    private static String getProductionCode(String name) {
    	if( assemblyTypesInfo.get(name) != null && assemblyTypesInfo.get(name).production.trim().length() != 0 )
    		return assemblyTypesInfo.get(name).production;
    	return "productionCode"; // Should never happen!
	}

	private static String getSimulationCode(String name) {
		if( assemblyTypesInfo.get(name) != null && assemblyTypesInfo.get(name).simulation.trim().length() != 0 )
    		return assemblyTypesInfo.get(name).simulation;
    	return "simulationCode"; // Should never happen!
	}



	/**
	 * Loads all TMCDB hardware configuration files into the database.
	 * 
	 * @param addMissingComponentType
	 *     The AssemblyType table contains a foreign key to the ComponentType
	 *     table. If this parameter is set to true, then if a ComponentType record
	 *     is not found when adding an AssemblyType, this record
	 *     is added.
	 * @throws DbConfigException
	 *     If problems were found in the dbConfig.properties file.  
	 * @throws FileNotFoundException
	 *     In case a file could not be opened for reading.
	 * @throws XMLException
	 * 	   In case of errors parsing a XML configuration file.
	 * @throws TmcdbException
	 * 	   If addMissingComponentType parameter was set to false and there is a
	 *     missing record in the ComponentType table.
	 */
	public static void loadAllHwConfigFiles(boolean addMissingComponentType)
		throws XMLException, FileNotFoundException, Exception {
    	
    	Logger logger = Logger.getLogger("alma.tmcdb.utils.LruLoader");
    	
        String[] hwConfFiles = findTmcdbHwConfigFiles();
        
        TmcdbDbConfig dbconf = null;
        try {
            dbconf = new TmcdbDbConfig(logger);
        } catch (Exception ex) { 
            logger.warning("Cannot create TmcdbDbConfig"); 
            ex.printStackTrace();
        }
        HibernateUtil.createConfigurationFromDbConfig(dbconf);
        Session session = HibernateUtil.getSessionFactory().openSession();
        
        Transaction tx = session.beginTransaction();
        for (String file : hwConfFiles) {
            if (!shouldBeIgnored(file)) {
            	try {
            		loadLruType(session, new FileReader(file), addMissingComponentType);
            	} catch (Exception e) {
            		e.printStackTrace();
            	}
            }
        }
        tx.commit();
        session.close();
    }

	private static boolean shouldBeIgnored(String file) {
        List<String> baseClassesToBeIgnored = new ArrayList<String>();
        baseClassesToBeIgnored.add("Mount");
        baseClassesToBeIgnored.add("FEMC");
        baseClassesToBeIgnored.add("ColdCart");
        baseClassesToBeIgnored.add("PowerDist");
        baseClassesToBeIgnored.add("WCA");
        baseClassesToBeIgnored.add("PSU");
        baseClassesToBeIgnored.add("LSCommon");
	    String devname = file.replaceAll(".*TMCDB", "");
	    devname = devname.replace("Add.xml", "");
	    return baseClassesToBeIgnored.contains(devname);
	}

	/**
	 * Loads one TMCDB hardware configuration files into the database.
	 * 
	 * @param addMissingComponentType
	 *     The AssemblyType table contains a foreign key to the ComponentType
	 *     table. If this parameter is set to true, then if a ComponentType record
	 *     is not found when adding an AssemblyType, this record
	 *     is added.
	 * 
	 * @throws DbConfigException
	 *     If problems were found in the dbConfig.properties file.
	 * @throws XMLException
	 * 	   In case of errors parsing a XML configuration file.
	 * @throws TmcdbException
	 * 	   If addMissingComponentType parameter was set to false and there is a
	 *     missing record in the ComponentType table.
	 */
	public static void loadOneHwConfigFile(Reader in, boolean addMissingComponentType)
    	throws  XMLException, Exception {

    	Logger logger = Logger.getLogger("alma.tmcdb.utils.LruLoader");

        TmcdbDbConfig dbconf = null;
        try {
            dbconf = new TmcdbDbConfig(logger);
        } catch (Exception ex) { }
        HibernateUtil.createConfigurationFromDbConfig(dbconf);
        Session session;
        session = HibernateUtil.getSessionFactory().openSession();
        
        Transaction tx = session.beginTransaction();
        loadLruType(session, in, addMissingComponentType);
        tx.commit();
        session.close();        
    }
    
	/**
	 * Command line interface. With no arguments all the TMCDB hardware configuration
	 * files found in the $ACSROOT/config and $INTROOT/config directories are loaded.
	 * Files can also be passed as arguments, and each one of them will be loaded
	 * individually.
	 * @param args TMCDB configuration files to load into the database.
	 */
    public static void main(String[] args) {
        if (args.length == 0) {
            try {
                loadAllHwConfigFiles(true);
            } catch (XMLException ex) {
				ex.printStackTrace();
			} catch (FileNotFoundException ex) {
				ex.printStackTrace();
			} catch (Exception ex) {
				ex.printStackTrace();
			}
        } else {
            for (int i=0; i<args.length; i++) {
                try {
                    FileReader fr = new FileReader(args[i]);
                    loadOneHwConfigFile(fr, false);
                } catch (FileNotFoundException ex) {
                    ex.printStackTrace();
                } catch (DbConfigException ex) {
                    ex.printStackTrace();
                } catch (XMLException ex) {
					ex.printStackTrace();
				} catch (Exception ex) {
					ex.printStackTrace();
				}
            }
        }
    }
    
    /**
     * Finds the hardware configuration file for a given device and
     * returns its absolute path. It looks for the file in the config
     * directory in ACSROOT and INTROOT.
     * 
     * @param device Device name
     * @return Absolute path to the hardware configuration file
     */
    protected static String findTmcdbHwConfigFile(String device)
        throws FileNotFoundException {
        
        List<String> dirs = new ArrayList<String>();
        String introot = System.getenv("INTROOT");
        if (introot != null) {
            dirs.add(introot);
        }
        String intlist = System.getenv("INTLIST");
        if (intlist != null) {
            String[] intlistDirs = intlist.split(":");
            for (String d : intlistDirs) {
                dirs.add(d);
            }
        }
        String acsroot = System.getenv("ACSROOT");
        if (acsroot != null) {
            dirs.add(acsroot);
        }

        for (String dir : dirs) {
            String cf = dir + "/config/TMCDB" + device + "Add.xml";
            File f = new File(cf);
            if (f.exists()) {
                return cf;
            }
        }
        throw new FileNotFoundException("Device " + device + " not found in ACSROOT/INTROOT");
    }
    
    /**
     * Looks for all the TMCDB hardware configuration files in
     * $ACSROOT/config and $INTROOT/config.
     * 
     * The TMCDB hardware configuration files are generated by CONTROL
     * hardware generation framework from spreadsheets. They contain an
     * XML representation of the Archive Points for the LRU, and general
     * information about the LRU itself.
     * 
     * @return Absolute paths for the TMCDB hardware configuration files
     */
    protected static String[] findTmcdbHwConfigFiles() {
        List<String> dirs = new ArrayList<String>();
        String introot = System.getenv("INTROOT");
        if (introot != null) {
            dirs.add(introot);
        }
        String intlist = System.getenv("INTLIST");
        if (intlist != null) {
            String[] intlistDirs = intlist.split(":");
            for (String d : intlistDirs) {
                dirs.add(d);
            }
        }
        String acsroot = System.getenv("ACSROOT");
        if (acsroot != null) {
            dirs.add(acsroot);
        }
        
        // Let's find the TMCDBXYZAdd.xml files using RegEx
        String patternStr = "TMCDB(.*)Add\\.xml";
        Pattern pattern = Pattern.compile(patternStr);
        Matcher matcher = pattern.matcher("");
        // ... while being sure that there is no duplicated XML in the list
        HashMap<String, Boolean> LruUniqueMap = new HashMap<String,Boolean>();
        
        List<String> hwConfFiles = new ArrayList<String>();
        for (String dir : dirs) {
            String cd = dir + "/config/";
            String[] fl = new File(cd).list();
            for (String f : fl) {
            	matcher.reset(f);
                if (matcher.find()) {
                	String lru = matcher.group(1);
                	if( !LruUniqueMap.containsKey(lru) ){
                		hwConfFiles.add(cd+f);
                		LruUniqueMap.put(lru, true);
                	}
                }
            }
        }
        return hwConfFiles.toArray(new String[0]);
    }

    /**
	 * Loads a LRU type into the database.
	 * 
	 * This function will parse an XML description of the LRU type and create records
	 * in the tables LRUType and AssemblyType. As an option, it can also create a dummy
	 * record in the ComponentType table, to satisfy the relationship between
	 * AssemblyType and ComponentType.
	 * 
	 * @param session Hibernate Session
	 * @param lruIn XML description of the LRU
	 * @param addMissingCompType
	 * If true, a dummy record in the ComponentType table will be added, if the proper
	 * record is missing. The proper record has its IDL set as 'alma/Control/LRUName:1.0'.
	 * @throws XMLException
	 * @throws DbConfigException
	 * @throws Exception 
	 */
    protected static void loadLruType(Session session, Reader lruIn, boolean addMissingCompType)
        throws XMLException, Exception {
        
    	Logger logger = Logger.getLogger("alma.tmcdb.utils.LruLoader");
    	
        LruType xmllru = null;        
        xmllru = LruType.unmarshalLruType(lruIn);

        String query = "FROM LruType where name = '" + xmllru.getName() + "'";
        List<?> lrus = session.createQuery(query).list();
        if( lrus != null && lrus.size() == 1 ) {
        	logger.warning("LruType '" + xmllru.getName() + "' already exists, won't insert it into the database");
        	return;
        }
/*
 * Let's use introspection to resolve Lrus. Otherwise, we need to take also
 * ICD/SharedCode/TMCDB/Persistence
 */
//        alma.tmcdb.domain.LruType dblru = 
//            new alma.tmcdb.domain.LruType(xmllru.getName(),
//                                          xmllru.getFullname(),
//                                          xmllru.getIcd(),
//                                          xmllru.getIcdDate(),
//                                          xmllru.getDescription(),
//                                          xmllru.getNotes());
//        session.save(dblru);
        try{
	        Class LruTypeC = Class.forName("alma.tmcdb.domain.LruType");
	        Constructor LruTypeConstr = LruTypeC.getConstructor(String.class, 
	        		String.class, String.class, long.class, String.class, String.class);
	        Object dblru =  LruTypeConstr.newInstance(xmllru.getName(),
	              xmllru.getFullname(),
	              xmllru.getIcd(),
	              xmllru.getIcdDate(),
	              xmllru.getDescription(),
	              xmllru.getNotes());
	        session.save(dblru);
	        
	        AssemblyTypeT xmlas = xmllru.getAssemblyType();
	        
	        String compType = "IDL:alma/Control/" + xmllru.getName() + ":1.0";
	        query = "FROM ComponentType WHERE IDL = '" + compType + "'";
	        ComponentType ct = (ComponentType) session.createQuery(query)
	                                                  .uniqueResult();
	        if (addMissingCompType && (ct == null)) {
	            ct = new ComponentType();
	            ct.setIDL(compType);
	            session.save(ct);
	        }
	        if (ct == null)
	            throw new Exception("No component type in database for IDL:" + compType);
	        /*
	         * Let's use introspection to resolve Lrus. Otherwise, we need to take also
	         * ICD/SharedCode/TMCDB/Persistence
	         */   
//	                alma.tmcdb.domain.AssemblyType dbas =
//	                    new alma.tmcdb.domain.AssemblyType(xmlas.getName(),
//	                                                       xmllru.getFullname(),
//	                                                       getBaseElementType(xmlas.getName()),
//	                                                       xmlas.getDescription(),
//	                                                       "",
//	                                                       ct,
//	                                                       getProductionCode(xmlas.getName()),
//	                                                       getSimulationCode(xmlas.getName()));
//	                dblru.addAssemblyType(dbas);
//	                session.save(dblru);
	         
        	Class AssemblyTypeC = Class.forName("alma.tmcdb.domain.AssemblyType");
        	Constructor AssemblyTypeConstr = 
        			AssemblyTypeC.getConstructor(String.class,
        					String.class, String.class, String.class,
        					 String.class,ComponentType.class, String.class, String.class);
        	Object dbas = AssemblyTypeConstr.newInstance(xmlas.getName(),
                                               xmllru.getFullname(),
                                               getBaseElementType(xmlas.getName()),
                                               xmlas.getDescription(),
                                               "",
                                               ct,
                                               getProductionCode(xmlas.getName()),
                                               getSimulationCode(xmlas.getName()));
        	
        	Method addAssemblyType = LruTypeC.getDeclaredMethod("AddAssemblyType", AssemblyTypeC);
        	//dblru.addAssemblyType(dbas);
        	addAssemblyType.invoke(dblru, dbas);
            session.save(dblru);
        } catch (NoSuchFieldException e) {
            e.printStackTrace();
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            e.printStackTrace();
        } catch (ClassNotFoundException e){
        	e.printStackTrace();
        }
 

    }

}
