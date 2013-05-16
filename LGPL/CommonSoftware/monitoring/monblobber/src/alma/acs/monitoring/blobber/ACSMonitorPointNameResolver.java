/*
 * ALMA - Atacama Large Millimiter Array
 * Copyright (c) European Southern Observatory, 2011 
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
 */
package alma.acs.monitoring.blobber;

import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.exolab.castor.xml.XMLException;

public class ACSMonitorPointNameResolver
{
  private Hashtable monitorPointCache;

  public Hashtable getMonitorPointCache()
  {
    return this.monitorPointCache;
  }

  public void loadMonitorPointFromXML()
    throws XMLException, IOException
  {
    this.monitorPointCache = new Hashtable();

    String[] hwConfFiles = ACSLruLoader.findTmcdbHwConfigFiles();
    System.out.println(hwConfFiles.length);
    Map<String, ACSLruType> lruTypes = new HashMap();
    for (String hwConfFile : hwConfFiles) {
      ACSLruType xmllru = ACSLruType.unmarshalLruType(new FileReader(hwConfFile));
      lruTypes.put(xmllru.getName(), xmllru);
    }

    for (Map.Entry<String, ACSLruType> lru : lruTypes.entrySet()) {
      String slru = (String)lru.getKey();
      List<String> xmlFoundBaciProperties = new ArrayList();
      ACSBaciPropertyT xmlBaciProperty = null;
      for (ACSBaciPropertyT xmlbp : getXmlBaciProperties((ACSLruType)lru.getValue(), lruTypes)) {
        xmlFoundBaciProperties.add(xmlbp.getPropertyname());
        String sbp = xmlbp.getPropertyname();
        List xmlFoundMonitorPoints = new ArrayList();
        ACSMonitorPointT xmlMonitorPoint = null;
        int i = 0;
        for (ACSMonitorPointT xmlmp : xmlbp.getMonitorPoint()) {
          xmlFoundMonitorPoints.add(xmlmp.getMonitorpointname());
          String smp = xmlmp.getMonitorpointname();
          this.monitorPointCache.put(sbp + "_" + i, smp);
          i++;
        }
      }
    }
    List xmlFoundBaciProperties;
  }

  public String getMonitorPointName(String bpn, int index) {
    Object mpn = this.monitorPointCache.get(bpn + "_" + index);
    if (mpn != null) {
      return (String)mpn;
    }
    return "unknown";
  }

  private List<ACSBaciPropertyT> getXmlBaciProperties(ACSLruType type, Map<String, ACSLruType> types)
  {
    List retVal = new ArrayList();
    for (ACSBaciPropertyT p : type.getAssemblyType().getBaciProperty()) {
      retVal.add(p);
    }
    if (type.getName().equals("MountAEM"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("Mount"), types));
    else if (type.getName().equals("MountVertex"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("Mount"), types));
    else if (type.getName().equals("MountA7M"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("MountACACommon"), types));
    else if (type.getName().equals("MountACA"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("MountACACommon"), types));
    else if (type.getName().equals("MountVertex"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("Mount"), types));
    else if (type.getName().equals("MountACACommon"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("Mount"), types));
    else if (type.getName().equals("PSA"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("PSU"), types));
    else if (type.getName().equals("PSCR"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("PSU"), types));
    else if (type.getName().equals("PSD"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("PSU"), types));
    else if (type.getName().equals("PSLLC"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("PSU"), types));
    else if (type.getName().equals("PSSAS"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("PSU"), types));
    else if (type.getName().equals("LORTM"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("LSCommon"), types));
    else if (type.getName().equals("LS"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("LSCommon"), types));
    else if (type.getName().equals("LSPP"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("LSCommon"), types));
    else if (type.getName().equals("Cryostat"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("FEMC"), types));
    else if (type.getName().equals("IFSwitch"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("FEMC"), types));
    else if (type.getName().equals("LPR"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("FEMC"), types));
    else if (type.getName().equals("ColdCart3"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("ColdCart"), types));
    else if (type.getName().equals("ColdCart4"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("ColdCart"), types));
    else if (type.getName().equals("ColdCart6"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("ColdCart"), types));
    else if (type.getName().equals("ColdCart7"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("ColdCart"), types));
    else if (type.getName().equals("ColdCart8"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("ColdCart"), types));
    else if (type.getName().equals("ColdCart9"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("ColdCart"), types));
    else if (type.getName().equals("PowerDist3"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("PowerDist"), types));
    else if (type.getName().equals("PowerDist4"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("PowerDist"), types));
    else if (type.getName().equals("PowerDist6"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("PowerDist"), types));
    else if (type.getName().equals("PowerDist7"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("PowerDist"), types));
    else if (type.getName().equals("PowerDist8"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("PowerDist"), types));
    else if (type.getName().equals("PowerDist9"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("PowerDist"), types));
    else if (type.getName().equals("WCA3"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("WCA"), types));
    else if (type.getName().equals("WCA4"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("WCA"), types));
    else if (type.getName().equals("WCA6"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("WCA"), types));
    else if (type.getName().equals("WCA7"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("WCA"), types));
    else if (type.getName().equals("WCA8"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("WCA"), types));
    else if (type.getName().equals("WCA9"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("WCA"), types));
    else if (type.getName().equals("ColdCart"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("FEMC"), types));
    else if (type.getName().equals("PowerDist"))
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("FEMC"), types));
    else if (type.getName().equals("WCA")) {
      retVal.addAll(getXmlBaciProperties((ACSLruType)types.get("FEMC"), types));
    }
    return retVal;
  }
}
