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

import java.util.ArrayList;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.HashMap;
import java.util.List;
import java.io.File;

public class ACSLruLoader
{
  protected static String[] findTmcdbHwConfigFiles()
  {
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

    String patternStr = "TMCDB(.*)Add\\.xml";
    Pattern pattern = Pattern.compile(patternStr);
    Matcher matcher = pattern.matcher("");

    HashMap LruUniqueMap = new HashMap();

    List hwConfFiles = new ArrayList();
    for (String dir : dirs) {
      String cd = dir + "/config/";
      String[] fl = new File(cd).list();
      for (String f : fl) {
        matcher.reset(f);
        if (matcher.find()) {
          String lru = matcher.group(1);
          if (!LruUniqueMap.containsKey(lru)) {
            hwConfFiles.add(cd + f);
            LruUniqueMap.put(lru, Boolean.valueOf(true));
          }
        }
      }
    }
    return (String[])hwConfFiles.toArray(new String[0]);
  }
}
