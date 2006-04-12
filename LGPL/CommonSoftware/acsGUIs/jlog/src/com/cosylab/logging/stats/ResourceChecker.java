package com.cosylab.logging.stats;

public class ResourceChecker {

	public static String getMemoryStatusMessage() {
        Runtime rt = Runtime.getRuntime();
        long totalMemKB = rt.totalMemory()/1024;
        long usedMemKB = totalMemKB - rt.freeMemory()/1024;
        String memStatus = "Memory usage " + usedMemKB + " of " + totalMemKB + " kB ";
        long maxMem = rt.maxMemory();
        if (maxMem < Long.MAX_VALUE) {
            long maxMemKB = maxMem/1024;
            memStatus += "(= " + (usedMemKB*1000/maxMemKB)/10.0 + "% of JVM growth limit " + maxMemKB + " kB) ";
        }
		return memStatus;
	}

}
