/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package cl.utfsm.samplingSystemUI;

import java.io.BufferedReader;
import java.io.InputStreamReader;



public class ScriptExecutor { 

    private String executableName;
    private String arguments;

    public ScriptExecutor(String executable, String inArguments) {
        setExecutableName(executable);
        setArguments(inArguments);
    }

    public void run() {
        try {
            Runtime rt = Runtime.getRuntime();
            
            System.out.println("Changing script permissions (chmod +x)...");
            Process pr = rt.exec("chmod +x " + getExecutableName());
            System.out.println("Executing: " + getExecutableName() + " " + getArguments());
            pr = rt.exec(getExecutableName() + " " + getArguments());
    
            BufferedReader input = new BufferedReader(new InputStreamReader(pr.getInputStream()));
    
            String line=null;
    
            while((line=input.readLine()) != null) {
                System.out.println(line);
            }
    
            int exitVal = pr.waitFor();
            System.out.println("Exited with error code "+exitVal);
    
        } catch(Exception e) {
            System.out.println(e.toString());
            e.printStackTrace();
        }
    }

	public void setArguments(String arguments) {
		this.arguments = arguments;
	}

	public String getArguments() {
		return arguments;
	}

	public void setExecutableName(String executableName) {
		this.executableName = executableName;
	}

	public String getExecutableName() {
		return executableName;
	}
}

