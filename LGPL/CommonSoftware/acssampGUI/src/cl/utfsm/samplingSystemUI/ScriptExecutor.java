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

