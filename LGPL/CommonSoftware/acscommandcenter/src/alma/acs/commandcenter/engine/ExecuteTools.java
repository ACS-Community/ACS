/*
 * Created on Oct 30, 2003 by mschilli
 */
package alma.acs.commandcenter.engine;

import java.util.HashMap;

import alma.entity.xmlbinding.acscommandcentertools.Tool;

/**
 *
 * @author mschilli
 */
public class ExecuteTools {


	protected RunModel runModel;

	public ExecuteTools(RunModel runModel) {
		this.runModel = runModel;
	}

	//
	// ========================================================================================
	//

   public ToolStarter addTool(Tool tool, HashMap<String,Object> input) {
      return new ToolStarter(tool, input);
   }
   

   public class ToolStarter {
      
      protected Tool tool;
      protected HashMap<String,Object> input;
      
      public ToolStarter(Tool tool, HashMap<String,Object> input){
         this.tool = tool;
         this.input = input;
      }
      
      
      public void start(NativeCommand.Listener listener) throws Throwable {
         
         String command = ToolManager.generateCommand(tool, runModel, input);
         
         Executor.localOutProc(command, false, tool.getMaxStartupSeconds() * 1000, tool.getExpectedOutput(), listener);
      }
      
      
   }
   


}

