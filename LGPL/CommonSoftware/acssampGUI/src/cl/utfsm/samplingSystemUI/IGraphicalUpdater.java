package cl.utfsm.samplingSystemUI;

public interface IGraphicalUpdater {
	
	public void updateValues(long time, double value);
	
	public void setValues(String component, String property);
	
	public void setComponentAvailable(boolean tmp, String reason);
}