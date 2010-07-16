package cl.utfsm.samplingSystemUI;

public interface IGraphicalUpdater {
	
	//public void updateValues(long time, double value);
	public void updateValues(long time, double value, int position);
	
	//public void setValues(String component, String property);
	public void setValues(String component, String property, int position);
	
	//public void setComponentAvailable(boolean tmp, String reason);
	public void setComponentAvailable(boolean tmp, String reason, int position);
	
	public void resetSampleCount();
	
	public void setTimeWindow(double frequency, int time);
}