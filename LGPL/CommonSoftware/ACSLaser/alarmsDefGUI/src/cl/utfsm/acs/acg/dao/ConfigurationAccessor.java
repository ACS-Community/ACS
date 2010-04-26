package cl.utfsm.acs.acg.dao;

public interface ConfigurationAccessor extends com.cosylab.acs.laser.dao.ConfigurationAccessor {
	public void addConfiguration(String path, String data) throws Exception;
	public String listConfigurations(String path);
}
