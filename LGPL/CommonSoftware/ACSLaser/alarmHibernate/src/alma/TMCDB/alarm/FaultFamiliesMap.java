/**
 * 
 */
package alma.TMCDB.alarm;

import java.util.Map;
import java.util.logging.Logger;

import org.hibernate.Session;

import alma.acs.tmcdb.Configuration;

import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.cdb.jdal.hibernate.InternalElementsMap;
import com.cosylab.cdb.jdal.hibernate.DOMJavaClassIntrospector.XMLSaver;
import com.cosylab.cdb.jdal.hibernate.plugin.HibernateWDALAlarmPluginImpl;

/**
 * @author msekoranja
 */
@SuppressWarnings("serial")
public class FaultFamiliesMap extends InternalElementsMap<String, alma.TMCDB.alarm.FaultFamily> implements XMLSaver {

	public static final String ALARM_CATEGORY_DEFINITION_PATH = "/Alarms/AlarmDefinitions";

	private final Session session;
	private final Configuration config;
	private final ConfigurationAccessor conf;
	private final Map<String, Object> rootMap;
	private final Logger m_logger;
	
	public FaultFamiliesMap(Session session, Configuration config, ConfigurationAccessor conf, Map<String, Object> rootMap, Logger m_logger) {
		this.session = session;
		this.config = config;
		this.conf = conf;
		this.rootMap = rootMap;
		this.m_logger = m_logger;
	}

	@Override
	public void save() {
		// noop
	}

	@Override
	public void save(String xml) {
		boolean rollback = false;
		String backup = null;
		try 
		{
			backup = conf.getConfiguration(ALARM_CATEGORY_DEFINITION_PATH);
			rollback = true;
			
			// replace with new
			conf.setConfiguration(ALARM_CATEGORY_DEFINITION_PATH, xml);

			// validate and update
			HibernateWDALAlarmPluginImpl.importAlarms(session, config, conf, m_logger);
			
			// alles OK, do not rollback
			rollback = false;

			// and reload
			HibernateWDALAlarmPluginImpl.loadEpilogue(session, config, rootMap, m_logger);
			
		} catch (Throwable th) {
			throw new RuntimeException(th);
		} finally {
			if (rollback) {
				try {
					conf.setConfiguration(ALARM_CATEGORY_DEFINITION_PATH, backup);
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		}
	}

}
