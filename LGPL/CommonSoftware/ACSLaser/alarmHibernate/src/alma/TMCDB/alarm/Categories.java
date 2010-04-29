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
import com.cosylab.cdb.jdal.hibernate.NameOverrideFeature;
import com.cosylab.cdb.jdal.hibernate.DOMJavaClassIntrospector.XMLSaver;
import com.cosylab.cdb.jdal.hibernate.plugin.HibernateWDALAlarmPluginImpl;

/**
 * @author msekoranja
 */
@SuppressWarnings("serial")
public class Categories extends InternalElementsMap<String, Category> implements NameOverrideFeature, XMLSaver {

	public static final String CATEGORY_DEFINITION_PATH = "/Alarms/Administrative/Categories";

	private final Session session;
	private final Configuration config;
	private final ConfigurationAccessor conf;
	private final Map<String, Object> rootMap;
	private final Logger m_logger;
	
	public Categories(Session session, Configuration config, ConfigurationAccessor conf, Map<String, Object> rootMap, Logger m_logger) {
		this.session = session;
		this.config = config;
		this.conf = conf;
		this.rootMap = rootMap;
		this.m_logger = m_logger;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.NameOverrideFeature#getNameOverride()
	 */
	public String getNameOverride() {
		return "categories";
	}

	@Override
	public void save() {
		//noop
	}

	@Override
	public void save(String xml) {
		boolean rollback = false;
		String backup = null;
		try 
		{
			backup = conf.getConfiguration(CATEGORY_DEFINITION_PATH);
			rollback = true;
			
			// replace with new
			conf.setConfiguration(CATEGORY_DEFINITION_PATH, xml);

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
					conf.setConfiguration(CATEGORY_DEFINITION_PATH, backup);
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		}
	}

}
