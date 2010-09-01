package alma.acs.component.dynwrapper;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import alma.ACS.OffShoot;
import alma.ACS.OffShootOperations;
import alma.acs.logging.AcsLogLevel;

/**
 * Mapper for converting between an OffShoot normal type and an OffShoot that uses special XML entities
 * 
 * @author rtobar, Aug 20th, 2010
 */
public class OffShootMapper extends TypeMapper {

	private final Map<Object, OffShoot> m_offshootMap;

	public OffShootMapper(Object delegate, Logger logger) {
		super(delegate, logger);
		m_offshootMap = new HashMap<Object, OffShoot>();
	}

	void addOffshoot(Object offshootImpl, OffShoot shoot) {
		m_offshootMap.put(offshootImpl, shoot);
	}

	@Override
	public boolean canTranslate(Class<?> oldObjClass, Class<?> newObjClass,
			ComponentInvocationHandler invHandler) {

		if( OffShootOperations.class.isAssignableFrom(oldObjClass) )
			return true;

		return false;
	}

	@Override
	public <T> Object translate(Object oldObject, T newObjectTemplate,
			Class<T> newObjectClass, ComponentInvocationHandler invHandler)
			throws DynWrapperException {

		Object ret = null;
		OffShoot s = m_offshootMap.get(oldObject);

		String helperClass = newObjectClass.getName() + "Helper";
		try {
			Class<?> clazz = Class.forName(helperClass);
			Method m = clazz.getMethod("narrow", org.omg.CORBA.Object.class);
			ret = m.invoke(clazz, s);
		} catch (Exception e) {
			m_logger.log(AcsLogLevel.NOTICE, "Error while narrowing offshoot CORBA object for returning, will return null");
		}

		return ret;
	}

}
