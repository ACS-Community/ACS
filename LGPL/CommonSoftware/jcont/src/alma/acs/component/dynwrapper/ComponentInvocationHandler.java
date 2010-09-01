/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.component.dynwrapper;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import alma.ACS.OffShoot;


/**
 * @author heiko
 */
@SuppressWarnings("unchecked")
public class ComponentInvocationHandler implements InvocationHandler
{
	private final Logger m_logger;
	
	private final Object m_delegate;
	private final Map<String, Method> m_delegateMethodMap;
	
	private final List<TypeMapper> m_typeMappers;
	
	// key=String(fromClassNameToClassName), value=Mapper that can do it
	private final Map<String, TypeMapper> m_mapperMap;

	/**
	 * Constructor for ComponentInvocationHandler.
	 * <p>
	 * <FONT SIZE="-1">Explanation of why <code>delegateIF</code> must be passed 
	 * in addition to <code>delegate</code>: 
	 * the method <code>findDelegateMethod</code> has to resolve the corresponding 
	 * method from the delegation object. 
	 * In the server-side case, the delegation object is the
	 * component implementation, and the passed method belongs to the corba operations
	 * interface of that component. CORBA interfaces are not allowed to use two methods
	 * with the same name but different parameters, and the "internal" functional
	 * interface of the component is foreseen to be derived such that method names
	 * don't change.
	 * <p> 
	 * One might thus believe that there would be a unique correspondence
	 * between method names from the corba operations interface and from the functional 
	 * interface of the component implementation; this is not necessarily the case
	 * though, because the component implementation might inherit arbitrary
	 * methods for which the Corba IDL naming restrictions don't apply. Suppose the 
	 * Corba operations IF contains a method <code>SchedBlock getSchedBlock()</code>.
	 * Thanks to IDL conventions, there can not be any other name-overloaded method like 
	 * <code>SchedBlock getSchedBlock(String name)</code>. However, the component 
	 * implementation could contain this method, either directly or through inheritance, 
	 * outside of the functional interface. This would cause an ambiguity when trying
	 * to dispatch to the correct method. To avoid this conflict, <code>delegateIF</code>
	 * is passed as well, so that the ComponentInvocationHandler knows which 
	 * subset of methods found in 
	 * <code>delegate</code> can be considered for method dispatching. 
	 * </FONT>
	 */
	ComponentInvocationHandler(java.lang.Object delegate, Class delegateIF, Logger logger)
	{
		m_delegate = delegate;
		m_logger = logger;
		m_typeMappers = new ArrayList<TypeMapper>();
		m_mapperMap = new HashMap<String, TypeMapper>();
		m_delegateMethodMap = new HashMap<String, Method>();

		// store delegate methods by name in a Map
		Method[] delegateMethods = delegateIF.getMethods();
		for (int i = 0; i < delegateMethods.length; i++)
		{
			m_delegateMethodMap.put(delegateMethods[i].getName(), delegateMethods[i]);
		}

		// add more type mappers here as they become available
		addTypeMapper(new OffShootMapper(delegate, m_logger));
		addTypeMapper(new IdentityMapper(delegate, m_logger));
		addTypeMapper(new CastorMarshalMapper(delegate, m_logger));
		addTypeMapper(new CastorUnmarshalMapper(delegate, m_logger));		
		addTypeMapper(new HolderMapper(delegate, m_logger));		
		addTypeMapper(new ArrayMapper(delegate, m_logger));
		addTypeMapper(new CompositionMapper(delegate, m_logger));
	}

	void addTypeMapper(TypeMapper typeMapper)
	{
		m_typeMappers.add(typeMapper);
	}	

	public void addOffshoot(Object offshootImpl, OffShoot shoot) {
		for(TypeMapper mapper: m_typeMappers) {
			if( mapper instanceof OffShootMapper ) {
				((OffShootMapper)mapper).addOffshoot(offshootImpl, shoot);
				break;
			}
		}
	}

    /**
     * Processes a method invocation on a proxy instance and returns
     * the result.  This method will be invoked on an invocation handler
     * when a method is invoked on a proxy instance that it is
     * associated with.
     *
     * @param	proxy the proxy instance that the method was invoked on
     *
     * @param	method the <code>Method</code> instance corresponding to
     * the interface method invoked on the proxy instance.  The declaring
     * class of the <code>Method</code> object will be the interface that
     * the method was declared in, which may be a superinterface of the
     * proxy interface that the proxy class inherits the method through.
     *
     * @param	args an array of objects containing the values of the
     * arguments passed in the method invocation on the proxy instance,
     * or <code>null</code> if interface method takes no arguments.
     * Arguments of primitive types are wrapped in instances of the
     * appropriate primitive wrapper class, such as
     * <code>java.lang.Integer</code> or <code>java.lang.Boolean</code>.
     *
     * @return	the value to return from the method invocation on the
     * proxy instance.  If the declared return type of the interface
     * method is a primitive type, then the value returned by
     * this method must be an instance of the corresponding primitive
     * wrapper class; otherwise, it must be a type assignable to the
     * declared return type.  If the value returned by this method is
     * <code>null</code> and the interface method's return type is
     * primitive, then a <code>NullPointerException</code> will be
     * thrown by the method invocation on the proxy instance.  If the
     * value returned by this method is otherwise not compatible with
     * the interface method's declared return type as described above,
     * a <code>ClassCastException</code> will be thrown by the method
     * invocation on the proxy instance.
     *
     * @throws	Throwable the exception to throw from the method
     * invocation on the proxy instance.  The exception's type must be
     * assignable either to any of the exception types declared in the
     * <code>throws</code> clause of the interface method or to the
     * unchecked exception types <code>java.lang.RuntimeException</code>
     * or <code>java.lang.Error</code>.  If a checked exception is
     * thrown by this method that is not assignable to any of the
     * exception types declared in the <code>throws</code> clause of
     * the interface method, then an
     * {@link UndeclaredThrowableException} containing the
     * exception that was thrown by this method will be thrown by the
     * method invocation on the proxy instance.
     *
     * @see	UndeclaredThrowableException
     */
	/**
	 * @see java.lang.reflect.InvocationHandler#invoke(Object, Method, Object[])
	 */
	public Object invoke(Object proxy, Method proxyMethod, Object[] proxyArgs)
		throws Throwable
	{
		Method delegateMethod = findDelegateMethod(proxyMethod);
		
		// translate arguments (todo: only IN and INOUT)
		Object[] delegateArgs = null;
		if (proxyArgs != null)
		{
			delegateArgs = new Object[proxyArgs.length];
			for (int argIndex = 0; argIndex < proxyArgs.length; argIndex++)
			{
				Class delegateArgType = delegateMethod.getParameterTypes()[argIndex];
				delegateArgs[argIndex] = translate(proxyArgs[argIndex], null, delegateArgType);
			}
		}
		
		// call the delegate method
		Object delegateReturn = null;
		try
		{
			delegateReturn = delegateMethod.invoke(m_delegate, delegateArgs);
		}
		catch (InvocationTargetException ex)
		{
			throw ex.getCause();
		}
		
		// translate arguments (todo: only OUT and INOUT arguments)
		int argIndex = 0;
		try 
		{
			if (proxyArgs != null)
			{
				for (argIndex = 0; argIndex < proxyArgs.length; argIndex++)
				{
					Class facadeArgType = proxyMethod.getParameterTypes()[argIndex];
					proxyArgs[argIndex] = translate(delegateArgs[argIndex], proxyArgs[argIndex], facadeArgType);
				}
			}		
		} 
		catch (Exception e) 
		{
			String methodName = m_delegate.getClass().getName() + "#" + delegateMethod.getName();
			throw new DynWrapperException("failed to translate parameter in method " + methodName, e);
//			System.err.println("exception in invoke--after delegate call: ");
//			e.printStackTrace(System.err);
		}

		// translate return value if necessary 
		Object proxyRet = null;
		try 
		{
			if (delegateReturn != null)
			{
				proxyRet = translate(delegateReturn, null, proxyMethod.getReturnType());
			}
		} 
		catch (Exception e) 
		{
			throw new DynWrapperException("failed to translate return value.", e);
//			System.err.println("exception in invoke--after delegate call: ");
//			e.printStackTrace(System.err);
		}
		return proxyRet;
	}


	/**
	 * Finds the <code>Method</code> in the functional interface of the delegate object 
	 * that maps to the given argument <code>method</code>. 
	 * <p>
	 * Note that the restriction to methods from the functional interface (passed to the ctor)
	 * reduces the task to finding a method with the same name, because the interfaces
	 * considered here are derived from CORBA IDL definitions and therefore cannot have
	 * method names overloaded.
	 *   
	 * @param method  the method from the outside interface
	 * @return  the corresponding delegate method.
	 * @throws DynWrapperException  if no such method can be found.
	 */
	Method findDelegateMethod(Method method)
		throws DynWrapperException
	{
		Method matchingDelegateMethod = m_delegateMethodMap.get(method.getName());

		if (matchingDelegateMethod == null)
		{
			throw new DynWrapperException("no method '" + method.getName() + 
											"' found in delegate object's functional interface.");
		}
		
		return matchingDelegateMethod;
	}
	
	
	boolean canTranslate(Class<?> oldObjClass, Class<?> newObjClass)
	{
		String mapKey = oldObjClass.getName() + newObjClass.getName();
		
		if (m_mapperMap.containsKey(mapKey))
		{
			// checked this already
			return true;
		}

		// now try all our mappers and see who can do it
		for (TypeMapper typeMapper : m_typeMappers) {
			if (typeMapper.canTranslate(oldObjClass, newObjClass, this))
			{
				m_mapperMap.put(mapKey, typeMapper);
				return true;
			}
		}
		return false;
	}


	Object translate(Object oldObject, Object newObjectTemplate, Class newObjectClass)
		throws DynWrapperException
	{
		if (oldObject == null)
		{
			return null;
		}
		
		// must convert since oldObject is of wrapper type even if the method signature uses the primitive type
		newObjectClass = primitiveToWrapper(newObjectClass);
		
		if (newObjectClass.isInstance(oldObject))
		{
			// nothing to do... TODO: or do we leave this nothing to our IdentityMapper?
			return oldObject;
		}
		
		// get the right Mapper
		String mapKey = oldObject.getClass().getName() + newObjectClass.getName();
		TypeMapper typeMapper = m_mapperMap.get(mapKey);
		
		// if not found, check them out again; perhaps canTranslate hadn't been called
		if (typeMapper == null)
		{
			canTranslate(oldObject.getClass(), newObjectClass);
			typeMapper = m_mapperMap.get(mapKey);
		}
		// now we know that we can't deal with it 
		if (typeMapper == null)
		{
			String msg = "no TypeMapper found to translate " + oldObject.getClass().getName() + " to " +
							newObjectClass.getName();
			throw new DynWrapperException(msg);
		}
		
		Object newObject = typeMapper.translate(oldObject, newObjectTemplate, newObjectClass, this);

		return newObject;
	}

	/**
	 * Converts the Class of primitive types to the Class of the corresponding wrappers.
	 */
	private Class<?> primitiveToWrapper(Class<?> inClass)
	{
		// todo: check if the JDK provides this method as public (must have it internally somewhere...)
		// remember: Boolean.TYPE == boolean.class 
		if (inClass == Boolean.TYPE) return Boolean.class;
		if (inClass == Character.TYPE) return Character.class;
		if (inClass == Byte.TYPE) return Byte.class;
		if (inClass == Short.TYPE) return Short.class;
		if (inClass == Integer.TYPE) return Integer.class;
		if (inClass == Long.TYPE) return Long.class;
		if (inClass == Float.TYPE) return Float.class;
		if (inClass == Double.TYPE) return Double.class;
		if (inClass == Void.TYPE) return Void.class;
		
		return inClass;
	}
	
}
