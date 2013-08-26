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
package alma.demo.XmlComponentImpl;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.PortableServer.Servant;

import alma.ACS.ACSComponentOperations;
import alma.ACS.ComponentStates;
import alma.JContExmplErrTypeTest.XmlComponentErrorEx;
import alma.JavaContainerError.wrappers.AcsJJavaComponentHelperEx;
import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ComponentHelper;
import alma.acs.container.ExternalInterfaceTranslator;
import alma.acs.entityutil.EntityDeserializer;
import alma.acs.entityutil.EntityException;
import alma.acs.entityutil.EntitySerializer;
import alma.demo.ObsProjectTree;
import alma.demo.ObsProjectTreeJ;
import alma.demo.SchedBlockHolder;
import alma.demo.XmlComponentJ;
import alma.demo.XmlComponentOperations;
import alma.demo.XmlComponentPOATie;
import alma.demo.XmlOffshoot;
import alma.maciErrType.wrappers.AcsJComponentCreationEx;
import alma.xmlentity.XmlEntityStruct;
import alma.xmlentity.XmlEntityStructHolder;
import alma.xmljbind.test.obsproposal.ObsProposal;
import alma.xmljbind.test.schedblock.SchedBlock;



/**
 * Helper class for the <code>XmlContainer</code> component.
 * <p>
 *<code>XmlContainer</code> implements an "inner" functional interface
 * that is different from the IDL generated <code>XmlComponentOperations</code>.
 * It uses xml binding classes instead of stringified xml in the method signatures.
 * An interface translator is created that translates between the flat-xml
 * in the outer (CORBA) interface and the transparent-xml binding classes
 * in the inner interface.
 *
 * @author hsommer Nov 29, 2002 3:37:24 PM
 */
public class XmlComponentHelper extends ComponentHelper
{


	/**
     * @param containerLogger
     */
    public XmlComponentHelper(Logger containerLogger)
    {
        super(containerLogger);
    }

    /**
     * @see alma.acs.container.ComponentHelper#_createComponentImpl()
     */
    protected ComponentLifecycle _createComponentImpl()
    {
        return new XmlComponentImpl();
    }

    /**
     * @see alma.acs.container.ComponentHelper#_getPOATieClass()
     */
    protected Class<? extends Servant> _getPOATieClass()
    {
        return XmlComponentPOATie.class;
    }

    /**
     * @see alma.acs.container.ComponentHelper#_getOperationsInterface()
     */
    protected Class<? extends ACSComponentOperations> _getOperationsInterface()
    {
        return XmlComponentOperations.class;
    }


    /**
     * @see alma.acs.container.ComponentHelper#getInternalInterface()
     */
    protected Class<?> getInternalInterface() 
    {
        return XmlComponentJ.class;
    }


    /**
     * @see alma.acs.container.ComponentHelper#_getInterfaceTranslator(java.lang.Object)
     */
    protected Object _getInterfaceTranslator(Object defaultInterfaceTranslator) throws AcsJJavaComponentHelperEx {
        XmlComponentJ impl = null;
        XmlComponentOperations opDelegate = null;
        try {
			impl = (XmlComponentJ) getComponentImpl();
		} catch (AcsJComponentCreationEx e) {
			throw new AcsJJavaComponentHelperEx(e);
		}
        opDelegate = (XmlComponentOperations) defaultInterfaceTranslator;
        return new IFTranslator(impl, opDelegate, getComponentLogger());
    }

	protected String[] _getComponentMethodsExcludedFromInvocationLogging() {
		return new String[] {"dumbMethod"};
	}

	/**
     * Interface translator class that presents serialized XML toward
     * the container, but presents the corresponding Java binding objects
     * toward the component implementation.
     * <p>
     * XmlComponentHelper does not really need to provide such a
     * translator class at all, since the container can do all necessary
     * XML (de-)serializations to mediate between the CORBA interface
     * <code>XmlComponentOperations</code> and <code>XmlComponentJ</code>
     * transparently.
     * <p>
     * Nontheless we want to illustrate the use of the
     * <code>_getInterfaceTranslator</code> method. Even if the container
     * can perform the translation between the various XML method parameters
     * and return types, we still might want to be in full control of this
     * for selected component interface methods.
     * <p>
     * In the method implementations of <code>IFTranslator</code>, the private
     * instance variable <code>m_useDefaultTranslator</code> controls whether
     * the translation should be delegated to the container (represented by
     * the <code>m_defaultTranslator</code> member object), or if it should be
     * carried out by hand (using the methods <code>marshalEntity</code> and
     * <code>unmarshalEntity</code>). For a real application, we would of
     * course have more fine-grained control on the method level,
     * and likely implement either automatic or manual parameter translation,
     * but not both to choose from. This is done here only for demo purposes.
     */
    private static class IFTranslator implements XmlComponentOperations, ExternalInterfaceTranslator
    {
        private boolean m_useDefaultTranslator = true;  // toggle for dev testing

        private XmlComponentJ m_componentImpl;
        private XmlComponentOperations m_defaultTranslator;
        private EntitySerializer m_entSer;

        private Logger m_ifTransLogger;

        IFTranslator(XmlComponentJ componentImpl, XmlComponentOperations defaultTranslator, Logger logger)
        {
            m_componentImpl = componentImpl;
            m_defaultTranslator = defaultTranslator;
            m_ifTransLogger = logger;
            m_entSer = EntitySerializer.getEntitySerializer(logger);
        }

		public ComponentStates componentState()
		{
			return m_componentImpl.componentState();
		}

		public String name()
		{
			return m_componentImpl.name();
		}


        /**
         * @see alma.demo.XmlComponentOperations#createObsProposal()
         */
        public XmlEntityStruct createObsProposal()
        {
            XmlEntityStruct entStruct = null;
            if (m_useDefaultTranslator)
            {
                entStruct = m_defaultTranslator.createObsProposal();
            }
            else
            {
                // call directly into component, do custom translations
                ObsProposal opsProp = m_componentImpl.createObsProposal();
                try
				{
					entStruct = m_entSer.serializeEntity(opsProp, opsProp.getObsProposalEntity());
				}
				catch (EntityException e)
				{
					e.printStackTrace();
					// signature of createObsProposal does not foresee marshalling problems...
					throw new RuntimeException(e);
				}
            }
            return entStruct;
        }

        /**
         * @see alma.demo.XmlComponentOperations#dumbMethod(java.lang.String)
         */
        public int dumbMethod(String somevalue)
        {
            return m_componentImpl.dumbMethod(somevalue);
        }

        /**
         * @see alma.demo.XmlComponentOperations#getAllSchedBlocks()
         */
        public XmlEntityStruct[] getAllSchedBlocks()
        {
            XmlEntityStruct[] entStructs = null;

            if (m_useDefaultTranslator)
            {
                entStructs = m_defaultTranslator.getAllSchedBlocks();
            }
            else
            {
                SchedBlock[] schedBlocks = m_componentImpl.getAllSchedBlocks();

                if (schedBlocks == null)
                {
                    return new XmlEntityStruct[0];
                }

                entStructs = new XmlEntityStruct[schedBlocks.length];
                try
                {
                    for (int i = 0; i < schedBlocks.length; i++)
                    {
                        entStructs[i] = m_entSer.serializeEntity(schedBlocks[i], schedBlocks[i].getSchedBlockEntity());
                    }
                }
                catch (Exception ex)
                {
                    ex.printStackTrace(); // todo: add exception to IDL and throw it here
                }
            }

            return entStructs;
        }


        /**
         * @see alma.demo.XmlComponentOperations#addNewSchedBlocks(alma.xmlentity.XmlEntityStruct[])
         */
        public void addNewSchedBlocks(XmlEntityStruct[] newSchedBlocks)
        {
    		String msg = "will add " + newSchedBlocks.length + " SchedBlock(s):\n";
    		for (int i = 0; i < newSchedBlocks.length; i++) {
    			msg += newSchedBlocks[i].xmlString + "\n";
    		}
    		m_ifTransLogger.info(msg);

    		if (m_useDefaultTranslator)
            {
                m_defaultTranslator.addNewSchedBlocks(newSchedBlocks);
            }
            else
            {
				m_ifTransLogger.log(Level.WARNING, "bummer, manual translation of newSchedBlocks array not implemented.");
            }
        }

        /**
         * @see alma.demo.XmlComponentOperations#getBestSchedBlock()
         */
        public XmlEntityStruct getBestSchedBlock()
        {
            XmlEntityStruct entStruct = null;

            if (m_useDefaultTranslator)
            {
                entStruct = m_defaultTranslator.getBestSchedBlock();
            }
            else
            {
                try
                {
                    SchedBlock sb = m_componentImpl.getBestSchedBlock();
                    entStruct = m_entSer.serializeEntity(sb, sb.getSchedBlockEntity());
                }
                catch (Exception e)
                {
                    e.printStackTrace(); // todo: add exception to IDL and throw it here
                }
            }

            return entStruct;
        }


        /**
         * @see alma.demo.XmlComponentOperations#xmlInOutMethod(alma.xmlentity.XmlEntityStruct, alma.xmlentity.XmlEntityStructHolder)
         */
        public void xmlInOutMethod(XmlEntityStruct opsPropIn, XmlEntityStructHolder schedBlockOut)
        {
            if (m_useDefaultTranslator)
            {
                m_defaultTranslator.xmlInOutMethod(opsPropIn, schedBlockOut);
            }
            else
            {
                try
                {
                    EntityDeserializer entdes = EntityDeserializer.getEntityDeserializer(m_ifTransLogger);
                    ObsProposal obsProp = (ObsProposal) entdes.deserializeEntity(opsPropIn, ObsProposal.class);
                    SchedBlockHolder sbh = new SchedBlockHolder();

                    m_componentImpl.xmlInOutMethod(obsProp, sbh);

                    SchedBlock schedBlock = sbh.value;
                    XmlEntityStruct schedStruct = m_entSer.serializeEntity(schedBlock, schedBlock.getSchedBlockEntity());
                    schedBlockOut.value = schedStruct;

                }
                catch (Exception ex)
                {
                    ex.printStackTrace(); // todo: add exception to IDL and throw it here
                }
            }
        }


        /**
         * @see alma.demo.XmlComponentOperations#getEntireTreeInAStruct()
         */
        public ObsProjectTree getEntireTreeInAStruct()
        {
            ObsProjectTree struct = null;

            if (m_useDefaultTranslator)
            {
                struct = m_defaultTranslator.getEntireTreeInAStruct();
            }
            else
            {
                ObsProjectTreeJ structJ = m_componentImpl.getEntireTreeInAStruct();

                struct = new ObsProjectTree();
                try
                {
                    struct.prop =  m_entSer.serializeEntity(structJ.prop, structJ.prop.getObsProposalEntity());

                    for (int i = 0; i < structJ.schedBlocks.length; i++)
                    {
                        SchedBlock sb = structJ.schedBlocks[i];
                        XmlEntityStruct xes = m_entSer.serializeEntity(sb, sb.getSchedBlockEntity());
                        struct.schedBlocks[i] = xes;
                    }
                }
                catch (EntityException e)
                {
					m_ifTransLogger.log(Level.WARNING, "exception in manual translation of entities.", e);
                }
            }
            return struct;
        }


        /**
         * @see alma.demo.XmlComponentOperations#sayHello()
         */
        public String sayHello()
        {
            return m_componentImpl.sayHello();
        }


        /* (non-Javadoc)
         * @see alma.demo.XmlComponentOperations#exceptionMethod()
         */
        public void exceptionMethod() throws XmlComponentErrorEx
        {
            m_defaultTranslator.exceptionMethod();
        }

		@Override
		public XmlOffshoot getOffshoot() {
			return m_defaultTranslator.getOffshoot();
		}

		@Override
		public Object getDefaultInterfaceTranslator() {
			return m_defaultTranslator;
		}

		@Override
		public void setDefaultInterfaceTranslator(
				Object defaultInterfaceTranslator) {
			// no-op
		}

		@Override
		public void activateOffshoot() {
			m_componentImpl.activateOffshoot();
		}

		@Override
		public void deactivateOffshoot() {
			m_componentImpl.deactivateOffshoot();
		}

    }
}


