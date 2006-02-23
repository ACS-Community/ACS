/*
 * Copyright (c) 2003 by Cosylab d.o.o.
 *
 * The full license specifying the redistribution, modification, usage and other
 * rights and obligations is included with the distribution of this project in
 * the file license.html. If the license is not included you may find a copy at
 * http://www.cosylab.com/legal/abeans_license.htm or may write to Cosylab, d.o.o.
 *
 * THIS SOFTWARE IS PROVIDED AS-IS WITHOUT WARRANTY OF ANY KIND, NOT EVEN THE
 * IMPLIED WARRANTY OF MERCHANTABILITY. THE AUTHOR OF THIS SOFTWARE, ASSUMES
 * _NO_ RESPONSIBILITY FOR ANY CONSEQUENCE RESULTING FROM THE USE, MODIFICATION,
 * OR REDISTRIBUTION OF THIS SOFTWARE.
 */

package dartboard;

import abeans.datatypes.DoubleProperty;
import abeans.datatypes.LongProperty;

import com.cosylab.abeans.adapters.DoublePropertyAdapter;
import com.cosylab.abeans.adapters.LongPropertyAdapter;

import com.cosylab.gui.components.util.IconHelper;
import com.cosylab.gui.components.util.PopupManager;
import com.cosylab.gui.core.CosyApplicationPanel;
import com.cosylab.gui.displayers.CommonDisplayer;
import com.cosylab.gui.displayers.DataConsumer;
import com.cosylab.gui.displayers.DataState;
import com.cosylab.gui.displayers.DoubleConsumer;
import com.cosylab.gui.displayers.LongConsumer;

import com.cosylab.util.CommonException;

import java.awt.Dimension;
import java.awt.event.ActionEvent;

import java.beans.Beans;
import java.beans.PropertyVetoException;

import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.Action;


/**
 * 
 * @author <a href="mailto:jernej.kamenik@cosylab.com">Jernej Kamenik</a>
 */
public class DartboardBean extends DartboardPanel implements CommonDisplayer,
	java.io.Serializable
{
	private abstract class ConsumerImpl implements DoubleConsumer, LongConsumer
	{
		private String name;
		private String[] characteristics = new String[]{  };
		private Class[] types = new Class[]{
				DoubleConsumer.class, LongConsumer.class
			};

		/* (non-Javadoc)
		 *
		 * @param string
		 */
		public ConsumerImpl(String string)
		{
			this.name = string;
		}

		/* (non-Javadoc)
		 * @see com.cosylab.gui.displayers.DataConsumer#setCharacteristics(java.util.Map)
		 */
		public void setCharacteristics(Map characteristics)
		{
		}

		/* (non-Javadoc)
		 * @see com.cosylab.gui.displayers.DataConsumer#getDataConsumer(java.lang.Class)
		 */
		public DataConsumer getDataConsumer(Class type)
		{
			if (type.equals(DoubleConsumer.class)
			    || type.equals(LongConsumer.class)) {
				return this;
			}

			return null;
		}

		/* (non-Javadoc)
		 * @see com.cosylab.gui.displayers.DataConsumer#getDefaultDataConsumer()
		 */
		public DataConsumer getDefaultDataConsumer()
		{
			return this;
		}

		/* (non-Javadoc)
		 * @see com.cosylab.gui.displayers.DataConsumer#getName()
		 */
		public String getName()
		{
			return name;
		}

		/* (non-Javadoc)
		 * @see com.cosylab.gui.displayers.DataConsumer#getSupportedCharacteristics()
		 */
		public String[] getSupportedCharacteristics()
		{
			return characteristics;
		}

		/* (non-Javadoc)
		 * @see com.cosylab.gui.displayers.DataConsumer#getSupportedConsumerTypes()
		 */
		public Class[] getSupportedConsumerTypes()
		{
			return types;
		}

		/* (non-Javadoc)
		 * @see com.cosylab.gui.displayers.DataConsumer#updateDataState(com.cosylab.gui.displayers.DataState)
		 */
		public void updateDataState(DataState state)
		{
			// TODO Auto-generated method stub
			DartboardBean.this.state = state;
		}

		/* (non-Javadoc)
		 * To be overriden in subimplementations.
		 * @see com.cosylab.gui.displayers.DoubleConsumer#updateValue(long, double)
		 */
		public void updateValue(long timestamp, double value)
			throws CommonException
		{
		}

		/* (non-Javadoc)
		 * To be overriden in subimplementations.
		 * @see com.cosylab.gui.displayers.LongConsumer#updateValue(long, long)
		 */
		public void updateValue(long timestamp, long value)
			throws CommonException
		{
		}
	}

	private class ResumeAction extends AbstractAction
	{
		/**
		 * Creates a new ResumeAction object.
		 */
		public ResumeAction()
		{
			super("Resume",
			    IconHelper.createIcon("Resources/icons/media/Play16.gif"));
		}

		/* (non-Javadoc)
		 * 
		 */
		public void actionPerformed(ActionEvent e)
		{
			if (isSuspended()) {
				resume();
			}
		}
	}

	private class SuspendAction extends AbstractAction
	{
		/**
		 * Creates a new SuspendAction object.
		 */
		public SuspendAction()
		{
			super("Suspend",
			    IconHelper.createIcon("Resources/icons/media/Pause16.gif"));
		}

		/* (non-Javadoc)
		 * 
		 */
		public void actionPerformed(ActionEvent e)
		{
			if (!isSuspended()) {
				suspend();
			}
		}
	}

	private DoublePropertyAdapter moonAzimuth = new DoublePropertyAdapter();
	private DoublePropertyAdapter moonDeclination = new DoublePropertyAdapter();
	private DoublePropertyAdapter moonElevation = new DoublePropertyAdapter();
	private DoublePropertyAdapter moonRightAscension = new DoublePropertyAdapter();
	private DoublePropertyAdapter sunAzimuth = new DoublePropertyAdapter();
	private DoublePropertyAdapter sunDeclination = new DoublePropertyAdapter();
	private DoublePropertyAdapter sunElevation = new DoublePropertyAdapter();
	private DoublePropertyAdapter sunRightAscension = new DoublePropertyAdapter();
	private DoublePropertyAdapter telescopeAzimuth = new DoublePropertyAdapter();
	private DoublePropertyAdapter telescopeDestinationAzimuth = new DoublePropertyAdapter();
	private DoublePropertyAdapter telescopeDestinationElevation = new DoublePropertyAdapter();
	private DoublePropertyAdapter telescopeElevation = new DoublePropertyAdapter();
	private DoublePropertyAdapter windDirection = new DoublePropertyAdapter();
	private DoublePropertyAdapter windSpeed = new DoublePropertyAdapter();
	private LongPropertyAdapter moonPhase = new LongPropertyAdapter();

	private CosyApplicationPanel panel;
	private PopupManager manager;
	private DataState state = new DataState(DataState.NORMAL);
	private Action resumeAction = new ResumeAction();
	private Action suspendAction = new SuspendAction();
	private int suspended;
	private boolean propertiesInRadians=false;

	/**
	 * SimulatorBean constructor comment.
	 */
	public DartboardBean()
	{
		super();
		initialize();
	}

	/**
	 *
	 * @param property
	 */
	public void setMoonAzimuth(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			moonAzimuth.setDoubleProperty(property);
		}
	}

	/**
	 * Insert the method's description here. Creation date: (11/12/00 10:35:11
	 * AM)
	 *
	 * @return si.ijs.anka.abeans.datatypes.DoubleProperty
	 */
	public DoubleProperty getMoonAzimuth()
	{
		return moonAzimuth.getDoubleProperty();
	}

	/**
	 *
	 * @param property
	 */
	public void setMoonDeclination(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			moonDeclination.setDoubleProperty(property);
		}
	}

	/**
	 * 
	 */
	public DoubleProperty getMoonDeclination()
	{
		return moonDeclination.getDoubleProperty();
	}

	/**
	 * 
	 */
	public void setMoonElevation(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			moonElevation.setDoubleProperty(property);
		}
	}

	/**
	 * 
	 */
	public DoubleProperty getMoonElevation()
	{
		return moonElevation.getDoubleProperty();
	}

	/**
	 * 
	 */
	public void setMoonPhase(LongProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			moonPhase.setLongProperty(property);
		}
	}

	/**
	 * 
	 */
	public LongProperty getMoonPhase()
	{
		return moonPhase.getLongProperty();
	}

	/**
	 * 
	 */
	public void setMoonRightAscension(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			moonRightAscension.setDoubleProperty(property);
		}
	}

	/**
	 * 
	 */
	public DoubleProperty getMoonRightAscension()
	{
		return moonRightAscension.getDoubleProperty();
	}

	/**
	 * 
	 */
	public void setSunAzimuth(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			sunAzimuth.setDoubleProperty(property);
		}
	}

	/**
	 * 
	 */
	public DoubleProperty getSunAzimuth()
	{
		return sunAzimuth.getDoubleProperty();
	}

	/**
	 * 
	 */
	public void setSunDeclination(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			sunDeclination.setDoubleProperty(property);
		}
	}

	/**
	 * 
	 */
	public DoubleProperty getSunDeclination()
	{
		return sunDeclination.getDoubleProperty();
	}

	/**
	 * 
	 */
	public void setSunElevation(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			sunElevation.setDoubleProperty(property);
		}
	}

	/**
	 * 
	 */
	public DoubleProperty getSunElevation()
	{
		return sunElevation.getDoubleProperty();
	}

	/**
	 * 
	 */
	public void setSunRightAscension(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			sunRightAscension.setDoubleProperty(property);
		}
	}

	/**
	 * 
	 */
	public DoubleProperty getSunRightAscension()
	{
		return sunRightAscension.getDoubleProperty();
	}

	/**
	 * 
	 */
	public void setTelescopeAzimuth(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			telescopeAzimuth.setDoubleProperty(property);
		}
	}

	/**
	 * 
	 */
	public DoubleProperty getTelescopeAzimuth()
	{
		return telescopeAzimuth.getDoubleProperty();
	}

	/**
	 * 
	 */
	public void setTelescopeDestinationAzimuth(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			telescopeDestinationAzimuth.setDoubleProperty(property);
		}
	}

	/**
	 * 
	 */
	public DoubleProperty getTelescopeDestinationAzimuth()
	{
		return telescopeDestinationAzimuth.getDoubleProperty();
	}

	/**
	 * 
	 */
	public void setTelescopeDestinationElevation(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			telescopeDestinationElevation.setDoubleProperty(property);
		}
	}

	/**
	 * 
	 */
	public DoubleProperty getTelescopeDestinationElevation()
	{
		return telescopeDestinationElevation.getDoubleProperty();
	}

	/**
	 * 
	 */
	public void setTelescopeElevation(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			telescopeElevation.setDoubleProperty(property);
		}
	}

	/**
	 * 
	 */
	public DoubleProperty getTelescopeElevation()
	{
		return telescopeElevation.getDoubleProperty();
	}

	/**
	 * 
	 */
	public void setWindDirection(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			windDirection.setDoubleProperty(property);
		}
	}

	/**
	 * 
	 */
	public DoubleProperty getWindDirection()
	{
		return windDirection.getDoubleProperty();
	}

	/**
	 * 
	 */
	public void setWindSpeed(DoubleProperty property)
	{
		if (Beans.isDesignTime()) {
			return;
		} else {
			windSpeed.setDoubleProperty(property);
		}
	}

	/**
	 * 
	 */
	public DoubleProperty getWindSpeed()
	{
		return windSpeed.getDoubleProperty();
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.core.CosyComponent#setCosyPanelParent(com.cosylab.gui.core.CosyApplicationPanel)
	 */
	public void setCosyPanelParent(CosyApplicationPanel panel)
	{
		this.panel = panel;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.core.CosyComponent#getCosyPanelParent()
	 */
	public CosyApplicationPanel getCosyPanelParent()
	{
		return panel;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.displayers.CommonDisplayer#getDataState()
	 */
	public DataState getDataState()
	{
		return state;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.components.util.PopupManageable#getPopupManager()
	 */
	public PopupManager getPopupManager()
	{
		if (manager == null) {
			manager = new PopupManager(this);
			manager.addAction(suspendAction);
			manager.addAction(resumeAction);
		}

		return manager;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.displayers.CommonDisplayer#cleanup()
	 */
	public void cleanup()
	{
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.core.CosyComponent#destroy()
	 */
	public void destroy()
	{
		moonAzimuth = null;

		moonDeclination = null;

		moonElevation = null;

		moonPhase = null;

		moonRightAscension = null;

		sunAzimuth = null;

		sunDeclination = null;

		sunElevation = null;

		sunRightAscension = null;

		telescopeAzimuth = null;

		telescopeDestinationAzimuth = null;

		telescopeDestinationElevation = null;

		telescopeElevation = null;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.displayers.CommonDisplayer#resume()
	 */
	public void resume()
	{
		if (suspended > 0) {
			suspended--;

			if (suspended == 0) {
				resumeAction.setEnabled(false);
				suspendAction.setEnabled(true);
				setEnabled(true);
			}
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.displayers.CommonDisplayer#suspend()
	 */
	public void suspend()
	{
		suspended++;

		if (suspended == 1) {
			resumeAction.setEnabled(true);
			suspendAction.setEnabled(false);
			setEnabled(false);
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.gui.displayers.CommonDisplayer#isSuspended()
	 */
	public boolean isSuspended()
	{
		return suspended > 0;
	}

	private void initialize()
	{
		try {
			moonAzimuth.addConsumer(new ConsumerImpl("Moon Azimuth") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setMoonAzimuth(convert(value));
						}
					}
				});

			moonDeclination.addConsumer(new ConsumerImpl("Moon Declination") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setMoonDeclination(convert(value));
						}
					}
				});

			moonElevation.addConsumer(new ConsumerImpl("Moon Elevation") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setMoonElevation(convert(value));
						}
					}
				});

			moonPhase.addConsumer(new ConsumerImpl("Moon Phase") {
					public void updateValue(long timestamp, long value)
						throws CommonException
					{
						if (!isSuspended()) {
							setMoonPhase((int)value);
						}
					}
				});

			moonRightAscension.addConsumer(new ConsumerImpl(
			        "Moon Right Ascension") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setMoonRightAscension(convert(value));
						}
					}
				});

			sunAzimuth.addConsumer(new ConsumerImpl("Sun Azimuth") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setSunAzimuth(convert(value));
						}
					}
				});

			sunDeclination.addConsumer(new ConsumerImpl("Sun Declination") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setSunDeclination(convert(value));
						}
					}
				});

			sunElevation.addConsumer(new ConsumerImpl("Sun Elevation") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setSunElevation(convert(value));
						}
					}
				});

			sunRightAscension.addConsumer(new ConsumerImpl(
			        "Sun Right Ascension") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setSunRightAscension(convert(value));
						}
					}
				});

			telescopeAzimuth.addConsumer(new ConsumerImpl("Telescope Azimuth") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setTelescopeAzimuth(convert(value));
						}
					}
				});

			telescopeDestinationAzimuth.addConsumer(new ConsumerImpl(
			        "Telescope Destination Azimuth") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setTelescopeDestinationAzimuth(convert(value));
						}
					}
				});

			telescopeDestinationElevation.addConsumer(new ConsumerImpl(
			        "Telescope Destination Elevation") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setTelescopeDestinationElevation(convert(value));
						}
					}
				});

			telescopeElevation.addConsumer(new ConsumerImpl(
			        "Telescope Elevation") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setTelescopeElevation(convert(value));
						}
					}
				});

			windDirection.addConsumer(new ConsumerImpl("Wind Direction") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setWindDirection(convert(value));
						}
					}
				});

			windSpeed.addConsumer(new ConsumerImpl("Wind Speed") {
					public void updateValue(long timestamp, double value)
						throws CommonException
					{
						if (!isSuspended()) {
							setWindSpeed(value);
						}
					}
				});
		} catch (PropertyVetoException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		setPreferredSize(new Dimension(300, 300));
		resumeAction.setEnabled(false);
		addMouseListener(getPopupManager().getMouseHook());
	}
	/**
	 * Returns <code>true</code> if remote coordinate properties are returning values in 
	 * radians. This means, that all remote coordinates are converted from radians 
	 * to degrees before applied to the Dartboard.
	 * @return <code>true</code> if remote coordinate properties are returning values in 
	 * radians
	 */
	public boolean isPropertiesInRadians() {
		return propertiesInRadians;
	}
	/**
	 * Set to <code>true</code> if remote coordinate properties are returning values in 
	 * radians. This means, that all remote coordinates are converted from radians 
	 * to degrees before applied to the Dartboard.
	 * @param propertiesInRadians new flag for remoe coordinates in radians
	 */
	public void setPropertiesInRadians(boolean propertiesInRadians) {
		this.propertiesInRadians = propertiesInRadians;
	}
	
	/**
	 * If necessary converts coordinate value from radians to degrees.
	 * @param val value in degrees or radians
	 * @return value in degrees
	 */
	private final double convert(double val) {
		if (propertiesInRadians) {
			return Math.toDegrees(val);
		}
		return val;
	}
}

/* __oOo__ */
