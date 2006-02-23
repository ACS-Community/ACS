/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.cdb.dal.demo;

import java.awt.GridLayout;

import javax.swing.JApplet;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;

import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.models.acs.cdb.dal.DAOChannel;

import com.cosylab.abeans.AbeansEngine;
import com.cosylab.abeans.AbeansLaunchable;
import com.cosylab.abeans.AbeansLauncher;
import com.cosylab.abeans.plugins.AbeansExceptionPanelPlugIn;
import com.cosylab.abeans.plugins.AbeansStandardActionsPlugIn;
import com.cosylab.abeans.plugins.AbeansSystemMenuPlugIn;
import com.cosylab.abeans.plugins.AboutPlugIn;
import com.cosylab.abeans.plugins.LoggingPlugIn;
import com.cosylab.abeans.plugins.ReportAreaPlugIn;
import com.cosylab.gui.framework.Desktop;
import com.cosylab.gui.framework.Launcher;
import com.cosylab.gui.framework.LauncherEnvironment;
import com.cosylab.gui.plugins.CosyStandardActionsPlugIn;
import com.cosylab.gui.plugins.VitragePlugIn;

/**
 * Overall test and demo app.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class AbeansDALDemo extends AbeansLaunchable implements Identifiable
{

	private AbeansDALDemoEngine engine = null;

	/**
	 * Constructor for AbeansDALDemo.
	 */
	public AbeansDALDemo()
	{
		super();
	}

	/**
	 * Constructor for AbeansDALDemo.
	 * @param launcher
	 * @param env
	 * @param owner
	 */
	public AbeansDALDemo(
		Launcher launcher,
		LauncherEnvironment env,
		JFrame owner)
	{
		super(launcher, env, owner);
	}

	/**
	 * Constructor for AbeansDALDemo.
	 * @param launcher
	 * @param env
	 * @param desk
	 * @param owner
	 */
	public AbeansDALDemo(
		Launcher launcher,
		LauncherEnvironment env,
		Desktop desk,
		JInternalFrame owner)
	{
		super(launcher, env, desk, owner);
	}

	/**
	 * Constructor for AbeansDALDemo.
	 * @param launcher
	 * @param env
	 * @param owner
	 */
	public AbeansDALDemo(
		Launcher launcher,
		LauncherEnvironment env,
		JApplet owner)
	{
		super(launcher, env, owner);
	}

	/**
	 * @see com.cosylab.abeans.AbeansLaunchable#getAbeansEngine()
	 */
	public AbeansEngine getAbeansEngine()
	{
		if (engine == null)
			engine = new AbeansDALDemoEngine();
		return engine;
	}

	public void userInitializeGUI()
	{
		setLayout(new GridLayout());
		//add(getGauger());
		//add(getWheelswitch());
		setSize(320,200);   

		
		try
		{
			// install ACS LoggingExceptionHandlerService
			// Root.getRoot().getComponentManager().installComponent(LoggingExceptionHandlerService.class);

			// install CORBA Service
			// Root.getRoot().getComponentManager().installComponent(DefaultCORBAService.class);

			// install CORBA Naming Service Remote Directory
			//Root.getRoot().getComponentManager().installComponent(NamingServiceRemoteDirectory.class);

			// install ACS Remote Logging Service
			// getRoot().getComponentManager().installComponent(RemoteLoggingService.class);


			DAOChannel cobs = engine.getChannel("MACI/Components");
			String[] names = cobs.getValue();
			for (int i = 0; i < names.length; i++)
				System.out.println("\t"+names[i]);

			/*
			names = (String[])cobs.getStringSeqCharacteristic("MOUNT1");
			for (int i = 0; i < names.length; i++)
				System.out.println("\t"+names[i]);
			*/
			
			String code = (String)cobs.getCharacteristic("MOUNT1/Code");
			System.out.println("\t\t"+code);

			String type = (String)cobs.getCharacteristic("MOUNT1/Type");
			System.out.println("\t\t"+type);

			String activator = (String)cobs.getCharacteristic("MOUNT1/Container");
			System.out.println("\t\t"+activator);

/*
			Exception ex = new IllegalArgumentException("source of all exceptions");
			ex = new AssertionFailed(this, "exception #1", ex);
			AssertionFailed af = new AssertionFailed(this, "exception #2", ex);
			af.caughtIn(this, "userInitializeGUI");
			ex = af;
			new MessageLogEntry(this, "testFormatter", "ERROR message", ex, Level.SEVERE).dispatch();

			ReportEvent re = new ReportEvent(getAbeansEngine().getApplicationContext(), "exception report");
			re.setException(ex);
			re.dispatch();

			DAOChannel channel = engine.getChannel("alma/PBEND_B_01");
			
			String text = "current/format: "+ channel.getStringCharacteristic("current/format");
			System.out.println(text);

			text = "readback/graph_max: "+ channel.getDoubleCharacteristic("readback/graph_max");
			System.out.println(text);

			int[] l = channel.getIntegerSeqCharacteristic("status/whenSet");
			System.out.println("status/whenSet: ");
			for (int i=0; i<l.length; i++)
				System.out.println("\t"+l[i]);

			double[] d = channel.getDoubleSeqCharacteristic("status/whenSet");
			System.out.println("status/whenSet: ");
			for (int i=0; i<d.length; i++)
				System.out.println("\t"+d[i]);

			String[] s = channel.getStringSeqCharacteristic("status/bitDescription");
			System.out.println("status/bitDescription");
			for (int i=0; i<s.length; i++)
				System.out.println("\t"+s[i]);

			s = channel.getValue();
			System.out.println("getValue");
			for (int i=0; i<s.length; i++)
				System.out.println("\t"+s[i]);

			s = channel.getStringSeqCharacteristic("status");
			System.out.println("status");
			for (int i=0; i<s.length; i++)
				System.out.println("\t"+s[i]);
*/
		} catch (Exception e)
		{
			e.printStackTrace(System.err);
		}


	}
	
	public void userInitializePlugIns()
	{
		try
		{
			installPlugIn(AboutPlugIn.class);
			installPlugIn(VitragePlugIn.class);
			installPlugIn(AbeansExceptionPanelPlugIn.class);
			//installPlugIn(TreeBrowserPlugIn.class);
			installPlugIn(ReportAreaPlugIn.class);
			installPlugIn(LoggingPlugIn.class);
			installPlugIn(AbeansSystemMenuPlugIn.class);
			installPlugIn(AbeansStandardActionsPlugIn.class);
			installPlugIn(CosyStandardActionsPlugIn.class);
		} catch (Exception e)
		{
			e.printStackTrace();
		}
	}
  
	public static void main(String[] args)
	{
			AbeansLauncher.main( new String[] { "com.cosylab.acs.cdb.dal.demo.AbeansDALDemo" } );
	}


	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		return getAbeansEngine().getIdentifier();
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return getAbeansEngine().isDebug();
	}

}
