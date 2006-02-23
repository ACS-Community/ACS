/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.cdb.dal.demo;

import abeans.core.InitializationException;
import abeans.framework.ApplicationContext;
import abeans.models.acs.cdb.dal.DAOChannel;

import com.cosylab.abeans.AbeansEngine;

/**
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class AbeansDALDemoEngine extends AbeansEngine
{

	private ApplicationContext applicationContext = null;
	private DAOChannel channel = null;

	/**
	 * Constructor for AbeansDALDemoEngine.
	 */
	public AbeansDALDemoEngine()
	{
		super();
	}

	/**
	 * @see com.cosylab.abeans.AbeansEngine#getName()
	 */
	public String getName()
	{
		return "AbeansDALDemoEngine";
	}

	/**
	 * @see com.cosylab.abeans.AbeansEngine#userFrameworkInitialize()
	 */
	protected void userFrameworkInitialize()
	{
		applicationContext = getApplicationContext();
	}

	/**
	 * @see com.cosylab.abeans.AbeansEngine#userDestroy()
	 */
	protected void userDestroy()
	{
	}

	/**
	 * @see com.cosylab.abeans.AbeansEngine#userAllInitializationsDone()
	 */
	protected void userAllInitializationsDone()
	{
	}

	/**
	 * @see abeans.framework.ApplicationEngine#getModelName()
	 */
	public String getModelName()
	{
		return "Channel";
	}

	public DAOChannel getChannel(String path) throws InitializationException
	{
		if (channel == null) channel = new DAOChannel(applicationContext.getDefaultFamily(), applicationContext.createRemoteInfo(path));
		return channel;
	}

}
