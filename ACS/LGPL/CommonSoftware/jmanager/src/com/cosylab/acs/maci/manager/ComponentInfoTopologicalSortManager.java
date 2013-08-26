/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.manager;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.ContainerInfo;
import com.cosylab.acs.maci.HandleConstants;
import com.cosylab.acs.maci.IntArray;
import com.cosylab.acs.maci.RemoteException;

/**
 * Manages TS.
 */
@SuppressWarnings("unchecked")
public class ComponentInfoTopologicalSortManager implements Runnable {
	
	/**
	 * Components data store.
	 */
	private HandleDataStore components;
	
	/**
	 * Containers data store.
	 */
	private HandleDataStore containers;
	
	/**
	 * Stastus flag.
	 */
	private volatile boolean destroyed = false;
	
	/**
	 * Lock.
	 */
	private ReaderPreferenceReadWriteLock activationPendingRWLock;
	//private ReadWriteLock activationPendingRWLock;
	
	/**
	 * Dirty map.
	 */
	private HashSet dirtyContainerMap = new HashSet();

	/**
	 * Lock.
	 */
	private Object listLock = new Object();

	/**
	 * Current ordered TS list.
	 */
	private ComponentInfo[] currentTSList = new ComponentInfo[0];

	/**
	 * List of all pending container shutdowns.
	 */
	private Set pendingContainerShutdown;

	/**
	 * Thread pool (guarantees order of execution).
	 */
	private ThreadPoolExecutor threadPool;

	/**
	 * Logger.
	 */
	private Logger logger;

	/**
	 * @param components
	 * @param containers
	 */
	public ComponentInfoTopologicalSortManager(HandleDataStore components,
											   HandleDataStore containers,
											   /* ReadWriteLock */ ReaderPreferenceReadWriteLock activationPendingRWLock,
											   Set pendingContainerShutdown,
											   ThreadPoolExecutor threadPool,
											   Logger logger) {
		this.components = components;
		this.containers = containers;
		this.activationPendingRWLock = activationPendingRWLock;
		this.pendingContainerShutdown = pendingContainerShutdown;
		this.threadPool = threadPool;
		this.logger = logger;
		
		// make all containers dirty
		synchronized (containers)
		{
			int h = containers.first();
			while (h != 0)
			{
				int containerHandle = ((ContainerInfo)containers.get(h)).getHandle();
				synchronized (dirtyContainerMap)
				{
					dirtyContainerMap.add(new Integer(containerHandle));
				}
				h = containers.next(h);
			}
		}

		
		new Thread(this, "ComponentInfoTopologicalSortManager").start();
	}
	
	/**
	 * (Sync is clean since activationPendingRWLock is acquired and all
	 * notify request is issued only from its reader lock).
	 * @see java.lang.Runnable#run()
	 */
	public void run() {

		while (!destroyed)
		{
			
			
			synchronized (this)
			{
				int dirtyContainersCount;
				synchronized (dirtyContainerMap)
				{
					dirtyContainersCount = dirtyContainerMap.size();
				}

				if (dirtyContainersCount == 0)
				{
					try {
						this.wait();
					} catch (InterruptedException e) {
						return;
					}
				}				
			}
	
			if (destroyed)
				return;
			
			// aquire writer lock to prevent activation/deactivation
			activationPendingRWLock.writeLock().lock();
	
			try 
			{
				Integer[] conts;
				synchronized (dirtyContainerMap)
				{
					conts = new Integer[dirtyContainerMap.size()];
					dirtyContainerMap.toArray(conts);
					dirtyContainerMap.clear();
				}

				ComponentInfo[] orderedList; 
				synchronized (components)
				{
					List list = ComponentInfoTopologicalSort.sort(components);
					orderedList = new ComponentInfo[list.size()];
					list.toArray(orderedList);
				}
				
				synchronized (listLock) 
				{
					currentTSList = orderedList;
				}
				
				for (int i = 0; i < conts.length; i++)
				{
					int handle = conts[i].intValue() & HandleConstants.HANDLE_MASK;

					ContainerInfo containerInfo = null;
					synchronized (containers)
					{
						if (containers.isAllocated(handle))
							containerInfo = (ContainerInfo)containers.get(handle);
					}
					
					if (containerInfo == null || containerInfo.getName() == null)
						break;
					
					String containerName = containerInfo.getName();

					// check if shutdown state
					if (pendingContainerShutdown.contains(containerName))
						break;

					IntArray containerOrderdList = new IntArray(10);
					for (int j = 0; j < orderedList.length; j++)
					{
						// if component already deactivated, skip it
						if (orderedList[j].getContainer() == 0)
							continue;
						
						// this is null proof
						if (containerName.equals(orderedList[j].getContainerName()))
						{
							containerOrderdList.add(orderedList[j].getHandle());
						}
					}
					
					// optimization
					if (containerOrderdList.size() > 1)
						notifyContainerShutdownOrder(containerInfo, containerOrderdList.toArray());
				}
			}
			finally 
			{
				activationPendingRWLock.writeLock().unlock();
			}

		}
	}
	
	/**
	 * Request topological sort.
	 */
	public void requestTopologicalSort() {

		// aquire writer lock to prevent activation/deactivation
		activationPendingRWLock.writeLock().lock();

		try 
		{
			ComponentInfo[] orderedList; 
			synchronized (components)
			{
				List list = ComponentInfoTopologicalSort.sort(components);
				orderedList = new ComponentInfo[list.size()];
				list.toArray(orderedList);
			}
			
			synchronized (listLock) 
			{
				currentTSList = orderedList;
			}
			
		}
		finally 
		{
			activationPendingRWLock.writeLock().unlock();
		}

	}

	/**
	 * Notify about (possible) topology change and initiates sorting.
	 * @param containerHandleToNotify container handle to notify.
	 */
	public synchronized void notifyTopologyChange(int containerHandleToNotify)
	{
		synchronized (dirtyContainerMap)
		{
			dirtyContainerMap.add(new Integer(containerHandleToNotify));
		}
		this.notify();
	}
	
	/**
	 * Get component shutdown order for container.
	 * @param containerInfo	valid container's info, if <code>null</code> complete TS is returned.
	 * @return component shutdown order
	 */
	public ComponentInfo[] getComponentShutdownOrder(ContainerInfo containerInfo)
	{
		if (containerInfo == null)
		{
			synchronized (listLock) {
				return currentTSList;
			}
		}
		
		String containerName = containerInfo.getName();

		ComponentInfo[] orderedList;
		synchronized (listLock) {
			orderedList = currentTSList;
		}
		
		List containerOrderdList = new ArrayList(10);
		for (int j = 0; j < orderedList.length; j++)
		{
			// if component already deactivated, skip it
			if (orderedList[j].getContainer() == 0)
				continue;
			
			// this is null proof
			if (containerName.equals(orderedList[j].getContainerName()))
			{
				containerOrderdList.add(orderedList[j]);
			}
		}
			
		ComponentInfo[] retVal = new ComponentInfo[containerOrderdList.size()];
		containerOrderdList.toArray(retVal);
		return retVal;
	}

	/**
	 * Destroy (terminate thread).
	 */
	public synchronized void destroy()
	{
		destroyed = true;
		this.notify();
	}
	
	/**
	 * Informs containers abouts its component shutdown order.
	 * @param	containerInfo	container to inform
	 * @param	handles			ordered list of component handles
	 */
	private Map pendingContainerNotifications = new HashMap();
	private void notifyContainerShutdownOrder(ContainerInfo containerInfo, int[] handles)
	{
		
		/**
		 * Task thats invokes <code>Container#shutdown</code> method.
		 */		
		class ContainerSetShutdownOrderTask implements Runnable
		{
			private boolean done = false;
			private int[] handles;
			private ContainerInfo containerInfo;
			
			public ContainerSetShutdownOrderTask(ContainerInfo containerInfo, int[] handles)
			{
				this.containerInfo = containerInfo;
				this.handles = handles;
			}

			/**
			 * Add new notification. This will keep only the latest one.
			 * @param handles
			 * @return <code>true<code> if notification is being accepted
			 */
			public synchronized boolean addNotification(int[] handles)
			{
				if (done)
					return false;
				
				this.handles = handles;
				return true;
			}
			
			public void run()
			{
				final int MAX_RETRIES = 3;

				for (int retries = 0; retries < MAX_RETRIES; retries++)
				{
					int[] hs = null;
					try
					{
						synchronized (this)
						{
							hs = handles;
						}

						(containerInfo.getContainer()).set_component_shutdown_order(hs);
						
						synchronized (this)
						{
							if (handles == hs)
							{
								// no new handles, done...
								done = true;
								break;
							}
							else
							{
								// all OK, new handles, reset retries
								retries = 0;
								continue;
							}
						}
					}
					catch (RemoteException re)
					{
						logger.log(Level.SEVERE, 
								"RemoteException caught while invoking 'Container.set_component_shutdown_order' on "+containerInfo+".", re);
					}
					catch (Throwable ex)
					{
						logger.log(Level.SEVERE, 
								"Unhandled exception caught while invoking 'Container.set_component_shutdown_order' on "+containerInfo+".", ex);
					}
					
					// decrease retries, if we have a new set of handles
					synchronized (this)
					{
						if (handles != hs)
							retries--;
					}					
				}
				
			}
		}

		
		synchronized (pendingContainerNotifications)
		{
			// get or create task
			ContainerSetShutdownOrderTask task = (ContainerSetShutdownOrderTask)pendingContainerNotifications.get(containerInfo);
			if (task == null || !task.addNotification(handles))
			{
				// spawn new task which surely does not block
				try
				{
					threadPool.execute(new ContainerSetShutdownOrderTask(containerInfo, handles));
				} catch (RejectedExecutionException ree) {
					// noop (threadPool) most likely in shutdown state
				}
			}
		}


	}

}
