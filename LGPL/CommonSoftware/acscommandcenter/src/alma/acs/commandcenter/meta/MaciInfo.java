/*
 * Created on Oct 10, 2006 by mschilli
 */
package alma.acs.commandcenter.meta;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;

import com.cosylab.acs.maci.HandleConstants;



public class MaciInfo extends DefaultTreeModel {


	// ====================================================================
	// Public API
	// This is for the benefit of a client like Exec that in some way or
	// another analyzes the contents of the MaciInfo. To not force them
	// to work closely on our tree node structure, this public API provides
	// some logical view onto us.
	
	public List<ContainerInfo> getContainers() {
		// grab reference as it is in this very moment, this is atomic.
		List<ContainerInfo> current = containers;

		List<ContainerInfo> ret = new ArrayList<ContainerInfo>(current);
		return ret;
	}

	public ContainerInfo getContainer (String name) {
		// grab reference as it is in this very moment, this is atomic.
		List<ContainerInfo> current = containers;

		for (ContainerInfo info : current)
			if (info.name.equals(name))
				return info;
		return null;
	}

	public ContainerInfo getContainer (int handle) {
		// grab reference as it is in this very moment, this is atomic.
		List<ContainerInfo> current = containers;

		for (ContainerInfo info : current)
			if (info.h == handle)
				return info;
		return null;
	}
	
	public List<ClientInfo> getClients() {
		// grab reference as it is in this very moment, this is atomic.
		List<ClientInfo> current = clientApps;

		List<ClientInfo> ret = new ArrayList<ClientInfo>(current);
		return ret;
	}

	public ClientInfo getClient (String name) {
		// grab reference as it is in this very moment, this is atomic.
		List<ClientInfo> current = clientApps;
		
		for (ClientInfo info : current)
			if (info.name.equals(name))
				return info;
		return null;
	}

	public ClientInfo getClient (int handle) {
		// grab reference as it is in this very moment, this is atomic.
		List<ClientInfo> current = clientApps;
		
		for (ClientInfo info : current)
			if (info.h == handle)
				return info;
		return null;
	}

	public List<ComponentInfo> getComponents () {
		// grab reference as it is in this very moment, this is atomic.
		List<ComponentInfo> current = components;
		
		List<ComponentInfo> ret = new ArrayList<ComponentInfo>(current);
		return ret;
	}

	public ComponentInfo getComponent (String name) {
		// grab reference as it is in this very moment, this is atomic.
		List<ComponentInfo> current = components;
		
		for (ComponentInfo info : current)
			if (info.name.equals(name))
				return info;
		return null;
	}

	public ComponentInfo getComponent (int handle) {
		// grab reference as it is in this very moment, this is atomic.
		List<ComponentInfo> current = components;
		
		for (ComponentInfo info : current)
			if (info.h == handle)
				return info;
		return null;
	}

	public List<ComponentInfo> getStartedComponents () {
		// grab reference as it is in this very moment, this is atomic.
		List<ComponentInfo> current = components;
		
		List<ComponentInfo> ret = new ArrayList<ComponentInfo>(current.size());
		for (ComponentInfo info : current)
			if (info.h != 0) // skip non-activated components
				ret.add(info);
		return ret;
	}


	// when new data comes in, each of these references is rewritten,
	// which is atomic per se and will trigger a memory flush.
   volatile List<ComponentInfo> components = Collections.EMPTY_LIST;
   volatile List<ContainerInfo> containers = Collections.EMPTY_LIST;
   volatile List<ClientInfo> clientApps = Collections.EMPTY_LIST;

	// ====================================================================
	// Not-so-public API
	// These are the portions used by the MaciSupervisor to construct the
	// MaciInfo, and by commandcenter's DeploymentTree to present it.
	
	
	protected MaciInfo () {
		super(null);
		root = managerNode = createNode("Manager");
		managerNode.add(containerNode = createNode(new FolderInfo("Containers")));
		managerNode.add(clientNode = createNode(new FolderInfo("Client Applications")));
		managerNode.add(componentNode = createNode(new FolderInfo("Components")));
	}

   protected final SortingTreeNode managerNode;
   protected final SortingTreeNode containerNode;
   protected final SortingTreeNode clientNode;
   protected final SortingTreeNode componentNode;



   /**
    * Sets the components, containers, and clients.
    * The given lists must not be changed anymore (otherwise 
    * this method would have to make a copy of them).
    */
   protected void setContents (
   		List<ComponentInfo> newComponents,
   		List<ContainerInfo> newContainers,
   		List<ClientInfo>    newClientApps,
   		Map<Object,String>  auxiliary) {
		
   	
   	// bend references, each assignment is atomic and triggers a flush.
		// note we shall not modify the lists anymore after this point! 
		this.components = newComponents;
		this.containers = newContainers;
		this.clientApps = newClientApps;

   	// re-populate the toplevel nodes
   	// ---------------------------------------------

   	componentNode.removeAllChildren();
		for (ComponentInfo comp : newComponents) {
			SortingTreeNode n = createNode(comp);
			addInfoNodes(n, auxiliary);
			componentNode.add(n);
		}

		containerNode.removeAllChildren();
		for (ContainerInfo cont : newContainers) {
			SortingTreeNode n = createNode(cont);
			addInfoNodes(n, auxiliary);
			// attach components that are active in this container
			for (ComponentInfo comp : newComponents) {
				if (comp.container == cont.h && comp.h != 0)
					n.add(createNode(comp));
			}
			containerNode.add(n);
		}

		clientNode.removeAllChildren();
		for (ClientInfo client : newClientApps) {
			SortingTreeNode n = createNode(client);
			addInfoNodes(n, auxiliary);
			clientNode.add(n);
		}


		// we sort - for some great user experience
		componentNode.sortChildrenByName();
		containerNode.sortChildrenByName();
		clientNode.sortChildrenByName();

		// send out change event
		nodeStructureChanged(managerNode);
   }


	/**
	 * Factory method
	 */
	protected SortingTreeNode createNode (Object info) {
		SortingTreeNode ret = new SortingTreeNode();

		if (info instanceof ContainerInfo) {
			ret.setUserObject(info);
			ContainerInfo casted = (ContainerInfo) info;
			if (casted.h != 0)
				ret.representedHandles = new int[]{casted.h};
			
		} else
		if (info instanceof ClientInfo) {
			ret.setUserObject(info);
			ClientInfo casted = (ClientInfo) info;
			if (casted.h != 0)
				ret.representedHandles = new int[]{casted.h};
			
		} else
		if (info instanceof ComponentInfo) {
			ret.setUserObject(info);
			ComponentInfo casted = (ComponentInfo) info;
			if (casted.h != 0)
				ret.representedHandles = new int[]{casted.h};
			
		} else
		if (info instanceof InfoDetail) {
			InfoDetail casted = (InfoDetail) info;
			ret.setUserObject(info);
			ret.representedHandles = casted.representedHandles;
			
		} else 
		if (info instanceof FolderInfo) {
			ret.setUserObject(info);
			
		} else {
			ret.setUserObject(info);
			/* when a component is configured as "autostart", it will have
			 * the manager as its first client.
			 * matej email 2009-04: there is no way to retrieve the handle
			 * of the manager... but it is always fixed. */
			if ("Manager".equals(info)) // = 83886080
				ret.representedHandles = new int[]{HandleConstants.MANAGER_MASK};
		}
		return ret;
	}

	protected void addInfoNodes (SortingTreeNode node, Map<Object,String> auxiliary) {
		Object info = node.getUserObject();
		int infokey = System.identityHashCode(info);

		if (info instanceof ContainerInfo) {
			//ContainerInfo casted = (ContainerInfo)info;
			node.add(createNode(new InfoDetail("location", auxiliary.get(infokey+".location") )));

	   } else
		if (info instanceof ClientInfo) {
			ClientInfo casted = (ClientInfo)info;
			node.add(createNode(new InfoDetail("location", auxiliary.get(infokey+".location") )));
			node.add(createNode(new InfoDetail("components", casted.components, true)));
			node.add(createNode(new InfoDetail("access", casted.access, false)));
			node.add(createNode(new InfoDetail("reference", casted.reference)));

		} else
		if (info instanceof ComponentInfo) {
			ComponentInfo casted = (ComponentInfo)info;
			node.add(createNode(new InfoDetail("clients", casted.clients, true)));
			node.add(createNode(new InfoDetail("container", casted.container, true)));
			node.add(createNode(new InfoDetail("container_name", casted.container_name)));
			node.add(createNode(new InfoDetail("access", casted.access, false)));
			node.add(createNode(new InfoDetail("reference", casted.reference)));
			node.add(createNode(new InfoDetail("interfaces", casted.interfaces)));
			node.add(createNode(new InfoDetail("type", casted.type)));
			node.add(createNode(new InfoDetail("code", casted.code)));
		}
	}


   // wraps a single piece of info from ComponentInfo,
   // each piece is to be shown in its own node.
   static public class InfoDetail {
      public String key;
      public String value;
      
		/** 
		 * Many tree nodes represent 
		 * a) something that has a handle or 
		 * b) something that references other things that have a handle
		 */
		public int[] representedHandles = new int[]{};
      
      
      protected InfoDetail(String key, String[] value) {
         if (value.length == 0)
               this.value = "none";
         else {
            StringBuffer list = new StringBuffer();
            for (int i=0; i<value.length ; i++)
               list.append(String.valueOf(value[i])).append(",");
            list.setLength(list.length() - ",".length());
            this.value = list.toString();
         }
         this.key = key;
      }
      protected InfoDetail(String key, int[] value, boolean intRepresentsHandle) {
         StringBuffer list = new StringBuffer();
         if (value.length == 0)
               list.append("none");
         else {
            for (int i=0; i<value.length ; i++)
               list.append(String.valueOf(value[i])).append(",");
            list.setLength(list.length() - ",".length());
         }
         this.key = key;
         this.value = list.toString();
         if (intRepresentsHandle)
         	this.representedHandles = value;
      }
      protected InfoDetail(String key, Object value) {
         this.key = key;
         this.value = String.valueOf(value);
      }
      protected InfoDetail(String key, int value, boolean intRepresentsHandle) {
         this.key = key;
         this.value = String.valueOf(value);
         if (intRepresentsHandle && value != 0)
         	this.representedHandles = new int[]{value};
      }

      @Override
		public String toString() {
         return  "Detail \""+key+"="+value+"\"";
       }
   }
   
  /**
   * Used as the userobject for the nodes "containers", "client applications", "components"
   */
   static public class FolderInfo {

      public String name;

      public String filter;
      public boolean hasFilter() {
      	return filter!=null && !filter.isEmpty();
      }

      protected FolderInfo(String name) {
       this.name = name;  
      }
      
      @Override
		public String toString() {
        return  "Folder \""+name+"\"";
      }
   }
   
   static public class SortingTreeNode extends DefaultMutableTreeNode {
      
      protected SortingTreeNode() {
         super();
      }
		public SortingTreeNode(Object userObject) {
			super(userObject);
		}
		
		public int[] representedHandles = new int[]{};
		public boolean represents (int h) {
			for (int i=0; i<representedHandles.length; i++)
				if (representedHandles[i] == h) return true;
			return false;
		}

		public boolean filtered;

		/**
		 * Support for guis (deployment tree). This is not a deep-clone. The userobject and
		 * representedHandles will be the same as in the source node but children and
		 * parent references will not be cloned.
		 */
		@Override
		public SortingTreeNode clone () {
			SortingTreeNode ret = (SortingTreeNode) super.clone();
			ret.representedHandles = (this.representedHandles);
			return ret;
		}
		
		
		/** Syntactic sugar, for easy iteration over this node's children. */
		public Iterable<SortingTreeNode> childrens() {
			final Enumeration<?> orig = children();
			return new Iterable<SortingTreeNode>() {
				@Override public Iterator<SortingTreeNode> iterator () {
					return new java.util.Iterator<SortingTreeNode>() {
						@Override public boolean hasNext () {return orig.hasMoreElements();}
						@Override public SortingTreeNode next () {return (SortingTreeNode)orig.nextElement();}
						@Override public void remove () {throw new UnsupportedOperationException();}
					};
				}
			};
		}

      public void sortChildrenByName () {
      	if (children != null) {
      		synchronized (children) {
      			Collections.sort (children, byName);
      		}
      	}
      }

      // a comparator for names, needed for sorting
      static Comparator<Object> byName = new Comparator<Object>(){
         public int compare (Object a, Object b) {
         	String valA = ((SortingTreeNode) a).infoName();
         	String valB = ((SortingTreeNode) b).infoName();
            return valA.compareTo(valB);
         }
      };

      /**
       * Read the 'name' value from the contained info struct.
       */
      protected String infoName () {
         if (userObject instanceof ContainerInfo)
         	return ((ContainerInfo)userObject).name;  
         if (userObject instanceof ClientInfo)
            return ((ClientInfo)userObject).name;  
         if (userObject instanceof ComponentInfo)
            return ((ComponentInfo)userObject).name;  
         return "";
      }

      /** */
      public boolean equals (SortingTreeNode other) {
      	Object otherObject = other.getUserObject();

      	if (userObject == null || otherObject == null)
      		return false;
      	if (userObject.getClass() != otherObject.getClass())
      		return false;
      	if (userObject instanceof ContainerInfo)
      		return ((ContainerInfo)userObject).name.equals( ((ContainerInfo)otherObject).name);
      	if (userObject instanceof ClientInfo)
      		return ((ClientInfo)userObject).h == ((ClientInfo)otherObject).h;
      	if (userObject instanceof ComponentInfo)
      		return ((ComponentInfo)userObject).name.equals( ((ComponentInfo)otherObject).name);
      	return false;
      }

   }

}

