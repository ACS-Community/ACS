/*
 * Created on Oct 10, 2006 by mschilli
 */
package alma.acs.commandcenter.meta;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.List;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;

import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;



public class MaciInfo extends DefaultTreeModel {

	
	// ====================================================================
	// Public API
	// This is for the benefit of a client like Exec that in some way or
	// another analyzes the contents of the MaciInfo. To not force them
	// to work closely on our tree node structure, this public API provides
	// some logical view onto us.
	
	
	public List<ContainerInfo> getContainers () {
		synchronized (refreshContainerInfoLock) {
			
			int nKids = containerNode.getChildCount();
			List<ContainerInfo> ret = new ArrayList<ContainerInfo>(nKids);
			for (int i = 0; i < nKids; i++) {
				DefaultMutableTreeNode kid = (DefaultMutableTreeNode) containerNode.getChildAt(i);
				ContainerInfo info = (ContainerInfo) kid.getUserObject();

				ret.add(info);
			}
			return ret;
		}
	}

	public ContainerInfo getContainer (String name) {
		for (ContainerInfo info : getContainers()) {
			if (info.name.equals(name))
				return info;
		}
		return null;
	}
	
	public ContainerInfo getContainer (int handle) {
		for (ContainerInfo info : getContainers()) {
			if (info.h == handle)
				return info;
		}
		return null;
	}
	
	public List<ClientInfo> getClients() {
		synchronized (refreshClientInfoLock) {
			
			int nKids = clientNode.getChildCount();
			List<ClientInfo> ret = new ArrayList<ClientInfo>(nKids);
			for (int i = 0; i < nKids; i++) {
				DefaultMutableTreeNode kid = (DefaultMutableTreeNode) clientNode.getChildAt(i);
				ClientInfo info = (ClientInfo) kid.getUserObject();

				ret.add(info);
			}
			return ret;
		}
	}	

	public ClientInfo getClient (String name) {
		for (ClientInfo info : getClients()) {
			if (info.name.equals(name))
				return info;
		}
		return null;
	}
	
	public ClientInfo getClient (int handle) {
		for (ClientInfo info : getClients()) {
			if (info.h == handle)
				return info;
		}
		return null;
	}
	
	public List<ComponentInfo> getComponents () {
		synchronized (refreshComponentInfoLock) {
			
			int nKids = componentNode.getChildCount();
			List<ComponentInfo> ret = new ArrayList<ComponentInfo>(nKids);
			for (int i = 0; i < nKids; i++) {
				DefaultMutableTreeNode kid = (DefaultMutableTreeNode) componentNode.getChildAt(i);
				ComponentInfo info = (ComponentInfo) kid.getUserObject();

				ret.add(info);
			}
			return ret;
		}
	}

	public ComponentInfo getComponent (String name) {
		for (ComponentInfo info : getComponents()) {
			if (info.name.equals(name))
				return info;
		}
		return null;
	}

	public ComponentInfo getComponent (int handle) {
		for (ComponentInfo info : getComponents()) {
			if (info.h == handle)
				return info;
		}
		return null;
	}	
	
	public List<ComponentInfo> getStartedComponents () {
		synchronized (refreshComponentInfoLock) {
			
			int nKids = componentNode.getChildCount();
			List<ComponentInfo> ret = new ArrayList<ComponentInfo>(nKids);
			for (int i = 0; i < nKids; i++) {
				DefaultMutableTreeNode kid = (DefaultMutableTreeNode) componentNode.getChildAt(i);
				ComponentInfo info = (ComponentInfo) kid.getUserObject();

				if (info.h == 0) // skip non-activated components
					continue;
				
				ret.add(info);
			}
			return ret;
		}
	}	
	
	
	
	// ====================================================================
	// Not-so-public API
	// These are the portions used by the MaciSupervisor to construct the
	// MaciInfo, and by commandcenter's DeploymentTree to present it.
	
	
	protected MaciInfo (SortingTreeNode root, SortingTreeNode conts, SortingTreeNode clients, SortingTreeNode comps) {
		super(root);
		managerNode = root;
		managerNode.add(containerNode = conts);
		managerNode.add(clientNode = clients);
		managerNode.add(componentNode = comps);
	}

   /**
    */
   protected SortingTreeNode managerNode;
   /**
    */
   protected SortingTreeNode containerNode;
   /**
    */
   protected SortingTreeNode clientNode;
   /**
    */
   protected SortingTreeNode componentNode;
   
   /** 
    * For synchronization
    */
   final protected Object refreshContainerInfoLock = new Object();
   /**
    * For synchronization
    */
   final protected Object refreshComponentInfoLock = new Object();
   /**
    * For synchronization
    */
   final protected Object refreshClientInfoLock = new Object();
   	
	
   
   // wraps a single piece of info from ComponentInfo, each piece is to be shown in its own node.
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

      public String toString() {
         return  "Detail \""+key+"="+value+"\"";
       }
   }
   
  /**
   * Used as the userobject for the nodes "containers", "client applications", "components"
   */
   static public class FolderInfo {

      public String name;  
      
      protected FolderInfo(String name) {
       this.name = name;  
      }
      
      public String toString() {
        return  "Folder \""+name+"\"";
      }
   }
   
   static public class SortingTreeNode extends DefaultMutableTreeNode {
      
      protected SortingTreeNode() {
         super();
      }
		protected SortingTreeNode(Object userObject) {
			super(userObject);
		}
		
		public int[] representedHandles = new int[]{};
		
		
		/**
		 * Support for guis (deployment tree). This is not a deep-clone. The userobject and
		 * representedHandles will be the same as in the source node but children and
		 * parent references will not be cloned.
		 */
		public SortingTreeNode clone () {
			SortingTreeNode ret = (SortingTreeNode) super.clone();
			ret.representedHandles = (this.representedHandles);
			return ret;
		}
		
		
      public void sortChildrenByInfoDetail (final String key) {

      	if (children == null)
      		return; // nothing to do
      	
         synchronized(children) {
         	// make a working copy
            Object[] copy = new Object[children.size()];
            children.copyInto(copy);
            // create a comparator for detail-values
            Comparator<Object> c = new Comparator<Object>(){
               public int compare(Object a, Object b) {
                  String valA = ((SortingTreeNode)a).detailValue(key);
                  String valB = ((SortingTreeNode)b).detailValue(key);
                  return valA.compareTo(valB);
               }
            };
            // sort working copy, then replace original data
            Arrays.sort(copy, c);
            for (int i = 0; i < copy.length; i++) {
					children.set(i, copy[i]);
				}
         }
         
      }

      /**
       * Looks through child nodes with info detail for the InfoDetail with the specified name.
       * Used by the sort-method above.
       */
      protected String detailValue(String key) {

         if (key.equals("name")) {

            if (userObject instanceof ContainerInfo) {
             return ((ContainerInfo)userObject).name;  
            }
            else
            if (userObject instanceof ClientInfo) {
               return ((ClientInfo)userObject).name;  
            }
            else
            if (userObject instanceof ComponentInfo) {
               return ((ComponentInfo)userObject).name;  
            }
            else return "";

         } else {

            for (Enumeration en = children(); en.hasMoreElements();) {

            	try{
               SortingTreeNode elem = (SortingTreeNode) en.nextElement();
               InfoDetail info = (InfoDetail) elem.getUserObject();
               if (key.equals(info.key))
               	return info.value;
               }catch(Exception exc){/*child is not a InfoDetail node, just continue*/}
   			}
         	return ""; // no InfoDetail nodes at all, or none with that key
         }
      }
      
      /** support for merge operations */
      public boolean equals (SortingTreeNode other) {
      	
      	Object otherObject = other.getUserObject();
      	if (!otherObject.getClass().equals(userObject))
      		return false;

      	if (userObject instanceof ContainerInfo) {
      		return ((ContainerInfo)userObject).name.equals( ((ContainerInfo)otherObject).name);
      	
      	} else if (userObject instanceof ClientInfo) {
      		return ((ClientInfo)userObject).h == ((ClientInfo)otherObject).h;
      	
      	} else if (userObject instanceof ComponentInfo) {
      		return ((ComponentInfo)userObject).name.equals( ((ComponentInfo)otherObject).name);
      	
      	} else {
      		return false;
      	}
      }

   }
   
   
   
	
}

