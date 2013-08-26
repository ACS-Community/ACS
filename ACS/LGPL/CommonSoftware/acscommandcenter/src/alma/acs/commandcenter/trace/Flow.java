package alma.acs.commandcenter.trace;

import java.util.*;

import javax.swing.tree.DefaultMutableTreeNode;

/**
 * The base class for flow tracing, subclass this to define a flow.
 */
public class Flow {

   // ======= Internal Data Structure ======

   protected final Node UNDEF = new Node("");

   protected Map<String,Node> name2node = new HashMap<String,Node>();

   protected Node node(String name) {
      Node node = (Node) name2node.get(name);
      if (node == null) {
         node = new Node(name);
         name2node.put(name, node);
      }
      return node;
   }

   protected class Node extends DefaultMutableTreeNode {
      protected Node(String name) {
         super(name);
      }

      protected String getName() {
         return String.valueOf(getUserObject());
      }

   }

   // ========== for use by subclasses defining a flow =========

   public void consistsOf(String parent, String[] kids) {
      for (int i = 0; i < kids.length; i++)
         node(parent).add(node(kids[i]));
   }

   // ========== for use by workers implementing this flow =========

   protected Node latestSuccess = UNDEF;
   protected Node nowTrying = UNDEF;

   public String current() {

      // if the current step was announced, it is easy
      Node step = nowTrying;

      // if not, we try to determine it ourselves
      if (step == UNDEF) {
         try {

            // determine step after latest success
            Node current = latestSuccess;
            if (current == null)
               current = node(null);
            step = (Node) current.getNextLeaf(); //PENDING(msc): this assumption won't hold

            // otherwise assume first step is the current one
            if (step == null)
               step = ((Node) node(null).getChildAt(0));

         } catch (Exception exc) {
            System.err.println("Flow.failure: "+exc);
            step = UNDEF;
         }
      }
      return step.getName();
   }
   
   public void reset (Object info) {
      this.latestSuccess = UNDEF;
      this.nowTrying = UNDEF;
      fireReset(info);
   }

   public void trying (String step) {
      this.nowTrying = node(step);
      fireTrying(step);
   }

   public void success (String step) {
      this.latestSuccess = node(step);
      this.nowTrying = UNDEF;
      fireSuccess(step);

      if (latestSuccess.getNextLeaf() == null)
         fireCompletion();
   }

   public void failure (Object info) {
   	String failed = current();
      fireFailure(failed, info);
   }

   // --- helpers for the foregoing section ---

   protected void fireReset (Object info) {
      FlowListener[] l = new FlowListener[listeners.size()];
      listeners.copyInto(l);
      for (int i = 0; i < l.length; i++)
         l[i].reset(this, info);
   }

   protected void fireTrying(String step) {
      FlowListener[] l = new FlowListener[listeners.size()];
      listeners.copyInto(l);
      for (int i = 0; i < l.length; i++)
         l[i].trying(this, step);
   }

   protected void fireSuccess(String step) {
      FlowListener[] l = new FlowListener[listeners.size()];
      listeners.copyInto(l);
      for (int i = 0; i < l.length; i++)
         l[i].success(this, step);
   }

   protected void fireFailure(String step, Object info) {
      FlowListener[] l = new FlowListener[listeners.size()];
      listeners.copyInto(l);
      for (int i = 0; i < l.length; i++)
         l[i].failure(this, step, info);
   }

   protected void fireCompletion() {
      FlowListener[] l = new FlowListener[listeners.size()];
      listeners.copyInto(l);
      for (int i = 0; i < l.length; i++)
         l[i].completion(this);
   }

   // ========== for use by listeners observing this flow =========

   protected Vector<FlowListener> listeners = new Vector<FlowListener>();

   public void addListener(FlowListener l) {
      listeners.add(l);
   }

   public void removeListener(FlowListener l) {
      listeners.remove(l);
   }

}
