package cern.gp.explorer.test.helpers;

import cern.gp.nodes.NodeFactory;
import cern.gp.nodes.children.ChildrenListManager;
import cern.gp.nodes.children.NodeList;
import java.beans.IntrospectionException;
import java.lang.reflect.Constructor;
import java.util.Comparator;


/**
 * an implementation of the ChildrenListManager to be used for testing and quick hacks.
 * It recursively adds children of the same type to the parent, up to the number of
 * children specified.
 * This class is typically used in the following way:
 * <pre>
 *     GPNode root = NodeFactory.createNode(new SimpleDemoBean("parent"), new RecursiveChildrenListManager(10, 3));
 *     expl = new TreeExplorer();
 *     expl.setRootNode(root);
 * </pre>
 * This creates a tree, in which you can expand 10 parent nodes 
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class RecursiveChildrenListManager implements ChildrenListManager {
  private int count = 0;
  private final int children;
  private final int childrenPerParent;
  private final Class beanClass;
  private final Constructor beanConstructor;
  private final static int DEFAULT_CHILDREN = 10;
  private final static int DEFAULT_CHILDREN_PER_PARENT = 3;
  private final static Class DEFAULT_BEAN_CLASS = SimpleDemoBean.class;
  
  /**
   * Constructor to create a tree with a pre-defined number of children. This constructor 
   * uses {@link SimpleDemoBean} as Beans to display in the tree. If you need to change
   * the default settings use a different constructor.
   */
  public RecursiveChildrenListManager() {
    this(DEFAULT_CHILDREN, DEFAULT_CHILDREN_PER_PARENT);
  }
  
  /**
   * constructor that allows to specify how many times children are added to the
   * parents, when the user opens the branches of the tree. After this number has been
   * reached, no further children will be added to the parent, trying to open further
   * branch of the tree will simply result in leaf nodes.
   *
   * @param children how many times children shall be added
   */
  public RecursiveChildrenListManager(int children) {
    this(DEFAULT_BEAN_CLASS, children, DEFAULT_CHILDREN_PER_PARENT);
  }
  /**
   * Constructor that allows to specify (1) how many times children are added to parents
   * when the user opens the branches of the tree and (2) how many children are added to each
   * parent.
   * @param children how many times children shall be added
   * @param childrenPerParent how many children shall be added to each parent
   */
  public RecursiveChildrenListManager(int children, int childrenPerParent) {
    this(DEFAULT_BEAN_CLASS, children, childrenPerParent);
  }
  
  /**
   * constructor that allows to specify how many times children are added to the
   * parents, and how many sibling children are added each time, and the Bean class to use
   *
   * @param beanClass the class to use as bean. This class should either have a constructor that
   * accepts a String (preferred) or at least a no-argument constructor. 
   * @param children the number of times children shall be added
   * @param the number of sibling children added to each parent
   */
  public RecursiveChildrenListManager(Class beanClass, int children, int childrenPerParent) {
    this.beanClass = beanClass;
    this.children = children;
    this.childrenPerParent = childrenPerParent;
    this.beanConstructor = getConstructor(beanClass);
  }
  
  
  //
  //--- IMPLEMENTS ChildrenListManager
  //
  public Comparator getComparator() {
    return null;
  }
  
  public void initChildrenList(NodeList nodeList) {
    count++;
    try {
      if (count < children) {
        for (int ix=0; ix< childrenPerParent; ix++) {
          nodeList.addNode(NodeFactory.createNode(createBean("child " + count + ix), this));
        }
      } else {
        for (int ix=0; ix< childrenPerParent; ix++) {
          nodeList.addNode(NodeFactory.createNode(createBean("child " + count + ix)));
        }
      }
    } catch (IntrospectionException ex) { ex.printStackTrace(); }
  }
  
  //
  //------------- Private Methods ----------------------------------------------
  //
  
  /**
   * helper method, tries to find a suitable constructor from the class specified
   * Currently, this must either be a constructor that accepts one string or a no-argument
   * constructor.
   */
  private static Constructor getConstructor(Class clazz) {
    try {
      return clazz.getConstructor(new Class[] { String.class});
    } catch (NoSuchMethodException ex) {
      System.err.println("warning: bad paramenter " + clazz +
      "; should have a Constructor that accepts one string as argument");
    }
    try {
      return clazz.getConstructor(null);
    } catch (NoSuchMethodException ex) {
      System.err.println("warning: bad paramenter " + clazz +
      "; must at least have a non-argument Constructor!");
    }
    try {
      return SimpleDemoBean.class.getConstructor(new Class[] { String.class} );
    } catch (NoSuchMethodException ex) {
      System.err.println("internal error: SimpleDemoBean.class does not have a String-constructor");
    }
    return null;
  }

  /**
   * helper method, uses the variable beanConstructor to create a new Bean
   */
  private Object createBean(String args) {
    try {
      return beanConstructor.newInstance(new Object[] { args });
    } catch (Exception ex) {
      System.err.println("error instantiating beanClass " + beanClass + " with String constructor");
    }
    try {
      return beanClass.newInstance();
    } catch (Exception ex) {
      System.err.println("error instantiating beanClass " + beanClass + " with default constructor");
    }
    return null;
  }
}
