/*
 * TstCategoryBrowsing.java
 *
 * Created on November 18, 2004, 5:32 PM
 */

package cern.laser.test;

/**
 *
 * @author  woloszyn
 */
import java.util.Collection;

import cern.laser.client.data.Category;
import cern.laser.client.services.browsing.CategoryBrowsingHandler;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
 */
public class TstCategoryBrowsing {
  /**
   * Creates a new TestCategoryBrowsing object.
   */
  public TstCategoryBrowsing() {
    try {
      CategoryBrowsingHandler handler = CategoryBrowsingHandler.get();
      Category root = handler.getCategoryTreeRoot();
      System.out.println("root :\n" + root);
      Collection children = handler.getChildren(root);
      System.out.println("children :\n" + children);
      Collection categories = handler.getCategories();
      System.out.println("categories :\n" + categories);
    } catch (Exception e) {
      System.err.println("stack trace : ");
      e.printStackTrace();
      System.err.println("root cause : ");
      e.getCause().printStackTrace();
    }
  }

  /**
   * DOCUMENT ME!
   *
   * @param args DOCUMENT ME!
   */
  public static void main(String[] args) {
    TstCategoryBrowsing testCategoryBrowsing = new TstCategoryBrowsing();
  }
}
