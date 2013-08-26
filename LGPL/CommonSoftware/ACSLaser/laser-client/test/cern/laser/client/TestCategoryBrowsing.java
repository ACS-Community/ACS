/*
 * $Id: TestCategoryBrowsing.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client;

import java.util.Collection;

import cern.laser.client.data.Category;
import cern.laser.client.services.browsing.CategoryBrowsingHandler;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
 */
public class TestCategoryBrowsing {
  /**
   * Creates a new TestCategoryBrowsing object.
   */
  public TestCategoryBrowsing() {
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
    TestCategoryBrowsing testCategoryBrowsing = new TestCategoryBrowsing();
  }
}
