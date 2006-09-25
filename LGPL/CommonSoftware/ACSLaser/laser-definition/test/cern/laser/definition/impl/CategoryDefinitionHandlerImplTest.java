/*
 * $Id: CategoryDefinitionHandlerImplTest.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.definition.impl;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;

import junit.framework.TestCase;
import cern.laser.client.LaserException;
import cern.laser.client.data.Category;
import cern.laser.client.services.browsing.CategoryBrowsingHandler;
import cern.laser.definition.LaserDefinitionException;

/**
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public class CategoryDefinitionHandlerImplTest extends TestCase {

  private static final String ADMIN_USER = "laser";
  private CategoryDefinitionHandlerImpl handler;
  private CategoryBrowsingHandler categoryBrowsing;

  public static void main(String[] args) {
    junit.textui.TestRunner.run(CategoryDefinitionHandlerImplTest.class);
  }

  //
  //-- CONSTRUCTORS ---------------------------------------------------
  //

  /**
   * Constructor for CategoryDefinitionHandlerImplTest.
   * 
   * @param arg0
   */
  public CategoryDefinitionHandlerImplTest(String arg0) {
    super(arg0);
  }

  //
  // -- SETUP - TEARDOWN ----------------------------------------------
  //

  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
    handler = new CategoryDefinitionHandlerImpl(ADMIN_USER);
    categoryBrowsing = CategoryBrowsingHandler.get();
  }

  /*
   * @see TestCase#tearDown()
   */
  protected void tearDown() throws Exception {
    super.tearDown();
  }

  //
  // -- TEST METHODS --------------------------------------------------
  //

  /*
   * Class to test for void upload(Reader)
   */
  public void testUploadReader() {
    InputStream is = getClass().getResourceAsStream("create-category-definitions.xml");
    InputStreamReader reader = new InputStreamReader(new BufferedInputStream(is));
    try {
      handler.upload(reader);
    } catch (LaserDefinitionException e) {
      fail(e.getMessage());
    }

    try {
      assertNotNull(categoryBrowsing.getCategoryByPath("CERN.DEFUNITTESTS"));
    } catch (LaserException e1) {
      fail(e1.getMessage());
    }
    try {
      assertNotNull(categoryBrowsing.getCategoryByPath("CERN.DEFUNITTESTS.DEFUNITTEST1"));
    } catch (LaserException e2) {
      fail(e2.getMessage());
    }
    try {
      assertNotNull(categoryBrowsing.getCategoryByPath("CERN.DEFUNITTESTS.DEFUNITTEST2"));
    } catch (LaserException e3) {
      fail(e3.getMessage());
    }
    try {
      assertNotNull(categoryBrowsing.getCategoryByPath("CERN.DEFUNITTESTS.DEFUNITTEST3"));
    } catch (LaserException e4) {
      fail(e4.getMessage());
    }

    is = getClass().getResourceAsStream("remove-category-definitions.xml");
    reader = new InputStreamReader(new BufferedInputStream(is));
    try {
      handler.upload(reader);
    } catch (LaserDefinitionException e5) {
      fail(e5.getMessage());
    }

    try {
      assertNull(categoryBrowsing.getCategoryByPath("CERN.DEFUNITTESTS"));
    } catch (LaserException e6) {}
    try {
      assertNull(categoryBrowsing.getCategoryByPath("CERN.DEFUNITTESTS.DEFUNITTEST1"));
    } catch (LaserException e7) {}
    try {
      assertNull(categoryBrowsing.getCategoryByPath("CERN.DEFUNITTESTS.DEFUNITTEST2"));
    } catch (LaserException e8) {}
    try {
      assertNull(categoryBrowsing.getCategoryByPath("CERN.DEFUNITTESTS.DEFUNITTEST3"));
    } catch (LaserException e9) {}
  }

  //
  // -- PRIVATE METHODS -----------------------------------------------
  //

  /**
   * @param categories
   * @param string
   * @return
   */
  private boolean listContainsCategory(Category[] categories, String categoryName) {
    for (int i = 0; i < categories.length; i++) {
      if (categories[i].getName().equals(categoryName)) 
      ;
      return true;
    }

    return false;
  }
}
