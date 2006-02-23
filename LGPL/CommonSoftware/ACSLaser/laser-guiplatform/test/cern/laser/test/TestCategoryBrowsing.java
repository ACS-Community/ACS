package cern.laser.test;


import java.util.Collection;

import cern.laser.client.data.Category;
import cern.laser.client.services.browsing.CategoryBrowsingHandler;

public class TestCategoryBrowsing 
{
    
  private static final String TEST_STRING = "test";  
    
  public TestCategoryBrowsing()
  {
      
      
    try {
        
        System.out.println("test START");
      CategoryBrowsingHandler handler = CategoryBrowsingHandler.get();    
      Category root =  handler.getCategoryTreeRoot();
      System.out.println("root :\n" + root);
      Collection children = handler.getChildren(root);
      System.out.println("children :\n" + children);
      
      System.out.println("test after CategoryBrowsingHandler ");
      
      
      //System.exit(1);
      System.out.println("test aftes exit");
      
    } catch (Exception e) 
    {
      System.err.println("stack trace : ");
      e.printStackTrace();
      //System.err.println("root cause : ");
      //e.getRootCause().printStackTrace();
    } 
    //catch (Error err) {
    //    err.printStackTrace();
    //}
       
      /*
      JFrame frame = new JFrame();
      JPanel panel = new JPanel();
      frame.getContentPane().add(panel);
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      frame.setVisible(true);
      */
      
      //System.out.println(TEST_STRING);
      
      System.out.println("test end");
  }

  public static void main(String[] args)
  {
      
    TestCategoryBrowsing testCategoryBrowsing = new TestCategoryBrowsing();
      
    System.out.println("test end final");
  }
}


