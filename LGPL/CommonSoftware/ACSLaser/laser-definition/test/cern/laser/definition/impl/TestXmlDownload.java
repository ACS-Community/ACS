package cern.laser.definition.impl;

import java.io.BufferedWriter;
import java.io.FileWriter;

import cern.laser.definition.AdminUser;
import cern.laser.definition.AdminUserHandler;
import cern.laser.definition.LaserDefinitionException;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
 */
public class TestXmlDownload {
  /**
   * Creates a new TestXmlDownload object.
   *
   * @param downloadFilename DOCUMENT ME!
   */
  public TestXmlDownload(String downloadFilename) {
    try {
      AdminUserHandler handler = AdminUserHandler.get();

      System.out.println("logging...");

      AdminUser admin = handler.loginUser("francesco", "password");

      System.out.println("downloading sources to : " + downloadFilename);

      BufferedWriter writer = new BufferedWriter(new FileWriter(downloadFilename));
      admin.getSourceDefinitionHandler().download(writer);
      writer.flush();
      writer.close();
      System.out.println("sources downloaded");
    } catch (LaserDefinitionException lde) {
      lde.printStackTrace();
      lde.getCause().printStackTrace();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * DOCUMENT ME!
   *
   * @param args DOCUMENT ME!
   */
  public static void main(String[] args) {
    TestXmlDownload testXmlDownload = new TestXmlDownload(args[0]);
  }
}
