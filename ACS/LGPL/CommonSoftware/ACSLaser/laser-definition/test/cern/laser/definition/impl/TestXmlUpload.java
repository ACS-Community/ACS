package cern.laser.definition.impl;

import java.io.BufferedInputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Arrays;
import java.util.Vector;

import cern.laser.definition.AdminUser;
import cern.laser.definition.AdminUserHandler;
import cern.laser.definition.LaserDefinitionException;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
 */
public class TestXmlUpload {
  private static final String ALARM_DEFINITIONS = "ALARMS";
  private static final String SOURCE_DEFINITIONS = "SOURCES";
  private static final String CATEGORY_DEFINITIONS = "CATEGORIES";
  private String uploadURL;

  /**
   * Creates a new TestXmlUpload object.
   *
   * @param args DOCUMENT ME!
   */
  public TestXmlUpload(String[] args) {
    try {
      Vector params = new Vector(Arrays.asList(args));
      String username = (String) params.get(params.indexOf("-u") + 1);
      String password = (String) params.get(params.indexOf("-p") + 1);
      String reason = (String) params.get(params.indexOf("-r") + 1);
      String uploadURL = (String) params.get(params.indexOf("-f") + 1);

      AdminUserHandler handler = AdminUserHandler.get();

      System.out.println("logging...");

      AdminUser admin = handler.loginUser(username, password);
      System.out.println("user " + username + " logged in");

      System.out.println("uploading from : " + uploadURL);

      URL url = new URL(uploadURL);
      InputStreamReader reader = new InputStreamReader(new BufferedInputStream(url.openStream()));

      if (reason.toUpperCase().equals(ALARM_DEFINITIONS)) {
        admin.getAlarmDefinitionHandler().upload(reader);
      } else if (reason.toUpperCase().equals(SOURCE_DEFINITIONS)) {
        admin.getSourceDefinitionHandler().upload(reader);
      } else if (reason.toUpperCase().equals(CATEGORY_DEFINITIONS)) {
        admin.getCategoryDefinitionHandler().upload(reader);
      } else {
        System.err.println("unknown reason : " + reason);
      }
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
    TestXmlUpload testXmlUpload = new TestXmlUpload(args);
  }
}
