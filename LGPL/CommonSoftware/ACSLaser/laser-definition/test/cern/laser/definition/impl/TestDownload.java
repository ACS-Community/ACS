package cern.laser.definition.impl;

import java.io.StringWriter;

import org.exolab.castor.xml.Marshaller;

import cern.laser.business.definition.data.AlarmDefinition;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
 */
public class TestDownload {
  /**
   * Creates a new TestDownload object.
   */
  public TestDownload() {
    try {
      AlarmDefinition definition = new AlarmDefinition("family", "member", new Integer(1));
      definition.setSystemName("system");
      definition.setIdentifier("ident");
      definition.setProblemDescription("pd");
      definition.setPriority(new Integer(1));
      definition.setInstant(Boolean.FALSE);
      System.out.println(definition.toString());

      //marshaller.marshal(marshalAlarmDefinition(definition));
      StringWriter writer = new StringWriter();
      Marshaller.marshal(marshalAlarmDefinition(definition), writer);
      writer.flush();
      System.out.println(writer.toString());
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
    TestDownload testDownload = new TestDownload();
  }

  private cern.laser.definition.impl.generated.AlarmDefinition marshalAlarmDefinition(AlarmDefinition definition) {
    cern.laser.definition.impl.generated.AlarmDefinition result = new cern.laser.definition.impl.generated.AlarmDefinition();
    result.setFaultFamily(definition.getFaultFamily());
    result.setFaultMember(definition.getFaultMember());
    result.setFaultCode(definition.getFaultCode().intValue());

    cern.laser.definition.impl.generated.VisualFields visual_fields = new cern.laser.definition.impl.generated.VisualFields();
    visual_fields.setSystemName(definition.getSystemName());
    visual_fields.setIdentifier(definition.getIdentifier());
    visual_fields.setProblemDescription(definition.getProblemDescription());
    result.setVisualFields(visual_fields);
    result.setCause(definition.getCause());
    result.setAction(definition.getAction());
    result.setConsequence(definition.getConsequence());
    result.setHelpUrl(definition.getHelpURL());
    result.setInstant((definition.getInstant() == null) ? false : definition.getInstant().booleanValue());
    result.setPiquetGSM(definition.getPiquetGSM());
    result.setPriority((definition.getPriority() == null) ? 0 : definition.getPriority().intValue());
    result.setResponsibleId((definition.getResponsiblePersonId() == null) ? 1 : definition.getResponsiblePersonId().intValue());
    result.setSourceName(definition.getSourceName());

    cern.laser.definition.impl.generated.Location location = new cern.laser.definition.impl.generated.Location();
    location.setBuilding(definition.getBuilding());
    location.setFloor(definition.getFloor());
    location.setMnemonic(definition.getMnemonic());
    location.setPosition(definition.getPosition());
    location.setRoom(definition.getRoom());
    result.setLocation(location);

    /*
    cern.laser.definition.impl.generated.Categories categories = new cern.laser.definition.impl.generated.Categories();
    Collection category_definitions = definition.getCategories();
    if (category_definitions != null && category_definitions.size() != 0)
    {
      Iterator iterator = category_definitions.iterator();
      while (iterator.hasNext())
      {
        String category_path = (String)iterator.next();
        cern.laser.definition.impl.generated.CategoryDefinition category = new cern.laser.definition.impl.generated.CategoryDefinition();
        category.setPath(category_path);
        categories.addCategoryDefinition(category);
      }
    }
    result.setCategories(categories);
    */
    return result;
  }
}
