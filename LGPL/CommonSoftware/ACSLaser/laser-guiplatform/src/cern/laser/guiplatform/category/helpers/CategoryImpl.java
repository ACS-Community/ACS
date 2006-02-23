package cern.laser.guiplatform.category.helpers;
import cern.laser.client.data.Category;

public class CategoryImpl implements Category, Cloneable
{
  private final Integer categoryId;
  private final String name;
  private final String description;
  private final String path;
  private final boolean leaf;

  //public CategoryImpl(cern.laser.business.data.Category category)
  public CategoryImpl(String _name, String _description) {
      this(_name, _description, -1000, "category path", true);
  }
  
  public CategoryImpl(String _name, String _description, int _categoryId, 
    String _categoryPath, boolean _leaf)
  {
    categoryId = new Integer(_categoryId);
    name = _name;
    description = _description;
    path = _categoryPath;
    leaf = _leaf;
    /*
    categoryId = -1000;
    name = null;
    description = null;
    path = null;
    leaf = false;
    */
    /*
    if (category == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    categoryId = category.getCategoryId().intValue();
    name = category.getName();
    description = category.getDescription();
    path = category.getPath();
    leaf = category.isLeaf();
    */
  }

  private CategoryImpl(Category category)
  {
    if (category == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    categoryId = category.getCategoryId();
    name = new String(category.getName());
    description = new String(category.getDescription());
    path = new String(category.getPath());
    leaf = category.isLeaf();
  }

  public Integer getCategoryId()
  {
    return categoryId;
  }

  public String getName()
  {
    return name;
  }

  public String getDescription()
  {
    return description;
  }

  public String getPath() 
  {
    return path;
  }

  public boolean isLeaf() 
  {
    return leaf;
  }

  public String toString() 
  {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nID : ");
    str_buf.append(getCategoryId());
    str_buf.append("\nNAME : ");
    str_buf.append(getName());
    str_buf.append("\nDESCRIPTION : ");
    str_buf.append(getDescription());
    str_buf.append("\nPATH : ");
    str_buf.append(getPath());
    str_buf.append("\nLEAF : ");
    str_buf.append(isLeaf());
    
    return str_buf.toString();
  }

  public Object clone() throws CloneNotSupportedException 
  {
    return new CategoryImpl(this);
  }  
  
}
