/**
 * 
 */
package alma.hibernate.test;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author msekoranja
 *
 */
/**
 * @author msekoranja
 *
 */
@Entity
@Table(name="sub_entity_table")
public class SubEntity {

	private Integer subId;
	private Integer parentId; 
	private String name;
	
	@Id
	@GeneratedValue
	public Integer getSubId() {
		return subId;
	}

	public void setSubId(Integer subId) {
		this.subId = subId;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the parentId
	 */
	public Integer getParentId() {
		return parentId;
	}

	/**
	 * @param parentId the parentId to set
	 */
	public void setParentId(Integer parentId) {
		this.parentId = parentId;
	}

}
