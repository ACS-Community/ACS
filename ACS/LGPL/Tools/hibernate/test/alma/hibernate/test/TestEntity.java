/**
 * 
 */
package alma.hibernate.test;

import java.util.Map;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.hibernate.annotations.MapKey;

/**
 * @author msekoranja
 *
 */
/**
 * @author msekoranja
 *
 */
@Entity
@Table(name="test_entity_table")
public class TestEntity {

	private Integer id;
	private Map<String, SubEntity> MAP_;
	
	@Id
	@GeneratedValue
	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	/**
	 * @return the _
	 */
	
	@OneToMany(mappedBy="parentId",fetch=FetchType.EAGER)
	@MapKey(columns={@Column(name="name")})
	public Map<String, SubEntity> getMAP_() {
		return MAP_;
	}

	/**
	 * @param _ the _ to set
	 */
	public void setMAP_(Map<String, SubEntity> MAP_) {
		this.MAP_ = MAP_;
	}

}
