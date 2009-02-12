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
	private Map<String, SubEntity> _;
	
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
	public Map<String, SubEntity> get_() {
		return _;
	}

	/**
	 * @param _ the _ to set
	 */
	public void set_(Map<String, SubEntity> _) {
		this._ = _;
	}

}
