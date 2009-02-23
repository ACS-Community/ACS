package alma.acs.eventbrowser.views;

public class ParsedAnyData {
	private String name;
	private String type;
	private String value;
	
	public ParsedAnyData(String name, String type, String value) {
		super();
		this.name = name;
		this.type = type;
		this.value = value;
	}
	
	public ParsedAnyData() {
		// TODO Auto-generated constructor stub
	}

	public String getName() {
		return name;
	}
	public String getType() {
		return type;
	}
	public String getValue() {
		return value;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setType(String type) {
		this.type = type;
	}

	public void setValue(String value) {
		this.value = value;
	}
}
