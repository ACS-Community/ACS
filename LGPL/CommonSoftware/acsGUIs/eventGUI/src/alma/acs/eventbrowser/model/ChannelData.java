package alma.acs.eventbrowser.model;

public class ChannelData {
	private String name;
	private int numberConsumers;
	private int numberSuppliers;
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public int getNumberConsumers() {
		return numberConsumers;
	}
	public void setNumberConsumers(int numberConsumers) {
		this.numberConsumers = numberConsumers;
	}
	public int getNumberSuppliers() {
		return numberSuppliers;
	}
	public void setNumberSuppliers(int numberSuppliers) {
		this.numberSuppliers = numberSuppliers;
	}
	public ChannelData(String name, int numberConsumers, int numberSuppliers) {
		super();
		this.name = name;
		this.numberConsumers = numberConsumers;
		this.numberSuppliers = numberSuppliers;
	}

}
