package alma.acs.eventbrowser.model;

public class ChannelData implements Comparable<ChannelData> {
	private final String name;
	private int numberConsumers;
	private int numberSuppliers;
	private int deltaConsumers;	// change since last update
	private int deltaSuppliers; 
	
	public String getName() {
		return name;
	}

	public int getNumberConsumers() {
		return numberConsumers;
	}
	public void setNumberConsumers(int numberConsumers) {
		this.numberConsumers = numberConsumers;
	}
	public void setDeltaConsumers(int deltaConsumers) {
		this.deltaConsumers = deltaConsumers;
	}
	public int getDeltaConsumers() {
		return deltaConsumers;
	}
	public int getNumberSuppliers() {
		return numberSuppliers;
	}
	public void setNumberSuppliers(int numberSuppliers) {
		this.numberSuppliers = numberSuppliers;
	}
	public void setDeltaSuppliers(int deltaSuppliers) {
		this.deltaSuppliers = deltaSuppliers;
	}
	public int getDeltaSuppliers() {
		return deltaSuppliers;
	}
	
	public String getNumConsumersAndDelta() {
		return ""+numberConsumers+(deltaConsumers !=0 ? " ("+(deltaConsumers > 0 ? "+" : "")+deltaConsumers+")":"")+" consumers.";
	}
	
	public String getNumSuppliersAndDelta() {
		return ""+numberSuppliers+(deltaSuppliers !=0 ? " ("+(deltaSuppliers > 0 ? "+" : "")+deltaSuppliers+")":"")+" suppliers.";
	}
	public ChannelData(String name, int[] adminCounts, int[] adminDeltas) {
		super();
		this.name = name;
		this.numberConsumers = adminCounts[0];
		this.numberSuppliers = adminCounts[1];
		this.deltaConsumers = adminDeltas[0];
		this.deltaSuppliers = adminDeltas[1];
	}

	@Override
	public int compareTo(ChannelData o) {
		return getName().compareTo(o.getName());
	}
	
	@Override
	public boolean equals(Object o) {
		if (o == null || !(o instanceof ChannelData)) {
			return false;
		}
		return getName().equals(((ChannelData)o).getName());
	}
	
	@Override
	public int hashCode() {
		return getName().hashCode();
	}
}

