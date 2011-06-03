package alma.acs.monitoring.DAO;


public class MonitorCharacteristicIDs{
    private Long configurationId=-1L;
    private Long hwConfigurationId=-1L;
    private Long assemblyId=-1L;
    private String serialNumber="";
    private Long componentId=-1L;
    private Long baciPropertyId=-1L;
    private Long monitorPointId=-1L;
    private int index=-1;
    private boolean isOnDB=false;
    private String monitorPointName = "generic";

    public MonitorCharacteristicIDs(){}

    public Long getConfigurationId(){
        return this.configurationId;
    }

    public void setConfigurationId(Long configurationId){
        this.configurationId=configurationId;
    }

    public Long getHwConfigurationId(){
        return this.hwConfigurationId;
    }

    public void setHwConfigurationId(Long hwConfigurationId){
        this.hwConfigurationId=hwConfigurationId;
    }

    public Long getAssemblyId(){
        return this.assemblyId;
    }

    public void setAssemblyId(Long assemblyId){
        this.assemblyId=assemblyId;
    }

    public String getSerialNumber(){
    	return this.serialNumber;
    }

    public void setSerialNumber(String serialNumber){
    	this.serialNumber = serialNumber;
    }

     public Long getComponentId(){
        return this.componentId;
    }

    public void setComponentId(Long componentId){
        this.componentId=componentId;
    }
     public Long getBACIPropertyId(){
        return this.baciPropertyId;
    }

    public void setBACIPropertyId(Long baciPropertyId){
        this.baciPropertyId=baciPropertyId;
    }

     public Long getMonitorPointId(){
        return this.monitorPointId;
    }

    public void setMonitorPointId(Long monitorPointId){
        this.monitorPointId=monitorPointId ;
    }

     public int getIndex(){
        return this.index;
    }

    public void setIndex(int index){
        this.index=index ;
    }

    public boolean isOnDB(){
        return this.isOnDB;
    }

    public void setIsOnDB(boolean isOnDB){
        this.isOnDB=isOnDB;
    }

    public String getMonitorPointName(){
        return this.monitorPointName;
    }

    public void setMonitorPointName(String monitorPointName){
        this.monitorPointName = monitorPointName;
    }
}
