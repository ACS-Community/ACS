/** MonitorDAO is the interface that defines the interaccion
 * between Blobbers and DAO Persistence Layer.
 *
 * @author Pablo Burgos
 * @since ACS-8_0_0-B Jun2009
 * @version "@(#) $Id: MonitorDAO.java,v 1.2 2011/06/03 22:29:54 vgonzale Exp $
 */
package alma.acs.monitoring.DAO;

import java.util.List;
import java.sql.Timestamp;
import alma.DAOErrType.wrappers.AcsJGettingMonitorCharacteristicsEx;
import alma.DAOErrType.wrappers.AcsJDynConfigFailureEx;

public interface MonitorDAO {

    public void store(ComponentData inData) throws Exception;

    public List getMonitorData(long monitorPointId, Timestamp startTimestamp,
            Timestamp stopTimestamp);

    public void close();

    public void openTransactionStore() throws Exception;

    public void closeTransactionStore() throws Exception;

    public boolean hasFailedToBeConfigured(ComponentData inData);

    public void setHasFailedToBeConfigured(ComponentData inData);

    public MonitorCharacteristicIDs getMonitorCharacteristicIDs(String configurationName, ComponentData inData)
            throws AcsJGettingMonitorCharacteristicsEx, AcsJDynConfigFailureEx;


}
