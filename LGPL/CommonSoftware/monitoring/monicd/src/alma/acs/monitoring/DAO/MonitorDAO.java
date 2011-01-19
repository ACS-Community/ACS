/** MonitorDAO is the interface that defines the interaccion
 * between Blobbers and DAO Persistence Layer.
 *
 * @author Pablo Burgos
 * @since ACS-8_0_0-B Jun2009
 * @version "@(#) $Id: MonitorDAO.java,v 1.1 2011/01/19 14:10:34 tstaig Exp $
 */
package alma.acs.monitoring.DAO;

import java.util.List;
import java.sql.Timestamp;

public interface MonitorDAO {

    public void store(ComponentData inData) throws Exception;

    public List getMonitorData(long monitorPointId, Timestamp startTimestamp,
            Timestamp stopTimestamp);

    public void close();

    public void openTransactionStore() throws Exception;

    public void closeTransactionStore() throws Exception;
}
