/** MonitorDAO is the interface that defines the interaction
 * between Blobbers and DAO Persistence Layer.
 *
 * @author Pablo Burgos
 * @since ACS-8_0_0-B Jun2009
 * @version "@(#) $Id: MonitorDAO.java,v 1.3 2011/07/01 08:36:56 hsommer Exp $
 */
package alma.acs.monitoring.DAO;

public interface MonitorDAO {

    public void store(ComponentData inData) throws Exception;

    public void close();

    public void openTransactionStore() throws Exception;

    public void closeTransactionStore() throws Exception;
}
