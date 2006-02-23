/*
 * ACTreeTableView.java
 *
 * Created on February 23, 2004, 9:19 AM
 */

/**
 *
 * @author  woloszyn
 */
package cern.laser.guiplatform.explorer;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;

import javax.swing.DefaultCellEditor;
import javax.swing.ImageIcon;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.RepaintManager;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;

import org.apache.log4j.Logger;
import org.openide.explorer.view.TreeTableView;
import org.openide.nodes.Node;

import cern.laser.guiplatform.alarms.AlarmBean;
import cern.laser.guiplatform.alarms.AlarmBeanNode;
import cern.laser.guiplatform.alarms.AlarmNodeManagerImpl;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.LogFactory;

public class ACTreeTableView extends TreeTableView implements Printable/*, PropertyChangeListener*/ {
    
    private Node nodes;
    /** logger */
    private static Logger logger = LogFactory.getLogger(AlarmNodeManagerImpl.class.getName());
    
    /** Creates a new instance */
    public ACTreeTableView(Node nodes) {
        super( );
        this.nodes = nodes;
        logger.debug("constructor VIEW");
        
        //treeTable.setDefaultRenderer(Object.class,  new MyTableCellRenderer(this));
        
        treeTable.setOpaque(true);
        treeTable.setSelectionBackground( Color.BLACK);//new Color(66,93,115) );
        treeTable.setSelectionForeground(Color.YELLOW);
        
        treeTable.setBackground(Color.BLACK);//new Color(66,93,115) );
        treeTable.setForeground(new Color(155,155,155));
        treeTable.setFocusable(true);
        
        treeTable.setGridColor(new Color(70, 92, 113));
        //treeTable.setIntercellSpacing(new Dimension(30,50));
        treeTable.setRowHeight(50);
        
    }
    
    public void setRenderers(){
        logger.debug("setRenderer()");
        
        // width of columns
        
        try {
            // first column with selecting icon
            treeTable.getColumnModel().getColumn(0).setMinWidth(30);
            treeTable.getColumnModel().getColumn(0).setMaxWidth(150);
            treeTable.getColumnModel().getColumn(0).setPreferredWidth(50);
            
            // date / new
            treeTable.getColumnModel().getColumn(1).setMinWidth(50);
            treeTable.getColumnModel().getColumn(1).setMaxWidth(150);
            treeTable.getColumnModel().getColumn(1).setPreferredWidth(80);
            
            // time
            treeTable.getColumnModel().getColumn(2).setMinWidth(50);
            treeTable.getColumnModel().getColumn(2).setMaxWidth(150);
            treeTable.getColumnModel().getColumn(2).setPreferredWidth(100);
        }
        catch(IndexOutOfBoundsException iobe) {
            
        }
        
        for( int i=0; i<treeTable.getColumnModel().getColumnCount(); i++){
            treeTable.getColumnModel().getColumn(i).setCellRenderer( new MyTableCellRenderer(Color.LIGHT_GRAY,ACTreeTableView.this));
            treeTable.getColumnModel().getColumn(i).setHeaderRenderer( new MyTableHeaderRenderer(Color.GRAY, ACTreeTableView.this) );
            DefaultCellEditor editor = new DefaultCellEditor( new JTextField() );
            editor.setClickCountToStart(8);
            treeTable.getColumnModel().getColumn(i).setCellEditor( editor );
        }
        
        // height of rows
        //setRowHeight( (int) ((double) AppRegister.getInstance().getFontSize() * 1.5) );
        setRowHeight( AppRegister.getInstance().getFontSize() + 4 );
    }
    public void setRowHeight(final int size) {
        new Thread() {
            public void run() {
                treeTable.setRowHeight( size );
            }
        }.start();
    }
    public Node getNodes( ) {
        //logger.debug("getNodes()");
        return nodes;
    }
    
    public int print( Graphics g, PageFormat pageFormat, int pageIndex) throws PrinterException {
        
        
        // for faster printing turn double buffering off
        RepaintManager.currentManager(this).setDoubleBufferingEnabled(false);
        
        Graphics2D g2 = (Graphics2D) g;
        g2.setColor( Color.black );
        g2.setFont( new Font("Arial",Font.PLAIN, 10) );
        
        int fontHeight = g2.getFontMetrics().getHeight();
        int fontDesent = g2.getFontMetrics().getDescent();
        
        // leave room for page number
        double pageHeight = pageFormat.getImageableHeight() - fontHeight;
        double pageWidth = pageFormat.getImageableWidth();
        double tableWidth = (double) treeTable.getColumnModel().getTotalColumnWidth();
        
        double scale = 1;
        if ( tableWidth >= pageWidth ) {
            scale = pageWidth / tableWidth;
        }
        
        double headerHeightOnPage = treeTable.getTableHeader().getHeight() * scale;
        
        double tableWidthOnPage = tableWidth * scale;
        double oneRowHeight = ( treeTable.getRowHeight() + treeTable.getRowMargin() ) * scale;
        
        int numRowsOnAPage = (int) ((pageHeight - headerHeightOnPage) / oneRowHeight );
        
        double pageHeightForTable = oneRowHeight * numRowsOnAPage;
        int totalNumPages = (int) Math.ceil(( (double) treeTable.getRowCount()) / numRowsOnAPage );
        
        if( pageIndex >= totalNumPages ) {
            return NO_SUCH_PAGE;
        }
        
        g2.translate( pageFormat.getImageableX(), pageFormat.getImageableY() );
        
        // put page no. at the bottom center
        g2.drawString("Page: " +(pageIndex + 1) + "of " + totalNumPages ,(int)pageWidth / 2 - 35,(int) ( pageHeight + fontHeight - fontDesent ));
        
        g2.translate( 0f, headerHeightOnPage );
        g2.translate( 0f, -pageIndex * pageHeightForTable );
        
        if ( ( pageIndex + 1 ) == totalNumPages ) {
            int lastRowPrinted = numRowsOnAPage * pageIndex;
            int numRowsLeft = treeTable.getRowCount() - lastRowPrinted;
            g2.setClip( 0, (int) ( pageHeightForTable * pageIndex ),(int) Math.ceil( tableWidthOnPage ),(int) Math.ceil( oneRowHeight * numRowsLeft ));
        }
        // else clip to the entire area available.
        else {
            g2.setClip( 0, (int) ( pageHeightForTable * pageIndex ),
            (int) Math.ceil( tableWidthOnPage ),
            (int) Math.ceil( pageHeightForTable ));
        }
        
        g2.scale( scale, scale );
        
        // paint the table
        treeTable.paint( g2 );
        
        g2.scale( 1 / scale, 1 / scale);
        g2.translate( 0f, pageIndex * pageHeightForTable );
        g2.translate( 0f, -headerHeightOnPage );
        g2.setClip( 0, 0,(int) Math.ceil( tableWidthOnPage ),(int) Math.ceil( headerHeightOnPage ));
        g2.scale( scale, scale );
        
        // paint header at top
        treeTable.getTableHeader().paint( g2 );
        
        RepaintManager.currentManager(this).setDoubleBufferingEnabled(true);
        
        return Printable.PAGE_EXISTS;
    }
    /*
    public void propertyChange(java.beans.PropertyChangeEvent evt) {
        logger.debug("propertyChange, event = " + evt);
    }
     */
    class MyTableCellRenderer extends DefaultTableCellRenderer {
        Color backgroundColor;
        ACTreeTableView view;
        
        public MyTableCellRenderer(ACTreeTableView myView) {
            super();
            backgroundColor = Color.GRAY;
            setOpaque(true);
            view = myView;
            
        }
        public MyTableCellRenderer(Color backgroundColor, ACTreeTableView myView) {
            super();
            this.backgroundColor = backgroundColor;
            setOpaque(true);
            view = myView;
        }
        
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            setEnabled(true);
            //setOpaque(true);
            if(row>=0){
                try {
                    AlarmBeanNode node = (AlarmBeanNode) view.getNodes().getChildren().getNodes()[row];
                    try {
                        AlarmBean alarm = (AlarmBean) node.getBean();
                        
                        super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column);
                        
                        switch ( column ) {
                            case 0:
                                setIcon( new ImageIcon(alarm.getNodeIcon(isSelected) ) );
                                setBackground( alarm.getStandardBackgroundColor() );
                                setForeground( alarm.getForegroundColor() );
                                setFont( alarm.getFont() );                                
                                break;
                            case 1:                                
                                if ( alarm.isNew() ) {
                                    value = "   N";
                                    setBackground( alarm.getStandardForegroundColor() );
                                    setForeground( alarm.getStandardBackgroundColor() );
                                    super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column);
                                    setFont( alarm.getFont() );                                                                                                        
                                }
                                else {
                                    setBackground( alarm.getBackgroundColor() );
                                    setForeground( alarm.getForegroundColor() );
                                    setFont( alarm.getFont() );                                
                                }
                                
                                break;
                            default :
                                setBackground( alarm.getBackgroundColor() );
                                setForeground( alarm.getForegroundColor() );
                                setFont( alarm.getFont() );
                        }                                                
                    }
                    catch(ClassCastException cce) {
                        logger.debug("ClassCastException obj="+node.getBean());
                    }
                }
                catch(IndexOutOfBoundsException ex) {
                    logger.debug("IndexOutOfBoundsException, row="+row);
                }
            }
            return this;
        }
        public void setValue(Object value){
            try {
                org.openide.nodes.PropertySupport.Reflection refl = (org.openide.nodes.PropertySupport.Reflection) value;
                try{
                    //logger.debug("refl.getValue().toString()="+refl.getValue().toString());
                    //logger.debug("refl.getValueType().getName()="+refl.getValueType().getName());
                    if( refl != null ) {
                        setText( (String) refl.getValue().toString() );
                    }
                }
                catch(java.lang.reflect.InvocationTargetException ite){
                    
                }
                catch(IllegalAccessException iae){
                    
                }
            }
            catch(ClassCastException cce){
                super.setValue(value);
            }
            
        }
    }
    class MyTableHeaderRenderer extends DefaultTableCellRenderer {
        Color backgroundColor;
        ACTreeTableView view;
        
        public MyTableHeaderRenderer(ACTreeTableView myView) {
            super();
            backgroundColor = Color.GRAY;
            setOpaque(true);
            view = myView;
        }
        public MyTableHeaderRenderer(Color backgroundColor, ACTreeTableView myView) {
            super();
            this.backgroundColor = backgroundColor;
            setOpaque(true);
            view = myView;
        }
        
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column);
            
            setEnabled(table == null || table.isEnabled());
            setOpaque(true);
            setBackground( new Color(188, 188, 188) );
            setForeground( Color.BLACK);
            //setDoubleBuffered(true);
            setVerticalTextPosition( SwingConstants.CENTER );
            return this;
        }
        public void setValue(Object value){
            try {
                org.openide.nodes.PropertySupport.Reflection refl = (org.openide.nodes.PropertySupport.Reflection) value;
                try{
                    if( refl != null ) {
                        setText( refl.getValue().toString() );
                    }
                }
                catch(java.lang.reflect.InvocationTargetException ite){
                    ite.printStackTrace();
                }
                catch(IllegalAccessException iae){
                    iae.printStackTrace();
                }
            }
            catch (ClassCastException cce) {
                super.setValue(value);
            }
            
        }
    }
}
