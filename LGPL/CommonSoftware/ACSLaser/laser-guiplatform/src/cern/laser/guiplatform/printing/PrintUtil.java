/*
 * PrintUtil.java
 *
 * Created on April 2, 2004, 11:04 AM
 */

package cern.laser.guiplatform.printing;

//import cern.printing.CERNPrinterClient;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterAbortException;
import java.awt.print.PrinterIOException;
import java.awt.print.PrinterJob;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.Socket;
import java.net.URL;
import java.net.URLConnection;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.print.Doc;
import javax.print.DocFlavor;
import javax.print.DocPrintJob;
import javax.print.PrintException;
import javax.print.PrintService;
import javax.print.SimpleDoc;
import javax.print.StreamPrintService;
import javax.print.StreamPrintServiceFactory;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.print.attribute.standard.Copies;
import javax.print.attribute.standard.JobName;
import javax.print.attribute.standard.OrientationRequested;
import javax.print.attribute.standard.PageRanges;
import javax.print.attribute.standard.Sides;

/** Class for printing objects which implements interface <B>Printable</B>.
 * @author woloszyn
 */
public class PrintUtil {
    private static final int USE_DEFAULTS = 1;
    private static final int USE_FORM_CODES = 2;
    private static final int USE_OPTIONS = 3;
    
    /** printable object */    
    private Printable toPrint;
    /** name of printer (without suffix .print.cern.ch) */    
    private String printerName;
    /** number of copies */    
    private int numberOfCopies;
    /** if true two side printing */    
    private boolean isTwoSidePrint;
    /** if true all pages printing */    
    private boolean isAllPagePrint;
    /** if true range printing */    
    private boolean isRangePagePrint;
    /** first page to print */    
    private int rangeFrom;
    /** last page to print */    
    private int rangeTo;
    /** if true, printing to file */    
    private boolean isPrintToFile;
    /** destination file for printing ( mode: printing to file in dialog ) */    
    private File fileToPrint;
    /** if true, printing in landscape mode */    
    private boolean isLandscape;
    /** id true, all properties needed for printing are correct */    
    private boolean settingsOK;
    /** status of printing */    
    private int sendStatus;
    private int nextJob = 1;
    private String cfName;
    private String dfName;
    private String controlFile;
    private String userName = "null";
    private String jobName = "null";
    private int printOptions = USE_DEFAULTS;
    private int widthCode = -1;
    private String pagePosition = "twopage";
    private String rectoVerso = "rverso";
    private Socket sock = null;
    private int nextPort = 721;
    private final int timeout = 30;
    
    /** Creates a new instance of PrintUtil */
    public PrintUtil() {
        toPrint = null;
        settingsOK = false;
        sendStatus = 0;
        try {
            userName = System.getProperty("user.name");
        }
        catch (Exception e) {
            System.out.println("ERROR: Could not get user ID: " + e);
        }
    }
    /** Method obtains building number on the basis of ip address
     * @return Number of building as a string
     */
    public static String getBuildingNumber() {
        /* Finds the local host. */
        InetAddress localHost = null;
        try {
            localHost = InetAddress.getLocalHost();
        }
        catch (UnknownHostException e) {
            System.out.println("ERROR: Could not get local host name");
            return "0";
        }
        String localIP = localHost.getHostAddress();
        String switchIP = getSwitchIP( localIP );
        String switchName = getSwitchName(switchIP );
        int index = switchName.indexOf("-");
        String buildingNumber = "";
        if (index != -1 ) {
            buildingNumber = switchName.substring(1, index);
        }
        return buildingNumber;
    }
    /** Method obtains building number on the basis of ip address
     * @param ip IP od client in text format
     * @throws ArrayIndexOutOfBoundsException when the IP from parameter is in incorect format
     * @return switch IP address to which client is connected
     */    
    private static String getSwitchIP( String ip ) throws ArrayIndexOutOfBoundsException {
        String [] parts = new String[4];
        
        parts = ip.split("\\.");
        
        String switchIP = parts[0] + "." +
        parts[1] + "." +
        parts[2] + "." +
        ( new Integer( parts[3]).intValue()/ 64 * 64 + 3);
        
        return switchIP;
    }
    /** Method returns host name of switch with a given IP
     * @param switchIP switch IP
     * @return hostname of switch
     */    
    private static String getSwitchName( String switchIP ) {
        try {
            InetAddress ip = InetAddress.getByName(switchIP);
            return ip.getHostName();
        }
        catch (UnknownHostException uhe) {
            return null;
        }
    }
    /** Method returns printers available in building with given number
     * @param buildingNumber Number of building
     * @throws Exception occures when is problem with connection to IT Print Support web script or the format of
     * returned data has changed
     * @return table witch names of printers present in building
     */    
    public static String [] getPrintersInBuilding( String buildingNumber )
    throws Exception {
        if ( buildingNumber.compareTo("") == 0 ) {
            buildingNumber = "0";
        }
        BufferedReader in = null;
        buildingNumber = buildingNumber.trim().replace(' ','_');
        try {
            String address = "http://service-print.web.cern.ch/service-print/cgi-bin/printer-result.plx?system=UNIX&building="+buildingNumber;
            
            URL www = new URL(address);
            URLConnection connection = www.openConnection();
            
            in = new BufferedReader( new InputStreamReader( connection.getInputStream() ) );
            
            String inputLine = new String();;
            String html= new String();
            String printersList = new String();
            
            while ((inputLine = in.readLine()) != null) {
                html += inputLine + "<myBreak/>";
            }
            
            Matcher matcher = Pattern.compile("^.*<textarea name=\"results\" rows=10 cols=40>(.*)</textarea>.*$").matcher(html);
            
            if (matcher.lookingAt()) {
                printersList = matcher.group(1);
            }
            else {
                throw new Exception();
            }
            String [] printers = printersList.split("<myBreak/>");
            Arrays.sort(printers);
            
            if ( printers.length==1) {
                matcher = Pattern.compile("^.*Sorry, NONE found.*$").matcher( printers[0] );
                if ( matcher.lookingAt() ) {
                    return null;
                }
                else {
                    return printers;
                }
            }
            else {
                return printers;
            }
        }
        catch ( UnknownHostException uhe) {
            //uhe.printStackTrace();
            return null;
        }
        catch( MalformedURLException mue ) {
            //mue.printStackTrace();
            return null;
        }
        catch( IOException ioe){
            //ioe.printStackTrace();
            return null;
        }
        finally {
            if ( in != null ) {
                try {
                    in.close();
                }
                catch(IOException ioe) {
                    ioe.printStackTrace();
                }
            }
        }
    }
    /** Method returns printers available in client's building
     * @throws Exception occures when is problem with connection to IT Print Support web script or the format of
     * returned data has changed
     * @return table witch names of printers present in building
     */    
    public static String [] getPrintersInThisBuilding()
    throws Exception {
        return getPrintersInBuilding( getBuildingNumber());
    }
    /** Displays print dialog. When obtaining of printers from OS is successfull it
     * displays regular print dialog. In other case it displays dialog which obtains
     * printers on the base of building number from IT Print Support web script.
     * Obtaining client's building number is automatic, as well.
     * @throws MissingPrintableObjectException Printable object is missing. Use: setPrintable method
     * @return true: settings are correct and user wants to continue printing (print Button)
     * false: user pressed Cancel buttom
     */    
    public boolean printDialog() throws MissingPrintableObjectException {
        if( toPrint==null || ! (toPrint instanceof Printable) ) {
            throw new MissingPrintableObjectException();
        }
        
        PrinterJob printerJob = PrinterJob.getPrinterJob();
        
        PrintService printService = printerJob.getPrintService();  
        
        // mm: to get a homemade dialog instead of the system one (which doesn't actually work)
        printService = null;
        
        if (printService != null ) {            
            
            PageFormat pageFormat = printerJob.defaultPage();
            pageFormat.setOrientation(PageFormat.LANDSCAPE);
            
            PageFormat userFormat = printerJob.pageDialog(pageFormat);
            printerJob.setPrintable(toPrint, userFormat);
            
            printerJob.setJobName("Laser AC");
            
            try {
                if ( printerJob.printDialog() ) {
                    printerJob.print();
                }
                return true;
            }
            catch (PrinterAbortException a) {
                System.out.println("Printing terminated by user " +a);
                return false;
            }
            catch (PrinterIOException b) {
                System.out.println("Printer Error " +b);
                return false;
            }
            catch (Exception PrintException) {
                System.out.println("Printer Error " +PrintException);
                return false;
            }
        }
        else {
            PrintDialog dialog = new PrintDialog(new javax.swing.JFrame(), true);
            dialog.show();
            if ( dialog.areSettingsOK() ) {
                
                // reading parameters from dialog
                
                printerName = dialog.getPrinterName();
                numberOfCopies = dialog.getNumberOfCopies();
                isTwoSidePrint = dialog.isTwoSidedPrint();
                isAllPagePrint = dialog.isAllPagePrint();
                isRangePagePrint = dialog.isRangePagePrint();
                rangeFrom = dialog.getRangeFrom();
                rangeTo = dialog.getRangeTo();
                isPrintToFile = dialog.isPrintToFile();
                fileToPrint = dialog.getFileToPrint();
                isLandscape = dialog.isLandscape();
                
                settingsOK = true;
                
                return true;
            }
            else {
                return false;
            }
        }
    }
    
    /** method prints printable object on printer choosen in dialog with respecting
     * settings from this dialog
     * @throws SettingsNotCorrectException you should use printDialog() method before invoking print() method
     */    
    public void print() throws SettingsNotCorrectException {
        if( settingsOK==false ) {
            throw new SettingsNotCorrectException();
        }
        
        /* Use the pre-defined flavor for a Printable from an InputStream */
        DocFlavor flavor = DocFlavor.SERVICE_FORMATTED.PRINTABLE;
        
        /* Specify the type of the output stream */
        String psMimeType = DocFlavor.BYTE_ARRAY.POSTSCRIPT.getMimeType();
        
        /* Locate factory which can export a GIF image stream as Postscript */
        StreamPrintServiceFactory[] factories =
        StreamPrintServiceFactory.lookupStreamPrintServiceFactories(flavor, psMimeType);
        if (factories.length == 0) {
            System.err.println("No suitable factories");
            System.exit(0);
        }
        
        try {
            File tmp;
            if ( isPrintToFile ) {
                tmp = fileToPrint;
            }
            else {
                tmp = createTempFile();
            }
            
            /* Create a file for the exported postscript */
            FileOutputStream fos = new FileOutputStream( tmp );
            
            /* Create a Stream printer for Postscript */
            StreamPrintService sps = factories[0].getPrintService(fos);
            
            /* Create and call a Print Job */
            DocPrintJob pj = sps.createPrintJob();
            PrintRequestAttributeSet aset = new HashPrintRequestAttributeSet();
            
            // Adding parameters of document
            
            aset.add( new Copies( numberOfCopies ) );
            aset.add( new JobName("Laser AC", null) );
            if ( isLandscape ) {
                aset.add( OrientationRequested.LANDSCAPE );
            }
            else {
                aset.add( OrientationRequested.PORTRAIT );
            }
            if ( isTwoSidePrint == true ) {
                aset.add( Sides.DUPLEX );
            }
            else {
                aset.add( Sides.ONE_SIDED );
            }
            if( isRangePagePrint ) {
                aset.add( new PageRanges(rangeFrom, rangeTo) );
            }
            
            // Creating ps file
            
            Doc doc = new SimpleDoc( toPrint, flavor, null);
            
            pj.print(doc, aset);
            
            if ( ! isPrintToFile ) {
                // Printing ps on printer
                
                //CERNPrinterClient client = new CERNPrinterClient();
                //client.print( tmp, 'o' , printerName );
                print( tmp, 'o' , printerName );
            }
            fos.close();
            
            
        } catch (PrintException pe) {
            System.err.println(pe);
        }
        catch (IOException ie) {
            System.err.println(ie);
        }
    }
    
    /** sets reference to object which will be printed
     * @param painter reference to printable object. It have to implement interface Printable
     */    
    public void setPrintable( Printable painter ) {
        toPrint = painter;
    }
    
    /** Checks if printer with given name exists in network
     * @param printerName name of printer ( without suffix .print.cern.ch )
     * @return true if printer seems to be valid
     */    
    public static boolean checkPrinter( String printerName ) {
        try {
            InetAddress ip = InetAddress.getByName(printerName + ".print.cern.ch");
            return true;
        }
        catch( UnknownHostException uhe ){
            return false;
        }
    }
    /** */    
    public static void main(String[] args) {
        try {
            String [] printers = PrintUtil.getPrintersInBuilding("864");
            //String [] printers = PrintUtil.getPrintersInThisBuilding();
            if ( printers != null ) {
                for( int i=0; i<printers.length; i++ ) {
                    System.out.println(printers[i]);
                }
            }
            else {
                System.out.println("Sorry, none printers found");
            }
        }
        catch ( Exception e) {
            e.printStackTrace();
            System.out.println("+++ Probably IT Printer Support Website has changed !");
        }
    }
    /** Method creates temporary file. It file is used later for storing Postscript file
     * created before sending it to printer. It is created in temporary directory.
     * @throws IOException problem with creation file in temporary directory
     * @return created temporary file
     */    
    private File createTempFile() throws IOException {
        Random wheel = new Random() ;
        File temp;
        int unique = 0;
        do {
            // generate random a number 10,000,000 .. 99,999,999
            unique = ( wheel.nextInt() & Integer. MAX_VALUE ) %90000000 + 10000000 ;
            
        }
        while ( (new File("acout" + Integer.toString( unique ) + ".ps") ).exists() );
        
        temp = File.createTempFile("ac" + Integer.toString( unique ), ".ps");
        
        // Delete temp file when program exits.
        temp.deleteOnExit();
        
        return temp;
    }
    
    /** Sends a file to a printer to be printed. (method from cern.printing package)
     * <BR>
     * This method returns <I>true</I> if the job was sent successfully
     * or returns <I>false</I> if an error or timeout occured.
     * <BR>
     * The file's format must be specified as a single character,
     * and must be one of the following types:
     * <UL>
     * <LI><B>f</B> - a formatted file (the default)</LI>
     * <LI><B>l</B> - a binary/literal file</LI>
     * <LI><B>c</B> - a CIF file</LI>
     * <LI><B>d</B> - a DVI file</LI>
     * <LI><B>g</B> - the output from the plot(3X)</LI>
     * <LI><B>n</B> - a ditroff output file</LI>
     * <LI><B>o</B> - a Postscript output file</LI>
     * <LI><B>p</B> - print file using 'pr' format</LI>
     * <LI><B>r</B> - a text file with FORTRAN carriage</LI>
     * <LI><B>t</B> - a troff output file</LI>
     * <LI><B>v</B> - a raster file</LI>
     * </UL>
     * @param printableFile The file to be printed
     * @param fileType The format of the file (see above)
     * @param nameOfPrinter The name of the printer to use
     * @return blank
     */
    public boolean print(File printableFile, char fileType, String nameOfPrinter) {
        
        sendStatus = 0;  // reset the send status
        printerName = nameOfPrinter;
        
        String fileTypeString = String.valueOf(Character.toLowerCase(fileType));
        if (!fileTypeString.equals("f") &&    // formatted file (default)
        !fileTypeString.equals("l") &&    // binary/literal file
        !fileTypeString.equals("c") &&    // CIF file
        !fileTypeString.equals("d") &&    // DVI file
        !fileTypeString.equals("g") &&    // output from the plot(3X)
        !fileTypeString.equals("n") &&    // ditroff output file
        !fileTypeString.equals("o") &&    // Postscript output file
        !fileTypeString.equals("p") &&    // print file with 'pr' format
        !fileTypeString.equals("r") &&    // text with FORTRAN carriage
        !fileTypeString.equals("t") &&    // troff output file
        !fileTypeString.equals("v")) {    // raster file
            System.out.println("ERROR: Invalid file type: " + fileTypeString);
            return false;
        }
        
        //Gets the file specified
        //fileToPrint = new File(fileName);
        fileToPrint = printableFile;
        
        /* Builds the 3 characters string of the job number. */
        String jobString = String.valueOf(nextJob);
        while (jobString.length() < 3) {
            jobString = "0" + jobString;
        }
        nextJob++;
        if (nextJob == 1000) {
            nextJob = 1;
        }
        
        /* Finds the local host. */
        InetAddress localHost = null;
        try {
            localHost = InetAddress.getLocalHost();
        }
        catch (UnknownHostException e) {
            System.out.println("ERROR: Could not get local host name");
            return false;
        }
        
        /* The control file name */
        cfName = new String("cfA" + jobString + localHost.getHostName());
        
        /* The data file name */
        dfName = new String("dfA" + jobString + localHost.getHostName());
        
        //=============================================
        //------------START OF CONTROL FILE------------
        //=============================================
        
        controlFile = new String(
        
        "H" + localHost.getHostName() + '\n' +		// Host name
        "P" + userName + '\n');				// User name
        if (jobName.equals("null")) {
            controlFile +=	"J" + fileToPrint.getName() + '\n';                     // Job name
        }
        else {
            controlFile +=    "J" + jobName + '\n';
        }
        
        //if (useBanner) {
        //    controlFile +=	"L" + '\n';						// Print banner
        //}
        
        
        switch(printOptions) {
            case USE_DEFAULTS:   break;
            case USE_FORM_CODES: // indent is 1, width is the form code index
                if (widthCode != -1) {
                    int indent = 1;
                    controlFile +=	"I" + indent + '\n' +
                    "Z" + "XP_Width=" + widthCode + '\n';
                }
                break;
            case USE_OPTIONS:    // user specified options
                controlFile +=	"Z" + pagePosition +		// Z-Options
                "," + rectoVerso;
                
                if (widthCode != -1) {
                    controlFile +=	"," + "XP_Width=" + widthCode;
                }
                controlFile +=	'\n';
                break;
        }
        
        
        controlFile +=	fileTypeString + dfName + '\n';		                // Printing filter
        
        //=============================================
        //-------------END OF CONTROL FILE-------------
        //=============================================
        
        /* Resolves distant host. */
        InetAddress hostAddress = null;
        try {
            hostAddress = InetAddress.getByName(printerName + ".print.cern.ch");
        }
        catch (UnknownHostException e) {
            System.out.println("ERROR: Unknown Host");
        }
        
        // Tries to connect with a privileged local port (721-731 : see RFC 1179 specification)
        // If it fails (always when user is not root on a unix system), use a non-privileged port
        try {
            sock = new Socket(hostAddress,515,localHost,nextPort);
        } catch (IOException e) {}
        
        if (sock==null) {
            try {
                sock = new Socket(hostAddress,515);
            } catch (IOException e) {}
        }
        
        if (sock==null) {
            System.out.println("ERROR: Cannot make a socket to the printer");
        }
        
        // Another port next time
        nextPort++;
        if (nextPort==732) {
            nextPort=721;
        }
        
        if (sock != null) {
            
            boolean success = false;
            boolean sending = true;
            
            // Create the FileReader
            FileReader fr = null;
            try {
                fr = new FileReader(fileToPrint);
            }
            catch (FileNotFoundException e) {
                System.out.println("ERROR: FileReader could not find the specified file");
                sending = false;
                sendStatus = 3;
            }
            if (sending) {
                sendStatus = 1;
                try {
                    
                    /* Creates the streams to send commands and receive acknowledgements. */
                    BufferedReader ackReader = new BufferedReader(new InputStreamReader(sock.getInputStream()));
                    PrintWriter jobSender = new PrintWriter(sock.getOutputStream(),true);
                    
                    /* Command "Receive job" */
                    jobSender.println("\2" + printerName);
                    if ( waitForData(ackReader,timeout) && ackReader.read()=='\0') {
                        
                        /* Command "Receive control file" */
                        jobSender.println("\2" + controlFile.length() + ' ' + cfName);
                        if (waitForData(ackReader,timeout) && ackReader.read()=='\0') {
                            
                            /* Control file transmission */
                            jobSender.print(controlFile + '\0');
                            jobSender.flush();
                            
                            if (waitForData(ackReader,timeout) && ackReader.read()=='\0') {
                                /* Command "Receive data file" */
                                jobSender.println("\3" + fileToPrint.length() + ' ' + dfName);
                                if (waitForData(ackReader,timeout) && ackReader.read()=='\0') {
                                    System.out.println("Sending Job Data...");
                                    /* Data file transmission */
                                    while (fr.ready()) {
                                        jobSender.print((char)fr.read());
                                    }
                                    jobSender.print('\0');
                                    jobSender.flush();
                                    if (waitForData(ackReader,timeout) && ackReader.read()=='\0') {
                                        System.out.println("...Job Data Sent");
                                        success = true;
                                        sendStatus = 2;
                                        /* End of job transmission */
                                    }
                                }
                            }
                        }
                    }
                    
                    fr.close();
                    ackReader.close();
                    jobSender.close();
                    sock.close();
                    
                    if (!success) {
                        System.out.println("ERROR: Could not send job");
                        sendStatus = 3;
                    }
                }
                catch (IOException e) {
                    System.out.println("ERROR: IO Error: " + e);
                }
            }
        }
        return true;
    }
    
    /** Waits for data to become available from a reader, or a timeout.
     * Returns true if some data is received before the timeout specified.
     * @param reader The reader to read data from
     * @param timeout The timeout in seconds
     * @throws IOException if an I/O error occurs
     * @return true if reader is ready after given time
     */
    private static boolean waitForData(BufferedReader reader, int timeout) throws IOException {
        long time = System.currentTimeMillis();
        while (!reader.ready() && System.currentTimeMillis()-time<timeout*2000);
        return (reader.ready());
    }
    
    /** Exception means that user didn't set reference to printable object */    
    public class MissingPrintableObjectException extends Exception {
        public MissingPrintableObjectException() {
            super();
        }
    }
    /** Exception means that settings are not correct. Probably invoking print() method
     * before invoking printDialog() method
     */    
    public class SettingsNotCorrectException extends Exception {
        public SettingsNotCorrectException() {
            super();
        }
    }
}
