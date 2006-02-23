package alma.acs.genfw;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import org.xml.sax.InputSource;

import de.bmiag.genfw.ConfigurationException;
import de.bmiag.genfw.instantiator.InstantiatorEnvironment;
import de.bmiag.genfw.instantiator.InstantiatorException;
import de.bmiag.genfw.instantiator.xml.io.StreamFactory;
import de.bmiag.genfw.meta.ElementSet;

import genfwutil.frontends.xmi.XMIInstantiator;

/**
 * A custom version of <code>XMIInstantiator</code> which performs additional checks in {@link #loadDesign(InstantiatorEnvironment)},
 * where it calls {@link #checkModelFileFormat()}.
 * 
 * @author hsommer
 */
public class AlmaXMIInstantiator extends XMIInstantiator {

	// separate field since XMIInstantiator#modelFilename is private
	private File modelFile;

	public AlmaXMIInstantiator(File modelFile, File xmlMapFile, File metaMapFile, String toolAdapterClassname) {
		super(modelFile, xmlMapFile, metaMapFile, toolAdapterClassname);
	}

	public void setModelFile(File f) {
		// XMIInstantiator#modelFilename is private -- perhaps change that later
		super.setModelFile(f);
		modelFile = f;
	}

	public ElementSet loadDesign(InstantiatorEnvironment env) throws ConfigurationException, InstantiatorException {
		checkModelFileFormat();
		return super.loadDesign(env);
	}
	
	
	/**
	 * Does simple checks to see if the model file has a valid format.
	 * <p> 
	 * Currently only checks if an XMI file saved with MagicDraw is "rich XMI", because otherwise
	 * some subtle mistakes can occur; these may or may not let the generator run fail.   
	 */
	protected void checkModelFileFormat() throws InstantiatorException {
		try {
			// StreamFactory takes care of packed files and encoding issues
			InputSource insrc = StreamFactory.createInputSource(modelFile);
			if (insrc == null) {
				throw new InstantiatorException("model file '" + modelFile.getAbsolutePath() + "' not found or not readable.");
			}
			
			Reader modelReader = null;
			if (insrc.getCharacterStream() != null) {
				modelReader = insrc.getCharacterStream();
			}
			else {
				InputStream instream = insrc.getByteStream();
				modelReader = new InputStreamReader(instream);
			}
			
			String magicDrawPoorXMILine = "<!-- This xmi file is optimized for MagicDraw UML. Some references are not saved. -->";			
			if (foundString(modelReader, magicDrawPoorXMILine, 10)) {
				throw new InstantiatorException("model file '" + modelFile.getAbsolutePath() + "' was not saved as \"rich XMI\" in Magic Draw!");
			}			
		} catch (InstantiatorException e) {
			throw e;
		}
		catch (Exception e) {
			throw new InstantiatorException("failed to check model file", e);
		}
	}

	
	
	/**
	 * Searches <code>maxNumLines</code> lines into the stream given by <code>modelReader</code> and 
	 * returns true if the string <code>searchString</code> is contained in any of these lines.
	 * Case sensitive.
	 * <p>
	 * 
	 * @throws IOException if reading the model from the stream fails.
	 */
	private static boolean foundString(Reader modelReader, String searchString, int maxNumLines) throws IOException {
		BufferedReader buffRead = null;
		boolean ret = false;
		try {
			buffRead = new BufferedReader(modelReader);
			String line = null;
			int lineCount = 0;
			while ((line = buffRead.readLine()) != null && ++lineCount <= maxNumLines) {
				if (line.indexOf(searchString) != -1) {
					ret = true;
					break;
				}
			}
		} finally {
			if (buffRead != null) {
				try {
					buffRead.close();
				} catch (IOException ie) {
				}
			}
		}
		return ret;
	}

}
