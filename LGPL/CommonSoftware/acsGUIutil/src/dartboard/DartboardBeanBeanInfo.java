package dartboard;

import java.beans.*;

public class DartboardBeanBeanInfo extends SimpleBeanInfo {
    
    // Bean descriptor //GEN-FIRST:BeanDescriptor
    /*lazy BeanDescriptor*/
    private static BeanDescriptor getBdescriptor(){
        BeanDescriptor beanDescriptor = new BeanDescriptor  ( DartboardBean.class , null );//GEN-HEADEREND:BeanDescriptor
        
        // Here you can add code for customizing the BeanDescriptor.
        
        return beanDescriptor;         }//GEN-LAST:BeanDescriptor
    
    
    // Property identifiers //GEN-FIRST:Properties
    private static final int PROPERTY_moonAzimuth = 0;
    private static final int PROPERTY_moonDeclination = 1;
    private static final int PROPERTY_moonElevation = 2;
    private static final int PROPERTY_moonPhase = 3;
    private static final int PROPERTY_moonRightAscension = 4;
    private static final int PROPERTY_settable = 5;
    private static final int PROPERTY_sunAzimuth = 6;
    private static final int PROPERTY_sunDeclination = 7;
    private static final int PROPERTY_sunElevation = 8;
    private static final int PROPERTY_sunRightAscension = 9;
    private static final int PROPERTY_telescopeAzimuth = 10;
    private static final int PROPERTY_telescopeDestinationAzimuth = 11;
    private static final int PROPERTY_telescopeDestinationElevation = 12;
    private static final int PROPERTY_telescopeElevation = 13;
    private static final int PROPERTY_windDirection = 14;
    private static final int PROPERTY_windSpeed = 15;

    // Property array 
    /*lazy PropertyDescriptor*/
    private static PropertyDescriptor[] getPdescriptor(){
        PropertyDescriptor[] properties = new PropertyDescriptor[16];
    
        try {
            properties[PROPERTY_moonAzimuth] = new PropertyDescriptor ( "moonAzimuth", DartboardBean.class, null, "setMoonAzimuth" );
            properties[PROPERTY_moonDeclination] = new PropertyDescriptor ( "moonDeclination", DartboardBean.class, null, "setMoonDeclination" );
            properties[PROPERTY_moonElevation] = new PropertyDescriptor ( "moonElevation", DartboardBean.class, null, "setMoonElevation" );
            properties[PROPERTY_moonPhase] = new PropertyDescriptor ( "moonPhase", DartboardBean.class, null, "setMoonPhase" );
            properties[PROPERTY_moonRightAscension] = new PropertyDescriptor ( "moonRightAscension", DartboardBean.class, null, "setMoonRightAscension" );
            properties[PROPERTY_settable] = new PropertyDescriptor ( "settable", DartboardBean.class, null, "setSettable" );
            properties[PROPERTY_sunAzimuth] = new PropertyDescriptor ( "sunAzimuth", DartboardBean.class, null, "setSunAzimuth" );
            properties[PROPERTY_sunDeclination] = new PropertyDescriptor ( "sunDeclination", DartboardBean.class, null, "setSunDeclination" );
            properties[PROPERTY_sunElevation] = new PropertyDescriptor ( "sunElevation", DartboardBean.class, null, "setSunElevation" );
            properties[PROPERTY_sunRightAscension] = new PropertyDescriptor ( "sunRightAscension", DartboardBean.class, null, "setSunRightAscension" );
            properties[PROPERTY_telescopeAzimuth] = new PropertyDescriptor ( "telescopeAzimuth", DartboardBean.class, null, "setTelescopeAzimuth" );
            properties[PROPERTY_telescopeDestinationAzimuth] = new PropertyDescriptor ( "telescopeDestinationAzimuth", DartboardBean.class, null, "setTelescopeDestinationAzimuth" );
            properties[PROPERTY_telescopeDestinationElevation] = new PropertyDescriptor ( "telescopeDestinationElevation", DartboardBean.class, null, "setTelescopeDestinationElevation" );
            properties[PROPERTY_telescopeElevation] = new PropertyDescriptor ( "telescopeElevation", DartboardBean.class, null, "setTelescopeElevation" );
            properties[PROPERTY_windDirection] = new PropertyDescriptor ( "windDirection", DartboardBean.class, null, "setWindDirection" );
            properties[PROPERTY_windSpeed] = new PropertyDescriptor ( "windSpeed", DartboardBean.class, null, "setWindSpeed" );
        }
        catch( IntrospectionException e) {}//GEN-HEADEREND:Properties
        
        // Here you can add code for customizing the properties array.
        
        return properties;         }//GEN-LAST:Properties
    
    // EventSet identifiers//GEN-FIRST:Events

    // EventSet array
    /*lazy EventSetDescriptor*/
    private static EventSetDescriptor[] getEdescriptor(){
        EventSetDescriptor[] eventSets = new EventSetDescriptor[0];//GEN-HEADEREND:Events
        
        // Here you can add code for customizing the event sets array.
        
        return eventSets;         }//GEN-LAST:Events
    
    // Method identifiers //GEN-FIRST:Methods
    private static final int METHOD_getMoonAzimuth0 = 0;
    private static final int METHOD_getMoonDeclination1 = 1;
    private static final int METHOD_getMoonElevation2 = 2;
    private static final int METHOD_getMoonPhase3 = 3;
    private static final int METHOD_getMoonRightAscension4 = 4;
    private static final int METHOD_getSunAzimuth5 = 5;
    private static final int METHOD_getSunDeclination6 = 6;
    private static final int METHOD_getSunElevation7 = 7;
    private static final int METHOD_getSunRightAscension8 = 8;
    private static final int METHOD_getTelescopeAzimuth9 = 9;
    private static final int METHOD_getTelescopeElevation10 = 10;
    private static final int METHOD_getWindDirection11 = 11;
    private static final int METHOD_getWindSpeed12 = 12;
    private static final int METHOD_setMoonDeclination13 = 13;
    private static final int METHOD_setMoonRightAscension14 = 14;
    private static final int METHOD_setSunDeclination15 = 15;
    private static final int METHOD_setSunRightAscension16 = 16;

    // Method array 
    /*lazy MethodDescriptor*/
    private static MethodDescriptor[] getMdescriptor(){
        MethodDescriptor[] methods = new MethodDescriptor[17];
    
        try {
            methods[METHOD_getMoonAzimuth0] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("getMoonAzimuth", new Class[] {}));
            methods[METHOD_getMoonAzimuth0].setDisplayName ( "" );
            methods[METHOD_getMoonDeclination1] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("getMoonDeclination", new Class[] {}));
            methods[METHOD_getMoonDeclination1].setDisplayName ( "" );
            methods[METHOD_getMoonElevation2] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("getMoonElevation", new Class[] {}));
            methods[METHOD_getMoonElevation2].setDisplayName ( "" );
            methods[METHOD_getMoonPhase3] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("getMoonPhase", new Class[] {}));
            methods[METHOD_getMoonPhase3].setDisplayName ( "" );
            methods[METHOD_getMoonRightAscension4] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("getMoonRightAscension", new Class[] {}));
            methods[METHOD_getMoonRightAscension4].setDisplayName ( "" );
            methods[METHOD_getSunAzimuth5] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("getSunAzimuth", new Class[] {}));
            methods[METHOD_getSunAzimuth5].setDisplayName ( "" );
            methods[METHOD_getSunDeclination6] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("getSunDeclination", new Class[] {}));
            methods[METHOD_getSunDeclination6].setDisplayName ( "" );
            methods[METHOD_getSunElevation7] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("getSunElevation", new Class[] {}));
            methods[METHOD_getSunElevation7].setDisplayName ( "" );
            methods[METHOD_getSunRightAscension8] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("getSunRightAscension", new Class[] {}));
            methods[METHOD_getSunRightAscension8].setDisplayName ( "" );
            methods[METHOD_getTelescopeAzimuth9] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("getTelescopeAzimuth", new Class[] {}));
            methods[METHOD_getTelescopeAzimuth9].setDisplayName ( "" );
            methods[METHOD_getTelescopeElevation10] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("getTelescopeElevation", new Class[] {}));
            methods[METHOD_getTelescopeElevation10].setDisplayName ( "" );
            methods[METHOD_getWindDirection11] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("getWindDirection", new Class[] {}));
            methods[METHOD_getWindDirection11].setDisplayName ( "" );
            methods[METHOD_getWindSpeed12] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("getWindSpeed", new Class[] {}));
            methods[METHOD_getWindSpeed12].setDisplayName ( "" );
            methods[METHOD_setMoonDeclination13] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("setMoonDeclination", new Class[] {Double.TYPE}));
            methods[METHOD_setMoonDeclination13].setDisplayName ( "" );
            methods[METHOD_setMoonRightAscension14] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("setMoonRightAscension", new Class[] {Double.TYPE}));
            methods[METHOD_setMoonRightAscension14].setDisplayName ( "" );
            methods[METHOD_setSunDeclination15] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("setSunDeclination", new Class[] {Double.TYPE}));
            methods[METHOD_setSunDeclination15].setDisplayName ( "" );
            methods[METHOD_setSunRightAscension16] = new MethodDescriptor ( dartboard.DartboardBean.class.getMethod("setSunRightAscension", new Class[] {Double.TYPE}));
            methods[METHOD_setSunRightAscension16].setDisplayName ( "" );
        }
        catch( Exception e) {}//GEN-HEADEREND:Methods
        
        // Here you can add code for customizing the methods array.
        
        return methods;         }//GEN-LAST:Methods
    
    
    private static final int defaultPropertyIndex = -1;//GEN-BEGIN:Idx
    private static final int defaultEventIndex = -1;//GEN-END:Idx
    
    
 //GEN-FIRST:Superclass
    
    // Here you can add code for customizing the Superclass BeanInfo.
    
 //GEN-LAST:Superclass
    
    /**
     * Gets the bean's <code>BeanDescriptor</code>s.
     *
     * @return BeanDescriptor describing the editable
     * properties of this bean.  May return null if the
     * information should be obtained by automatic analysis.
     */
    public BeanDescriptor getBeanDescriptor() {
        return getBdescriptor();
    }
    
    /**
     * Gets the bean's <code>PropertyDescriptor</code>s.
     *
     * @return An array of PropertyDescriptors describing the editable
     * properties supported by this bean.  May return null if the
     * information should be obtained by automatic analysis.
     * <p>
     * If a property is indexed, then its entry in the result array will
     * belong to the IndexedPropertyDescriptor subclass of PropertyDescriptor.
     * A client of getPropertyDescriptors can use "instanceof" to check
     * if a given PropertyDescriptor is an IndexedPropertyDescriptor.
     */
    public PropertyDescriptor[] getPropertyDescriptors() {
        return getPdescriptor();
    }
    
    /**
     * Gets the bean's <code>EventSetDescriptor</code>s.
     *
     * @return  An array of EventSetDescriptors describing the kinds of
     * events fired by this bean.  May return null if the information
     * should be obtained by automatic analysis.
     */
    public EventSetDescriptor[] getEventSetDescriptors() {
        return getEdescriptor();
    }
    
    /**
     * Gets the bean's <code>MethodDescriptor</code>s.
     *
     * @return  An array of MethodDescriptors describing the methods
     * implemented by this bean.  May return null if the information
     * should be obtained by automatic analysis.
     */
    public MethodDescriptor[] getMethodDescriptors() {
        return getMdescriptor();
    }
    
    /**
     * A bean may have a "default" property that is the property that will
     * mostly commonly be initially chosen for update by human's who are
     * customizing the bean.
     * @return  Index of default property in the PropertyDescriptor array
     * 		returned by getPropertyDescriptors.
     * <P>	Returns -1 if there is no default property.
     */
    public int getDefaultPropertyIndex() {
        return defaultPropertyIndex;
    }
    
    /**
     * A bean may have a "default" event that is the event that will
     * mostly commonly be used by human's when using the bean.
     * @return Index of default event in the EventSetDescriptor array
     *		returned by getEventSetDescriptors.
     * <P>	Returns -1 if there is no default event.
     */
    public int getDefaultEventIndex() {
        return defaultEventIndex;
    }
}

