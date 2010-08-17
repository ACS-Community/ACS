package si.ijs.acs.objectexplorer.engine;

import com.cosylab.gui.components.r2.DataFormatter;

public class DataElementFormatter {
	public static String unpackArray(Object array, String lineStart, int level, boolean expand) {
		StringBuffer result=new StringBuffer();
		Class type=array.getClass().getComponentType();
		Object[] list = null;
		if (type.isPrimitive())
			list = DataFormatter.convertPrimitiveArray(array);
		else
			list = (Object[]) array;
		result.append("[Array of "+ type+"], length = "+list.length);
		for (int i = 0; i < list.length; i++){
			if(list[i].getClass().isArray())
				result.append("\n"+lineStart+" ("+i+") "+unpackArray(list[i],lineStart+"  ",level+1,expand));
			if(list[i] instanceof DataElement)
				result.append("\n"+lineStart+" ("+i+") "+((DataElement)list[i]).toString(lineStart+"  ",level+1,expand));
			else
				result.append("\n"+lineStart+" ("+i+") "+DataFormatter.unpackReturnValue(list[i],lineStart+"  ",level+1,expand));
		}
		result.append("\n");
		return result.toString();
	}
}
