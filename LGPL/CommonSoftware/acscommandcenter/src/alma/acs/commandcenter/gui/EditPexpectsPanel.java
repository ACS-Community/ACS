/*
 * Created on Apr 6, 2004 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.lang.reflect.Field;

import alma.acs.commandcenter.app.CommandCenterLogic;



public class EditPexpectsPanel extends EditCommandsPanel {
    
    public EditPexpectsPanel(CommandCenterLogic controller) {
        super(controller);
    }

    @Override
	protected boolean isInterestingField(Field f) {
        return (f.getType().isAssignableFrom(String.class) && f.getName()
                .indexOf("expect") != -1);
    }

    @Override
	protected void setFieldValue(FieldStripe s) throws Exception {
        s.field.set(s.object, new String(s.valueF.getText()));
    }
}
////////////////////////////////////////////////////////
/// ------------------- API ------------------------ ///
////////////////////////////////////////////////////////

////////////////////////////////////////////////////////
/// ----------------- Internal --------------------- ///
////////////////////////////////////////////////////////

//
//
//
//
//
//
//
//
//
//
//
//