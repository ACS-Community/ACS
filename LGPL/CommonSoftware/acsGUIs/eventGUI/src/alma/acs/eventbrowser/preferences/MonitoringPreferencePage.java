package alma.acs.eventbrowser.preferences;

import java.io.IOException;

import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.jface.preference.*;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

import alma.acs.eventbrowser.Application;

/**
 * This class represents a preference page that is contributed to the
 * Preferences dialog. By subclassing <samp>FieldEditorPreferencePage</samp>,
 * we can use the field support built into JFace that allows us to create a page
 * that is small and knows how to save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They are stored in the
 * preference store that belongs to the main plug-in class. That way,
 * preferences can be accessed directly via the preference store.
 */

public class MonitoringPreferencePage extends FieldEditorPreferencePage
		implements IWorkbenchPreferencePage {

	public static final String AUTO_MONITOR = "prefs_auto_monitor";

	private ScopedPreferenceStore preferences;

	public MonitoringPreferencePage() {
		super(GRID);
		this.preferences = new ScopedPreferenceStore(new ConfigurationScope(),
				Application.PLUGIN_ID);
		setPreferenceStore(preferences);

		setDescription("Define the default monitoring strategy -- manual or periodic.");
	}

	/**
	 * Creates the field editors. Field editors are abstractions of the common
	 * GUI blocks needed to manipulate various types of preferences. Each field
	 * editor knows how to save and restore itself.
	 */
	public void createFieldEditors() {
		// addField(new DirectoryFieldEditor(PreferenceConstants.P_PATH,
		// "&Directory preference:", getFieldEditorParent()));
		addField(new BooleanFieldEditor(AUTO_MONITOR, "&Monitor periodically",
				getFieldEditorParent()));

		// addField(new RadioGroupFieldEditor(
		// PreferenceConstants.P_CHOICE,
		// "An example of a multiple-choice preference",
		// 1,
		// new String[][] { { "&Choice 1", "choice1" }, {
		// "C&hoice 2", "choice2" }
		// }, getFieldEditorParent()));
		// addField(
		// new StringFieldEditor(PreferenceConstants.P_STRING, "A &text
		// preference:", getFieldEditorParent()));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}

	@Override
	public boolean performOk() {
		try {
			preferences.save();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return super.performOk();

	}
}