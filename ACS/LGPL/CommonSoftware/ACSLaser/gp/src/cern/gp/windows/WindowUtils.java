/*
 * WindowUtils.java
 *
 * Created on September 21, 2002, 10:57 AM
 */
package cern.gp.windows;

import java.awt.Container;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.openide.windows.Mode;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import org.openide.windows.Workspace;

import cern.gp.util.Assertion;
import cern.gp.util.GPManager;

/**
 * Provides utility methods for windowing in NetBeans.
 * 
 * @author  Katarina Sigerud
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class WindowUtils {
  /** A frame that resides in the desktop */
  public static final int INTERNAL_FRAME = 0;
  /** A frame that resides as a separate window */
  public static final int TOP_FRAME = 1;
  /** The frame resides docked left, right, top, or bottom */
  public static final int DESKTOP_FRAME = 2;

  private static final String INTERNAL_FRAME_STRING = "INTERNAL_FRAME";
  private static final String TOP_FRAME_STRING = "TOP_FRAME";
  private static final String DESKTOP_FRAME_STRING = "DESKTOP_FRAME";

  private static final String CONSTRAINT_WEST = "WEST";

  /** persist a TopComponent only if it is opened */
  public static final int PERSIST_ONLY_OPENED = 1;
  /** dont persist the TopComponent */
  public static final int PERSIST_NEVER = 0;
  /** always persist the TopComponent */
  public static final int PERSIST_ALWAYS = 2;
  /** unkown persistence mode the TopComponent */
  public static final int PERSIST_UNKNOWN = -1;

  private static final String PERSISTENCE_TYPE = "PersistenceType";
  private static final String PERSIST_NEVER_STR = "Never";
  private static final String PERSIST_ONLY_OPENED_STR = "OnlyOpened";
  private static final String PERSIST_ALWAYS_STR = null;

  
  /** Indicates the default behavior for windows, MDI or SDI. Default is MDI. */
  private static int defaultFrameType = INTERNAL_FRAME;
  private static String[] possibleFrameTypes = { INTERNAL_FRAME_STRING, TOP_FRAME_STRING, DESKTOP_FRAME_STRING };

  private static Log log = LogFactory.getLog(WindowUtils.class);

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  /** do not instantiate */
  private WindowUtils() {
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  /**
   * Utility method: Sets the default windowing behavior, MDI or SDI. 
   * If set to <code>true</code> the default behavior for new windows is to reside in the desktop.
   * @param type indicates that the default windowing behavior, 
   * should be one of {@link #INTERNAL_FRAME}, {@link #TOP_FRAME}, or {@link #DESKTOP_FRAME}
   */
  public static void setFrameType(int frameType) {
    defaultFrameType = frameType;
  }

  /**
   * Utility method: opens a topcomponent in the current workspace in a mode with the 
   * indicated name. This method first looks if the mode already exists, or else
   * creates it. Opens the topcomponent in the desktop.
   *
   * @param component the component to open
   * @param modeName the (code) name of the mode
   * @return the mode the topcomponent was opened in.
   */
  public static Mode openInMode(TopComponent component, String modeName) {
    Workspace workspace = WindowManager.getDefault().getCurrentWorkspace();
    return openInMode(workspace, component, modeName, defaultFrameType);
  }

  /**
   * Utility method: opens a topcomponent in the given workspace in a mode with the 
   * indicated name. This method first looks if the mode already exists, or else
   * creates it. Opens the topcomponent in the desktop.
   *
   * @param workspace the workspace top open the component in
   * @param component the component to open
   * @param modeName the (code) name of the mode
   * @return the mode the topcomponent was opened in.
   */
  public static Mode openInMode(Workspace workspace, TopComponent component, String modeName) {
    return openInMode(workspace, component, modeName, defaultFrameType);
  }

  /**
   * Utility method: opens a topcomponent in the current workspace in a mode with the 
   * indicated name. This method first looks if the mode already exists, or else
   * creates it
   *
   * @param component the component to open
   * @param modeName the (code) name of the mode
   * @param frameType indicates the type of the window frame, 
   * should be one of {@link #INTERNAL_FRAME}, {@link #TOP_FRAME}, or {@link #DESKTOP_FRAME}
   * @return the mode the topcomponent was opened in.
   */
  public static Mode openInMode(TopComponent component, String modeName, int frameType) {
    Workspace workspace = WindowManager.getDefault().getCurrentWorkspace();
    return openInMode(workspace, component, modeName, frameType);
  }

  /**
   * Utility method: opens a topcomponent in the given workspace in a mode with the 
   * indicated name. This method first looks if the mode already exists, or else
   * creates it.
   * @param workspace the workspace top open the component in
   * @param component the component to open
   * @param modeName the (code) name of the mode
   * @param frameType indicates the type of the window frame, 
   * should be one of {@link #INTERNAL_FRAME}, {@link #TOP_FRAME}, or {@link #DESKTOP_FRAME}
   * @return the mode the topcomponent was opened in.
   */
  public static Mode openInMode(Workspace workspace, TopComponent component, String modeName, int frameType) {
    Assertion.assertTrue(workspace != null, "workspace != null");
    Assertion.assertTrue(component != null, "component != null");
    Assertion.assertTrue(modeName != null, "modeName != null");

    setPersistMode(component, PERSIST_NEVER);
    Mode mode = workspace.findMode(modeName);
    if (mode == null) {
      mode = workspace.createMode(modeName, modeName, null);
    }
    mode.dockInto(component);
    workspace.activate();
    frameResidesInDesktop(mode, frameType);
    component.open(workspace);
    component.requestFocus();
    return mode;
  }

  /**
   * Determine under which conditions the TopComponent is persisted to the NetBeans 
   * system directory
   * @param comp the component
   * @param option one of {@link #PERSIST_NEVER} or {@link #PERSIST_ONLY_OPENED}
   */
  public static void setPersistMode(TopComponent comp, int option) {
    Assertion.assertTrue(comp != null, "comp != null");
    Assertion.assertTrue(
      (PERSIST_NEVER == option || PERSIST_ALWAYS == option || PERSIST_ONLY_OPENED == option),
      "bad option: " + option);

    switch (option) {
      case PERSIST_NEVER :
        comp.putClientProperty(PERSISTENCE_TYPE, PERSIST_NEVER_STR);
        break;
      case PERSIST_ALWAYS :
        comp.putClientProperty(PERSISTENCE_TYPE, PERSIST_ALWAYS_STR);
        break;
      case PERSIST_ONLY_OPENED :
        comp.putClientProperty(PERSISTENCE_TYPE, PERSIST_ONLY_OPENED_STR);
        break;
      default :
        throw new AssertionError("[this should not happen: wrong option " + option);
    }
  }

  /**
   * Get the persistence mode of the given TopComponent
   * @param comp the component
   * @return one of {@link #PERSIST_NEVER} or {@link #PERSIST_ONLY_OPENED}
   */
  public static int getPersistMode(TopComponent comp) {
    Assertion.assertTrue(comp != null, "comp != null");
    String mode = (String) comp.getClientProperty(PERSISTENCE_TYPE);
    if (mode == PERSIST_ALWAYS_STR) {
      return PERSIST_ALWAYS;
    } else if (PERSIST_NEVER_STR.equals(mode)) {
      return PERSIST_NEVER;
    } else if (PERSIST_ONLY_OPENED_STR.equals(mode)) {
      return PERSIST_ONLY_OPENED;
    } else {
      return PERSIST_UNKNOWN;
    }
  }

  /**
   * Utility method: makes the given mode reside in the desktop instead of as a separate window.
   * @param mode the mode to make reside in the desktop
   */
  public static void frameResidesInDesktop(Mode mode) {
    frameResidesInDesktop(mode, defaultFrameType);
  }

  /**
   * Utility method: makes the given mode either reside in the desktop or display a separate window.
   * @param mode the mode to make reside in the desktop
   * @param frameType indicates the type of frame, MDI or SDI
   */
  public static void frameResidesInDesktop(Mode mode, int frameType) {
    Assertion.assertTrue(mode != null, "mode != null");

    try {
      ClassLoader classLoader = Mode.class.getClassLoader();
      Class modeImpl = Class.forName("org.netbeans.core.windows.ModeImpl", true, classLoader);
      Method getCurrentConstraintsMethod = modeImpl.getMethod("getCurrentConstraints", null);
      Object currentConstraints = getCurrentConstraintsMethod.invoke(mode, null);

      Method setFrameTypeMethod = modeImpl.getMethod("setFrameType", new Class[] { String.class });
      Method setVisibleMethod = modeImpl.getMethod("setVisible", new Class[] { boolean.class });
      Method isVisibleMethod = modeImpl.getMethod("isVisible", null);
      boolean wasVisible = ((Boolean) isVisibleMethod.invoke(mode, null)).booleanValue();

      if (frameType == DESKTOP_FRAME)
        currentConstraints = CONSTRAINT_WEST;

      Class windowUtils = Class.forName("org.netbeans.core.windows.util.WindowUtils", true, classLoader);
      Method findConstrainedModeMethod =
        windowUtils.getMethod("findConstrainedMode", new Class[] { Workspace.class, Object.class });
      Object constrainedMode =
        findConstrainedModeMethod.invoke(null, new Object[] { mode.getWorkspace(), currentConstraints });
      String frameTypeName = possibleFrameTypes[frameType];
      if (constrainedMode != null && frameType == DESKTOP_FRAME) {
        frameType = INTERNAL_FRAME;
        frameTypeName = possibleFrameTypes[frameType];
        currentConstraints = null;
      }

      Class windowTypeMgr = Class.forName("org.netbeans.core.windows.frames.WindowTypesManager", true, classLoader);
      Field frameFieldMode = windowTypeMgr.getField(frameTypeName);
      Method changeModeConstraintsMethod =
        windowUtils.getMethod("changeModeConstraints", new Class[] { modeImpl, Object.class });
      changeModeConstraintsMethod.invoke(null, new Object[] { mode, currentConstraints });
      setFrameTypeMethod.invoke(mode, new Object[] { frameFieldMode.get(windowTypeMgr)});

      if (wasVisible) {
        setVisibleMethod.invoke(mode, new Object[] { Boolean.TRUE });
        Class mainWindow = Class.forName("org.netbeans.core.windows.MainWindow", true, classLoader);
        Method getDefaultMethod = mainWindow.getMethod("getDefault", null);
        Method getContentPaneMethod = mainWindow.getMethod("getContentPane", null);
        Container cp = (Container) getContentPaneMethod.invoke(getDefaultMethod.invoke(null, null), null);
        cp.validate();
      }
    } catch (Exception e) {
      GPManager.notify(GPManager.EXCEPTION, e);
    }
  }
  /**
   * Utility method: Returns the TopComponent with the given programmatic name in the current workspace, 
   * or <code>null</code> if there is no such TopComponent.
   * @param componentName the programmatic name of the TopComponent
   * @return Returns the TopComponent with the given programmatic name, 
   * or <code>null</code> if there is no such TopComponent
   */
  public static TopComponent findTopComponent(String componentName) {
    Workspace ws = WindowManager.getDefault().getCurrentWorkspace();
    return findTopComponent(ws, componentName);
  }

  /**
   * Utility method: Returns the TopComponent with the given programmatic name in the given workspace, 
   * or <code>null</code> if there is no such TopComponent.
   * @param workspace the workspace in which to look for the TopComponent
   * @param componentName the programmatic name of the TopComponent
   * @return Returns the TopComponent with the given programmatic name, 
   * or <code>null</code> if there is no such TopComponent
   */
  public static TopComponent findTopComponent(Workspace workspace, String componentName) {
    Assertion.assertTrue(workspace != null, "workspace != null");
    Assertion.assertTrue(componentName != null, "componentName != null");

    Iterator modesIterator = workspace.getModes().iterator();
    while (modesIterator.hasNext()) {
      Mode mode = (Mode) modesIterator.next();
      TopComponent[] tcs = mode.getTopComponents();
      for (int i = 0; i < tcs.length; i++) {
        TopComponent element = tcs[i];
        if (element.getName().equals(componentName)) {
          return element;
        }
      }
    }

    return null;
  }

  /**
   * Utility method: creates a workspace and adds it to the WindowManager's list of workspaces.
   * The new workspace will appear as the last one in the list.
   *
   * @param codeName the code name of the new workspace
   * @param displayName the display name of the new workspace
   */
  public static Workspace createWorkspace(String codeName, String displayName) {
    Assertion.assertTrue(codeName != null, "codeName != null");

    if (displayName == null || displayName.equalsIgnoreCase(""))
      displayName = codeName;

    Workspace[] workspaces = new Workspace[WindowManager.getDefault().getWorkspaces().length + 1];

    Workspace[] currentList = WindowManager.getDefault().getWorkspaces();
    int i = 0;
    for (; i < currentList.length; i++) {
      workspaces[i] = currentList[i];
    }

    Workspace ws = WindowManager.getDefault().createWorkspace(codeName, displayName);
    workspaces[i] = ws;

    WindowManager.getDefault().setWorkspaces(workspaces);

    return ws;
  }

  //
  // -- implements XXX ----------------------------------------------
  //

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  private static boolean performDesktopFrameAction(
    Mode mode,
    TopComponent comp,
    ClassLoader classLoader,
    Class modeImplClass,
    Class windowUtilsClass)
    throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException {
    Workspace ws = mode.getWorkspace();
    Object currentConstraints = CONSTRAINT_WEST;
    Method findConstrainedModeMethod =
      windowUtilsClass.getMethod("findConstrainedMode", new Class[] { Workspace.class, Object.class });
    Object constrainedMode = findConstrainedModeMethod.invoke(null, new Object[] { ws, currentConstraints });
    if (constrainedMode != null) {
      Mode westMode = (Mode) constrainedMode;
      Method getContainerInstanceMethod = modeImplClass.getMethod("getContainerInstance", null);
      Object tcc = getContainerInstanceMethod.invoke(mode, null);
      if (tcc != null) {
        Class topComponentContainerClass =
          Class.forName("org.netbeans.core.windows.frames.TopComponentContainer", true, classLoader);
        Method removeTopComponentMethod =
          topComponentContainerClass.getMethod("removeTopComponent", new Class[] { TopComponent.class });
        removeTopComponentMethod.invoke(tcc, new Object[] { comp });
        Method dockIntoMethod = modeImplClass.getMethod("dockInto", new Class[] { TopComponent.class, Object.class });
        dockIntoMethod.invoke(westMode, new Object[] { comp, currentConstraints });

        return true;
      }
    }
    return false;
  }
}
