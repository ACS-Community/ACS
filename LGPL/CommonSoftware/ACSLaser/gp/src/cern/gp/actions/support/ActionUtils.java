package cern.gp.actions.support;

import java.awt.FlowLayout;

import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;

import org.openide.awt.Actions;
import org.openide.awt.Mnemonics;
import org.openide.util.ContextAwareAction;
import org.openide.util.Utilities;
import org.openide.util.actions.SystemAction;
import org.openide.windows.TopComponent;

/**
 * A few utility methods for Actions. This is partly an extension to the functionality
 * in openide.awt.Actions.
 * 
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class ActionUtils {

  private static ImageIcon BLANK_ICON = null;

  /** do not instantiate */
  private ActionUtils() {
  }

  /**
   * creates a JButton for the action class. This button takes its icon, description, shortcut etc from the
   * action class. It is enabled whenever the action is enabled.
   * @return a JButton
   */
  public static JButton createJButton(Class actionClass) {
    return createJButton(SystemAction.get(actionClass));
  }

  /**
   * creates a JButton for the action class that listens to Nodes in the specified TopComponent. This is useful, e.g.
   * when you want to place the JButton in a separate TopComponent (i.e. not in the Explorer TopComponent itself) 
   * @param tc the TopComponent that contains the nodes (typically this is an Explorer)
   * @param actionClass an action supported by one or more of the nodes in the TopComponent
   * @return a JButton
   * @since 2.0.5
   */
  public static JButton createJButton(TopComponent tc, Class actionClass) {
    Action act = actionForComp(tc, actionClass);
    return createJButton(act);
    //    if (act instanceof SystemAction) {
    //      return createJButton(act);
    //    } else {
    //      String msg = (String) act.getValue(Action.NAME);
    //
    //      String errMsg = "invalid action class, must inherit from SystemAction";
    //      System.err.println(errMsg);
    //      JButton but = new JButton(msg);
    //      but.setForeground(Color.red);
    //      but.setToolTipText(errMsg);
    //      return but;
    //    }
  }

  /**
   * creates a JButton for the SystemAction itself.
   * You should not need to use this method, rather use {@link #createJButton(Class)} 
   * If you find you need to use this action, please do not create actions yourself, 
   * use the {@link SystemAction#get(java.lang.Class) or {@link #actionForComp(TopComponent, Class)} instead.
   * 
   * @see #createJButton(Class) for more explanations
   * @return a JButton
   * @deprecated -- use {@link #createJButton(Action)}
   */
  public static JButton createJButton(SystemAction act) {
    JButton but = new JButton();
    // [PENDING] there is a problem when getIcon returns null: by default the icon is replaced with a text.
    // [PENDING] this produces buttons with twice the label.
    // [PENDING] to change this, one would have to work on the Actions.ButtonBridge class and especially
    // [PENDING] on the useTextIcons() method in there.
    // [PENDING] we have tried changing the icon in this method, but it doesn't work. We believeit gets updated 
    // [PENDING] dynamically in the ButtonBridge class.
    Action swingAction = act;
    Actions.connect(but, swingAction);
    Mnemonics.setLocalizedText(but, act.getName());
    return but;
  }

  /**
   * creates a JButton for the Action itself.
   * You should not need to use this method, rather use {@link #createJButton(Class)} 
   * If you find you need to use this action, please do not create actions yourself, 
   * use the {@link SystemAction#get(java.lang.Class) or {@link #actionForComp(TopComponent, Class)} instead.
   * 
   * @see #createJButton(Class) for more explanations
   * @return a JButton
   * @since 2.0.5
   */
  public static JButton createJButton(Action act) {
    JButton but = new JButton();
    // [PENDING] there is a problem when getIcon returns null: by default the icon is replaced with a text.
    // [PENDING] this produces buttons with twice the label.
    // [PENDING] to change this, one would have to work on the Actions.ButtonBridge class and especially
    // [PENDING] on the useTextIcons() method in there.
    // [PENDING] we have tried changing the icon in this method, but it doesn't work. We believeit gets updated 
    // [PENDING] dynamically in the ButtonBridge class.
    Actions.connect(but, act);

    String label = "invalid";
    if (act instanceof SystemAction) {
      label = ((SystemAction) act).getName();
    } else {
      label = act.getValue(Action.NAME).toString();
    }

    Mnemonics.setLocalizedText(but, label);

    return but;
  }

  /**
   * creates a JPanel with JButtons for the actions specified. The JPanel uses a FlowLayout. If you
   * don't like the JPanel, you can create the JPanel (or other component) yourself and use addJButtons
   * @return JPanel the panel containing the buttons
   */
  public static JPanel createJButtonPanel(Class[] actionClasses) {
    JPanel pan = new JPanel();
    pan.setLayout(new FlowLayout(FlowLayout.LEADING));
    return (JPanel) addJButtons((JComponent) pan, actionClasses);
  }

  /**
   * creates a JPanel with JButtons for action classes that listen to Nodes in the specified TopComponent. 
   * This is useful, e.g.when you want to place the Buttons in a separate TopComponent (i.e. not in the 
   * Explorer TopComponent itself).
   * 
   * @param tc the TopComponent that contains the nodes (typically this is an Explorer)
   * @param actionClasses actions supported by one or more of the nodes in the TopComponent
   * @return a JButton
   * @since 2.0.5
   */
  public static JPanel createJButtonPanel(TopComponent tc, Class[] actionClasses) {
    if (tc == null) {
      return createJButtonPanel(actionClasses);
    }
    JPanel pan = new JPanel();
    pan.setLayout(new FlowLayout(FlowLayout.LEADING));
    for (int ix = 0; ix < actionClasses.length; ix++) {
      pan.add(createJButton(tc, actionClasses[ix]));
    }
    return pan;
  }

  /**
   * Utility method, adds JButtons corresponding to the actionClasses to a JComponent
   * @return the JComponent passed as first paraameter, with the buttons added
   * @deprecated -- tell us (proj-gp-dev@cern.ch) if you really need it!
   */
  public static JComponent addJButtons(JComponent comp, Class[] actionClasses) {
    for (int ix = 0; ix < actionClasses.length; ix++) {
      comp.add((JComponent) createJButton(actionClasses[ix]));
    }
    return comp;
  }

  //  private static JComponent addJButtons(JComponent comp, Action[] actionClasses) {
  //    for (int ix = 0; ix < actionClasses.length; ix++) {
  //      comp.add((JComponent) createJButton(actionClasses[ix]));
  //    }
  //    return comp;
  //  }
  /**
   * utility method, returns a blank icon, can be used if an existing icon shall be "hidden"
   */
  public static ImageIcon getBlankIcon() {
    if (BLANK_ICON == null) {
      BLANK_ICON = new ImageIcon(Utilities.loadImage("org/openide/resources/actions/empty.gif"));
    }
    return BLANK_ICON;
  }

  /**
   * helper method, creates an  action that is "connected" to the nodes inside a TopComponent.
   * This makes it possible to create Action Buttons that listen to nodes in an Explorer but
   * are located outside the Explorer's TopComponent
   * 
   * @param tc the TopComponent (e.g. Explorer) to connect to
   * @param action the action class
   * @return the Context aware Action instance
   * @since 2.0.5
   */
  public static Action actionForComp(TopComponent tc, Class action) {
    Action a = SystemAction.get(action);
    return ((ContextAwareAction) a).createContextAwareInstance(tc.getLookup());
  }

}
