package alma.acs.commandcenter.trace;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.border.EmptyBorder;

/**
 * Default view for a flow trace.
 */
public class DefaultChecklistPanel extends JPanel implements FlowListener {

	protected Flow flow;
	protected JTextArea infoF = new JTextArea(4, 30);
	
	protected String tryText;
	protected Icon tryIcon;
	protected String okText;
	protected Icon okIcon;
	protected String errText;
	protected Icon errIcon;
		
	
	public DefaultChecklistPanel(Flow flowDefinition) {
		this(flowDefinition, "trying", null, "ok", null, "err", null);
	}
	
	public DefaultChecklistPanel(Flow flowDefinition, String tryText, Icon tryIcon, String okText, Icon okIcon, String errText, Icon errIcon) {

		this.tryText = tryText;
		this.tryIcon = tryIcon;
		this.okText = okText;
		this.okIcon = okIcon;
		this.errText = errText;
		this.errIcon = errIcon;
				
		this.setFlow(flowDefinition);

		this.setBorder(new EmptyBorder(1,0,1,0));		
		JPanel stripes = new JPanel(new GridLayout(0, 1));

		Enumeration<Object> en = ((Flow.Node) flowDefinition.name2node.get(null)).preorderEnumeration();
		// skip first element (== root)
		en.nextElement();
		Vector<String> vec = new Vector<String>();
		while (en.hasMoreElements()) {
			Flow.Node n = (Flow.Node) en.nextElement();

			Stripe s = createStripe(n.getName());
			stripes.add(s);
			name2stripe.put(s.name, s);

			StringBuffer buf = new StringBuffer(32);
			for (int i = 0; i < n.getLevel(); i++)
				buf.append("   ");
			buf.append(String.valueOf(n.getName()));
			vec.add(buf.toString());
		}

		infoF.setEditable(false);
		infoF.setLineWrap(true);

		this.setLayout(new BorderLayout());
		this.add(stripes, BorderLayout.NORTH);
		this.add(infoF, BorderLayout.CENTER);
	}

	

	public void setFlow (Flow newFlow) {
		if (this.flow != null)
			flow.removeListener(this);

		this.flow = newFlow;
		flow.addListener(this);
	}

	public Flow getFlow () {
		return flow;
	}

	Map<String, Stripe> name2stripe = new HashMap<String, Stripe>();

	public void reset (Flow f, Object info) {
		Iterator<Stripe> iter = name2stripe.values().iterator();
		while (iter.hasNext()) {
			Stripe s = (Stripe) iter.next();
			s.statusF.setText("");
			s.statusF.setIcon(null);
		}
		infoF.setText("");
	}

	public void completion (Flow f) {}

	public void trying (Flow f, String name) {
		Stripe s = ((Stripe) name2stripe.get(name));
		if (s != null) {
			s.statusF.setText(tryText);
			s.statusF.setIcon(tryIcon);
		}
	}

	public void success (Flow f, String name) {
		Stripe s = ((Stripe) name2stripe.get(name));
		if (s != null) {
			s.statusF.setText(okText);
			s.statusF.setIcon(okIcon);
		}
	}

	public void failure (Flow f, String name, Object info) {
		Stripe s = ((Stripe) name2stripe.get(name));
		if (s != null) {
			s.statusF.setText(errText);
			s.statusF.setIcon(errIcon);
		}
		
		if (info instanceof Throwable) {
			String text = "";
			Throwable t = (Throwable) info;
			do {
				text += t.toString() + "\n";
				t = t.getCause();
			} while (t != null);

			info = text;
		}

		infoF.setText(String.valueOf(info));
	}


	/** Factory method, overridable */
	protected Stripe createStripe (String name) {
		return new Stripe(name);
	}

	protected class Stripe extends JPanel {


		protected String name;
		protected JLabel nameF = new JLabel();
		protected JLabel statusF = new JLabel();

		protected Stripe(String name) {

			this.name = name;
			this.nameF.setText(name);

			this.setBorder(new EmptyBorder(0,5,1,10));		
			this.setLayout(new BorderLayout());
			this.add(nameF, BorderLayout.CENTER);
			this.add(statusF, BorderLayout.EAST);
		}

		
		
	}

}
