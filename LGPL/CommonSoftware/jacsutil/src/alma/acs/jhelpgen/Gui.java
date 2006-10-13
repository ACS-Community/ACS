/*
 * Created on Oct 4, 2006 by mschilli
 */
package alma.acs.jhelpgen;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;

import alma.acs.jhelpgen.Gen.AnchorNode;




public class Gui {

	static void showTree(Gen.AnchorNode n) {
		try {
			Gui inst = new Gui();
			inst.show(n);
			synchronized (inst) {
				inst.wait();
			}
		} catch (Exception e) {}
	}
	
	
	void show (Gen.AnchorNode n) {
		
		DefaultMutableTreeNode r = new DefaultMutableTreeNode(n);
		toTreeNode(n, r);
		JTree t = new JTree(r);

		
		final JFrame f = new JFrame();
		f.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing (WindowEvent evt) {
				synchronized (Gui.this) {
					Gui.this.notify();
				}
				f.dispose();
			}
		});
		f.getContentPane().add(new JScrollPane(t));
		f.pack();
		f.setVisible(true);
	}
	
	
	private void toTreeNode (Gen.AnchorNode src, DefaultMutableTreeNode trg) {
		for (Gen.AnchorNode subSrc : src.children) {
			DefaultMutableTreeNode subTrg = new DefaultMutableTreeNode(subSrc);
			trg.add(subTrg);
			toTreeNode(subSrc, subTrg);
		}
	}
	

}


