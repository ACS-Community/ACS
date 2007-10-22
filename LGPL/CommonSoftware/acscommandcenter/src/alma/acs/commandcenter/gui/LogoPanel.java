/*
 * Created on Apr 6, 2004 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.JPanel;



public class LogoPanel extends JPanel {

    private Color startcolor;
    private Color endcolor;

    protected LogoPanel(Color start, Color end) {
        super();
        this.startcolor = start;
        this.endcolor = end;
        this.setOpaque(false);
    }

    
    private int oldW;
    private int oldH;
    protected GradientPaint gradientPaint = null;
    
    @Override
	protected void paintComponent(Graphics g) {
        try {
            int w = getWidth();
            int h = getHeight();
            
            // optimization: reuse GradientPaint if possible
            if (gradientPaint == null || w != oldW || h != oldH) {
            	gradientPaint = new GradientPaint(0, 0, startcolor, w, h, endcolor, false);
            	oldW = w;
            	oldH = h;
            }
            
            Graphics2D g2 = (Graphics2D) g;
            g2.setPaint(gradientPaint);
            g2.fillRect(0, 0, w, h);

        } catch (InternalError exc) {
            // sometimes (but rarely), there's an error in g2.fillRect():
            /* java.lang.InternalError: MaskFill can only fill with colors
             *        at sun.java2d.loops.MaskFill.makePrimitive(MaskFill.java:120)
             */
        }
        super.paintComponent(g);
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