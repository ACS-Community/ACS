package cern.cmw.mom.test.chat;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class ChatFrame1 extends JFrame {
  JPanel contentPane;
  JToolBar toolBar = new JToolBar();
  JButton jButton1 = new JButton();
  JButton jButton2 = new JButton();
  JButton jButton3 = new JButton();
  ImageIcon image1;
  ImageIcon image2;
  ImageIcon image3;
  ImageIcon image4;
  JLabel statusBar = new JLabel();
  BorderLayout borderLayout1 = new BorderLayout();
  JButton jButton4 = new JButton();
  JPanel jPanel1 = new JPanel();
  JScrollPane jScrollPane1 = new JScrollPane();
  JTextField jTextField1 = new JTextField();
  BorderLayout borderLayout2 = new BorderLayout();
  JTextArea jTextArea1 = new JTextArea();
  TitledBorder titledBorder1;
  TitledBorder titledBorder2;

  //Construct the frame
  public ChatFrame1() {
    enableEvents(AWTEvent.WINDOW_EVENT_MASK);
    try {
      jbInit();
    }
    catch(Exception e) {
      e.printStackTrace();
    }
  }

  //Component initialization
  private void jbInit() throws Exception  {
    /*
    ClassLoader cl = this.getClass().getClassLoader();
    image1 = new ImageIcon(cl.getResource("Connect.gif"));
    image2 = new ImageIcon(cl.getResource("Disconnect.gif"));
    image3 = new ImageIcon(cl.getResource("Post.gif"));
    image4 = new ImageIcon(cl.getResource("Info.gif"));
    */
    image1 = new ImageIcon(ChatFrame1.class.getResource("images/Connect.gif"));
    image2 = new ImageIcon(ChatFrame1.class.getResource("images/Disconnect.gif"));
    image3 = new ImageIcon(ChatFrame1.class.getResource("images/Post.gif"));
    image4 = new ImageIcon(ChatFrame1.class.getResource("images/Info.gif"));

    contentPane = (JPanel) this.getContentPane();
    titledBorder1 = new TitledBorder("");
    titledBorder2 = new TitledBorder("");
    contentPane.setLayout(borderLayout1);
    this.setSize(new Dimension(400, 300));
    this.setTitle("CMW Chat Client");
    statusBar.setText("Not connected");
    jButton1.setIcon(image1);
    jButton1.setToolTipText("Connect");
    jButton2.setIcon(image2);
    jButton2.setEnabled(false);
    jButton2.setToolTipText("Disconnect");
    jButton3.setIcon(image3);
    jButton3.setEnabled(false);
    jButton3.setToolTipText("Post");
    //jSplitPane1.setTopComponent(jScrollPane1);
    jButton4.setToolTipText("Info");
    jButton4.setIcon(image4);
    jButton4.addActionListener(new java.awt.event.ActionListener() {

      public void actionPerformed(ActionEvent e) {
        helpAbout_actionPerformed(e);
      }
    });
    //image1.setDescription("");
    //jTextArea1.setPreferredSize(new Dimension(250, 150));
    //jTextArea1.setMinimumSize(new Dimension(200, 100));
    //jTextArea1.setSelectedTextColor(Color.magenta);
    //jTextArea1.setCursor(null);
    //jScrollPane1.setMinimumSize(new Dimension(200, 100));
    //jScrollPane1.setPreferredSize(new Dimension(250, 150));
    //jTextField1.setMinimumSize(new Dimension(200, 20));
    //jTextField1.setPreferredSize(new Dimension(250, 30));
    jPanel1.setLayout(borderLayout2);
    jScrollPane1.setAutoscrolls(true);
    jScrollPane1.setBorder(titledBorder1);
    jTextField1.setBorder(titledBorder2);
    jTextArea1.setForeground(Color.red);
    jTextArea1.setFont(new java.awt.Font("Dialog", 2, 12));
    toolBar.add(jButton1);
    toolBar.add(jButton2);
    toolBar.add(jButton3);
    toolBar.add(jButton4, null);
    contentPane.add(toolBar, BorderLayout.NORTH);
    contentPane.add(jPanel1, BorderLayout.CENTER);
    contentPane.add(statusBar, BorderLayout.SOUTH);
    jPanel1.add(jScrollPane1, BorderLayout.CENTER);
    jScrollPane1.getViewport().add(jTextArea1, null);
    jPanel1.add(jTextField1, BorderLayout.SOUTH);
  }

  //File | Exit action performed
  public void fileExit_actionPerformed(ActionEvent e) {
    System.exit(0);
  }

  //Help | About action performed
  public void helpAbout_actionPerformed(ActionEvent e) {
    ChatFrame1_AboutBox dlg = new ChatFrame1_AboutBox(this);
    Dimension dlgSize = dlg.getPreferredSize();
    Dimension frmSize = getSize();
    Point loc = getLocation();
    dlg.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x, (frmSize.height - dlgSize.height) / 2 + loc.y);
    dlg.setModal(true);
    dlg.show();
  }

  //Overridden so we can exit when window is closed
  protected void processWindowEvent(WindowEvent e) {
    super.processWindowEvent(e);
    if (e.getID() == WindowEvent.WINDOW_CLOSING) {
      System.exit(0);
    }
  }

}