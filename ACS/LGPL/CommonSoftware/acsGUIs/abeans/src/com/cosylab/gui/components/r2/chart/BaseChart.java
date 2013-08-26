package com.cosylab.gui.components.r2.chart;

/**0*********1*********2*********3*********4*********5*********6*********7**
 * <CODE>BaseChart</CODE> is basic component, which binds together all
 * chart related interfaces and puts on screen by interfaces prepared
 * image. <CODE>BaseChart</CODE> buffers chart image.
 * Following interfaces are registered at <CODE>BaseChart</CODE>:
 * <DL>
 * 	<DT><CODE>Chartics</CODE><DD> a graphics wrapper
 *	<DT><CODE>ChartArea</CODE><DD> chart image producer and data holder
 *	<DT><CODE>ChartXAxis</CODE><DD> draws X axis
 *	<DT><CODE>ChartYAxis</CODE><DD> draws Y axis
 *	<DT><CODE>ChartViewManager</CODE><DD> handles chart scale and zooming
 * </DL>
 *
 * User is encouraged to implement its own custom behaviour in case 
 * default implementation is not suitable.
 *
 * <CODE>BaseChart</CODE> tends to be as lightweight as possible. All real 
 * charting work is done by upper interfaces. <CODE>BaseChart</CODE> is 
 * merely joins them together. Idea behind is, that you can not have one 
 * component (peace of code) to be in the same time highly customisable 
 * and really fast. This chart package ties to achieve both, performance 
 * and flexibility, with collaboration of objects.
 *
 * @see com.cosylab.gui.chart.Chartics
 * @see com.cosylab.gui.chart.ChartArea
 * @see com.cosylab.gui.chart.ChartXAxis
 * @see com.cosylab.gui.chart.ChartYAxis
 * @see com.cosylab.gui.chart.ChartViewManager
 *
 */
public class BaseChart extends javax.swing.JComponent implements ChartService {
	private java.awt.Rectangle chartRectangle;
	private java.awt.Insets userMargin = new java.awt.Insets(30, 10, 10, 40);
	public final static java.lang.String PROPERTY_CHART = "chart";
	private BaseChartCustomizer chartCustomizer;
	private Chartics chartics;
	private ChartArea chartArea;
	private ChartStyleManager chartStyleManager;
	private ChartXAxis xAxis;
	private ChartYAxis yAxis;
	private ChartViewManager viewManager = null;
	private ChartDecorator chartDecorator;
	private ChartDecorator defaultChartDecorator = new DefaultChartDecorator();
	private int updateRequests = 0;
	private ChartUpdater updater = new ChartUpdater(); 
	{
		updater.start();
	}

	class ChartUpdater extends Thread {
		private int frames = 30;
		private long rate = (long) (1000.0 / 30.0);
		private ChartUpdateRequest request;
		private Runnable later = new Runnable() {
			public void run() {
				repaint();
			}
		};

		public synchronized void queueRequest(ChartUpdateRequest r) {
			if (request == null)
				request = r;
			else if (request.getCode() > r.getCode())
				request = r;
			notify();
		}
		private synchronized ChartUpdateRequest pull() {
			ChartUpdateRequest r = request;
			request = null;
			return r;
		}
		public void run() {
			long time =
				System.currentTimeMillis()
					- (long) (1000.0 / (double) frames)
					- 1L;
			ChartUpdateRequest r;
			while (true) {
				r = pull();
				if ((r != null)
					&& (System.currentTimeMillis() - time
						> (long) (1000.0 / (double) frames))) {
					time = System.currentTimeMillis();
					updateMainBuffer(r);
					javax.swing.SwingUtilities.invokeLater(later);
				}

				synchronized (this) {
					try {
						if (request == null)
							wait();
						else
							wait(
								System.currentTimeMillis() - time
									> (long) (1000.0 / (double) frames)
									? (long) (1000.0 / (double) frames)
									: (long) (1000.0 / (double) frames)
										+ time
										- System.currentTimeMillis());
					} catch (Exception e) {
					}
				}
			}
		}
		public int getFrames() {
			return frames;
		}
		public synchronized void setFrames(int frames) {
			this.frames = frames;
			rate = (long) (1000.0 / (double) frames);
			notify();
		}

	}

	/**
	 * <code>BaseChart</code> constructor. All values and interfaces are initialized to the default implementation.
	 */
	public BaseChart() {
		super();
		getViewManager().setXScale(new Interval(0.5, 1.5));
		getViewManager().setYScale(new Interval(0.0, 10.0));
		getViewManager().setUserXScaleUsed(true);
		getViewManager().setUserYScaleUsed(true);
		setSize(200, 150);
		setBackground(java.awt.Color.white);
		updateLayout();

		addMouseListener(new java.awt.event.MouseAdapter() {
			public void mouseClicked(java.awt.event.MouseEvent e) {
				_mouseClicked(e);
			}
		});
	}
	private void _mouseClicked(java.awt.event.MouseEvent mouseEvent) {
		if (mouseEvent.getClickCount() != 2)
			return;
		getChartCustomizer().updateCustomizer();
		getChartCustomizer().show();
	}
	/**
	 * @see javax.swing.JComponent.addNotify()
	 *
	 * This method also update Layout.
	 */
	public void addNotify() {
		super.addNotify();
		updateLayout();
	}
	/**
	 * Draws axis around chart area.
	 * @param g java.awt.Graphics
	 */
	protected void drawAxis(java.awt.Graphics g) {
		try {
			getXAxis().drawAxis(g);
			getYAxis().drawAxis(g);
		} catch (Throwable t) {
			handleException(t);
		}

	}
	/**
	 * Draws chart.
	 * @param g java.awt.Graphics
	 */
	protected void drawData(java.awt.Graphics g, ChartUpdateRequest request) {
		try {
			getChartArea().drawData(getChartics(), request);
		} catch (Throwable t) {
			handleException(t);
		}

	}
	/**
	 * Returns <code>ChartArea</code> interface. If <code>ChartArea</code>  
	 * @return si.ijs.anka.databush.utilities.ChartArea
	 */
	public ChartArea getChartArea() {
		if (chartArea == null) {
			synchronized (this) {
				if (chartArea == null) {
					chartArea =
						defaultChartDecorator
							.getDefultChartFlavor()
							.getChartArea();
					chartArea.setChartService(this);
					updateLayout();
				}
			}
		}
		return chartArea;
	}
	/**
	 * This method return <code>BaseChartCustomizer</code> that is used for customizing some settings of chart.
	 * @return si.ijs.anka.databush.utilities.BaseChartCustomizer
	 */
	public BaseChartCustomizer getChartCustomizer() {
		if (chartCustomizer == null) {
			synchronized (this) {
				if (chartCustomizer == null)
					try {
						chartCustomizer = new BaseChartCustomizer(this);
					} catch (Throwable e) {
						handleException(e);
					}
			}
		}
		return chartCustomizer;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (28.1.2002 12:45:17)
	 * @return com.cosylab.gui.chart.ChartDecorator
	 */
	public ChartDecorator getChartDecorator() {
		return chartDecorator;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (11/24/00 18:24:52)
	 * @return int
	 */
	public int getChartDisplayablePointCount() {
		return getChartArea().getChartRectangle().width;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (11/27/00 15:15:46)
	 * @return si.ijs.anka.databush.utilities.Chartics
	 */
	public Chartics getChartics() {
		if (chartics == null) {
			synchronized (this) {
				if (chartics == null) {
					chartics = new Chartics();
					chartics.setBackground(getBackground());
					//				chartics.setImage((java.awt.image.BufferedImage)createImage(getWidth(),getHeight()));
				}
			}
		}
		return chartics;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (31.03.2001 00:32:16)
	 * @return si.ijs.kgb.chart.ChartStyleManager
	 */
	public ChartStyleManager getChartStyleManager() {
		if (chartStyleManager == null) {
			synchronized (this) {
				if (chartStyleManager == null) {
					chartStyleManager =
						defaultChartDecorator
							.getDefultChartFlavor()
							.getChartStyleManager();
					getChartArea().setChartService(this);
				}
			}
		}
		return chartStyleManager;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (28.1.2002 12:47:54)
	 * @return com.cosylab.gui.chart.ChartDecorator
	 */
	public ChartDecorator getDefaultChartDecorator() {
		return defaultChartDecorator;
	}
	/**
	 * This method returns margins of drawing area.
	 * @return java.awt.Insets
	 */
	public java.awt.Insets getMargin() {
		return userMargin;
	}
	/**
	 * This method return <code>ChartViewManager</code> class.
	 * @see com.cosylab.gui.chart.ChartViewManager
	 * @return com.cosylab.gui.chart.ChartViewManager
	 */
	public ChartViewManager getViewManager() {
		if (viewManager == null) {
			synchronized (this) {
				if (viewManager == null) {
					viewManager =
						defaultChartDecorator
							.getDefultChartFlavor()
							.getViewManager();
					getChartArea().setChartService(this);
					getXAxis().setViewManager(viewManager);
					getYAxis().setViewManager(viewManager);
				}
			}
		}
		return viewManager;
	}
	/**
	 * This method return <code>ChartXAxis</code>, which is interfdace
	 * for class that is used for drawing x axis.
	 */
	public ChartXAxis getXAxis() {
		if (xAxis == null) {
			synchronized (this) {
				if (xAxis == null) {
					xAxis =
						defaultChartDecorator.getDefultChartFlavor().getXAxis();
					xAxis.setChartSize(
						getChartArea().getChartRectangle().getSize());
					xAxis.setViewManager(getViewManager());
				}
			}
		}
		return xAxis;
	}
	/**
	 * This method return <code>ChartYAxis</code>, which is interfdace
	 * for class that is used for drawing y axis.
	 */
	public ChartYAxis getYAxis() {
		if (yAxis == null) {
			synchronized (this) {
				if (yAxis == null) {
					yAxis =
						defaultChartDecorator.getDefultChartFlavor().getYAxis();
					yAxis.setChartSize(
						getChartArea().getChartRectangle().getSize());
					yAxis.setViewManager(getViewManager());
				}
			}
		}
		return yAxis;
	}
	/**
	 * Called whenever the part throws an exception.
	 * @param exception java.lang.Throwable
	 */
	private void handleException(java.lang.Throwable exception) {

		/* Uncomment the following lines to print uncaught exceptions to stdout */
		System.out.println("--------- UNCAUGHT EXCEPTION ---------");
		exception.printStackTrace(System.out);
	}
	/**
	 * Starts simple chart demo presentation.
	 * @param args java.lang.String[]
	 */
	public static void main(java.lang.String[] args) {
		try {
			javax.swing.JFrame frame = new javax.swing.JFrame();
			BaseChart aSimpleChart;
			aSimpleChart = new BaseChart();
			frame.setContentPane(aSimpleChart);
			frame.setSize(aSimpleChart.getSize());
			frame.addWindowListener(new java.awt.event.WindowAdapter() {
				public void windowClosing(java.awt.event.WindowEvent e) {
					System.exit(0);
				};
			});
			frame.setVisible(true);
			class MyThread extends Thread {
				SinusFunction f = new SinusFunction();
				int speed = 50;
				boolean first = true;
				BaseChart c;
				public MyThread(BaseChart sc) {
					c = sc;
					c.getChartArea().addDataModel(new FunctionDataModel(f));
				}
				public synchronized void run() {
					while (true) {
						if (first) {
							try {
								wait(1000);
							} catch (InterruptedException e) {
							};
							first = false;
						}
						if (f.getPhase() == -1000000)
							f.setPhase(0);
						f.setPhase(f.getPhase() - ((double) speed) / 3000.0);
						c.updateChart(ChartUpdateRequest.RELOAD_ALL);
						try {
							wait(speed);
						} catch (InterruptedException e) {
						};
					}
				}
			};
			new MyThread(aSimpleChart).start();
		} catch (Throwable exception) {
			System.err.println(
				"Exception occurred in main() of javax.swing.JPanel");
			exception.printStackTrace(System.out);
		}
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (11/20/00 12:27:38)
	 * @param g java.awt.Graphics
	 */
	protected void paintComponent(java.awt.Graphics g) {

		//	if (ChartProperties.getChartProperties().isDebugPerformance() && updateRequest==null) System.out.println("Swing request");

		updateRequests = 0;

		if ((chartRectangle.width
			!= getWidth()
				- userMargin.left
				- userMargin.right
				- getYAxis().getPreferedWidth())
			|| (chartRectangle.height
				!= getHeight()
					- userMargin.top
					- userMargin.bottom
					- getXAxis().getPreferedHeight())) {

			updater.queueRequest(ChartUpdateRequest.UPDATE_ALL);
		}

		if (getChartics().getImage()!=null)
			g.drawImage(getChartics().getImage(), 0, 0, this);

		//	if (ChartProperties.getChartProperties().isDebugPerformance()) System.out.println("["+System.currentTimeMillis()+"]Updates per repaint: "+updateRequests);

	}
	public void setBackground(java.awt.Color c) {
		getChartics().setBackground(c);
		super.setBackground(c);
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (11/27/00 15:16:29)
	 * @param newChartArea si.ijs.anka.databush.utilities.ChartArea
	 */
	public void setChartArea(ChartArea newChartArea) {
		chartArea = newChartArea;
		getChartArea().setChartService(this);
		updateLayout();
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (28.1.2002 12:45:17)
	 * @param newChartDecorator com.cosylab.gui.chart.ChartDecorator
	 */
	public void setChartDecorator(ChartDecorator newChartDecorator) {
		chartDecorator = newChartDecorator;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (11/27/00 15:15:46)
	 * @param newChartics si.ijs.anka.databush.utilities.Chartics
	 */
	public void setChartics(Chartics newChartics) {
		chartics = newChartics;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (31.03.2001 00:32:16)
	 * @param newChartStyleManager si.ijs.kgb.chart.ChartStyleManager
	 */
	public void setChartStyleManager(ChartStyleManager newChartStyleManager) {
		chartStyleManager = newChartStyleManager;
		getChartArea().setChartService(this);
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (28.1.2002 12:47:54)
	 * @param newDefaultChartDecorator com.cosylab.gui.chart.ChartDecorator
	 */
	public void setDefaultChartDecorator(ChartDecorator newDefaultChartDecorator) {
		defaultChartDecorator = newDefaultChartDecorator;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (11/20/00 12:21:51)
	 * @param newMargin java.awt.Insets
	 */
	public void setMargin(java.awt.Insets newMargin) {
		userMargin = newMargin;
		updateLayout();
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (24/12/01 23:07:45)
	 * @param newViewManager com.cosylab.gui.chart.ChartViewManager
	 */
	public void setViewManager(ChartViewManager newViewManager) {
		viewManager = newViewManager;
		getXAxis().setViewManager(getViewManager());
		getYAxis().setViewManager(getViewManager());
		getChartArea().setChartService(this);
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (13/10/01 12:37:29)
	 * @param newXAxis si.ijs.kgb.chart.ChartXAxis
	 */
	public void setXAxis(ChartXAxis newXAxis) {
		xAxis = newXAxis;
		getXAxis().setChartSize(getChartArea().getChartRectangle().getSize());
		getXAxis().setViewManager(getViewManager());
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (13/10/01 12:38:45)
	 * @param newYAxis si.ijs.kgb.chart.ChartYAxis
	 */
	public void setYAxis(ChartYAxis newYAxis) {
		yAxis = newYAxis;
		getYAxis().setChartSize(getChartArea().getChartRectangle().getSize());
		getYAxis().setViewManager(getViewManager());
	}
	/**
	 * @see com.cosylab.gui.chart.ChartService#updateChart(ChartUpdateRequest)
	 */
	public void updateChart(ChartUpdateRequest request) {
		updateRequests++;
		updater.queueRequest(request);
	}

	/**
	 * Updates main offscreen buffer.
	 * @param request
	 */
	protected void updateMainBuffer(ChartUpdateRequest request) {

		if ((chartRectangle.width
			!= getWidth()
				- userMargin.left
				- userMargin.right
				- getYAxis().getPreferedWidth())
			|| (chartRectangle.height
				!= getHeight()
					- userMargin.top
					- userMargin.bottom
					- getXAxis().getPreferedHeight())) {
			updateLayout();
			getChartics().setImage(
				(java.awt.image.BufferedImage) createImage(getWidth(),
				getHeight()));

			request = ChartUpdateRequest.UPDATE_ALL;
		}

		if (request != null)
			updateImageBuffer(request);

		//	if (ChartProperties.getChartProperties().isDebugPerformance()) System.out.println("["+System.currentTimeMillis()+"]Updates per repaint: "+updateRequests);
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (11/20/00 12:27:38)
	 * @param g java.awt.Graphics
	 */
	protected synchronized void updateImageBuffer(ChartUpdateRequest request) {
		//	ChartProperties.getChartProperties().setProperty(ChartProperties.DEBUG_PROPERTY,"false");
		if (request == null)
			return;

		if (request == ChartUpdateRequest.RELOAD_ALL)
			getViewManager().revalidate();

		java.awt.Graphics ig = getChartics().getGraphics();
		java.awt.Shape sh = ig.getClip();

		// clear background
		ig.setColor(getBackground());

		ig.clearRect(
			0,
			0,
			getChartics().getImage().getWidth(),
			getChartics().getImage().getHeight());

		// starts with images
		ig.setColor(getForeground());

		java.awt.Dimension d = chartRectangle.getSize();
		ig.translate(chartRectangle.x, chartRectangle.y);
		ig.setClip(0, 0, d.width, d.height);

		drawData(ig, request);

		if (ChartProperties.getChartProperties().isDebugGraphics()) {
			java.awt.Dimension d1 =
				getChartArea().getChartRectangle().getSize();

			chartics.getGraphics().setColor(java.awt.Color.red);
			chartics.getGraphics().drawRect(0, 0, d.width - 1, d.height - 1);
			chartics.getGraphics().drawLine(0, 0, d.width - 1, d.height - 1);
			chartics.getGraphics().drawLine(0, d.height - 1, d.width - 1, 0);
			chartics.getGraphics().setColor(java.awt.Color.magenta);
			chartics.getGraphics().drawRect(
				getChartArea().getChartRectangle().x,
				getChartArea().getChartRectangle().y,
				d1.width - 1,
				d1.height - 1);
			chartics.getGraphics().drawLine(
				getChartArea().getChartRectangle().x,
				getChartArea().getChartRectangle().y,
				getChartArea().getChartRectangle().x + d1.width - 1,
				getChartArea().getChartRectangle().y + d1.height - 1);
			chartics.getGraphics().drawLine(
				getChartArea().getChartRectangle().x,
				getChartArea().getChartRectangle().y + d1.height - 1,
				getChartArea().getChartRectangle().x + d1.width - 1,
				getChartArea().getChartRectangle().y);
			chartics.getGraphics().setColor(getForeground());
		}

		ig.setClip(sh);

		ig.translate(
			getChartArea().getChartRectangle().x,
			getChartArea().getChartRectangle().y);

		drawAxis(ig);

		if (ChartProperties.getChartProperties().isDebugGraphics()) {
			java.awt.Dimension d1 =
				getChartArea().getChartRectangle().getSize();

			chartics.getGraphics().setColor(java.awt.Color.green);

			chartics.getGraphics().drawRect(
				0,
				d1.height - 1,
				d1.width - 1,
				getXAxis().getPreferedHeight() - 1);
			chartics.getGraphics().drawLine(
				0,
				d1.height - 1,
				d1.width - 1,
				d1.height - 1 + getXAxis().getPreferedHeight() - 1);
			chartics.getGraphics().drawLine(
				0,
				d1.height - 1 + getXAxis().getPreferedHeight() - 1,
				d1.width - 1,
				d1.height - 1);

			chartics.getGraphics().drawRect(
				-getYAxis().getPreferedWidth() + 1,
				0,
				getYAxis().getPreferedWidth() - 1,
				d1.height - 1);
			chartics.getGraphics().drawLine(
				-getYAxis().getPreferedWidth() + 1,
				0,
				0,
				d1.height - 1);
			chartics.getGraphics().drawLine(
				-getYAxis().getPreferedWidth() + 1,
				d1.height - 1,
				0,
				0);

			chartics.getGraphics().setColor(getForeground());
		}

		ig.translate(
			-chartRectangle.x - getChartArea().getChartRectangle().x,
			-chartRectangle.y - getChartArea().getChartRectangle().y);

		//	System.out.println("PAINT!");
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (13/10/01 13:13:30)
	 */
	protected synchronized void updateLayout() {
		java.awt.Rectangle r =
			new java.awt.Rectangle(
				userMargin.left + getYAxis().getPreferedWidth(),
				userMargin.top,
				getSize().width
					- userMargin.left
					- userMargin.right
					- getYAxis().getPreferedWidth(),
				getSize().height
					- userMargin.top
					- userMargin.bottom
					- getXAxis().getPreferedHeight());
		if (r.width < 0 || r.height < 0)
			chartRectangle = new java.awt.Rectangle(0, 0, 1, 1);
		else
			chartRectangle = r;
		getChartArea().setSize(chartRectangle.getSize());
		getXAxis().setChartSize(getChartArea().getChartRectangle().getSize());
		getYAxis().setChartSize(getChartArea().getChartRectangle().getSize());
	}
}
