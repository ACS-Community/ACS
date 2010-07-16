package cl.utfsm.samplingSystemUI;
import javax.swing.JOptionPane;

public class Timer extends Thread {
	 protected int rate=50;

	    private   int elapsed;
	    private   int time_ms;
	    private   boolean run = true;

	    public Timer(int time_ms) {
	        this.time_ms = time_ms;
	        elapsed      = 0;
	    }
	   /** starts the timer, if timeout then calls the timeout method*/  
	    public void run() {
	        while(run) {
	            try {
	                Thread.sleep(rate);
	            } catch(InterruptedException e) {
	                continue;
	            }
	            synchronized(this) {
	                elapsed = elapsed + rate;
	                if(elapsed > time_ms) {
	                     timeout();
	                     break;
	                }    

	            }        
	        }

	    }
	    /** this method is called when the time is out!*/
	    public void timeout() {
	        JOptionPane.showMessageDialog(null, "There's no response from the container, maybe the container is down", "Time out!", JOptionPane.ERROR_MESSAGE);
	    }
	    /** reset the timer*/
	    public void reset() {
		elapsed = 0;
	        run = false;
	    }

}
