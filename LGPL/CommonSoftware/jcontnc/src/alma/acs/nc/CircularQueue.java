package alma.acs.nc;

import java.util.Vector;

import org.omg.CosNotification.StructuredEvent;

public class CircularQueue {
	private Vector<StructuredEvent> queue;
	private int length;
	
	public CircularQueue(int size){
		queue = new Vector<StructuredEvent>(size+1);
		size = 0;
	}
	
	public CircularQueue() {
		this(100);
	}
	
	public void push(StructuredEvent e){
		queue.add(e);
		length++;
		if(length > queue.capacity()-1){
			queue.remove(0);
			length--;
		}
	}
	
	public void clear(){
		queue.clear();
		length = 0;
	}
	
	public StructuredEvent pop(){
		StructuredEvent e = null;
		try{
			e = queue.remove(0);
			length--;
		}catch (IndexOutOfBoundsException ex)
		{}
		return e;
	}
}
