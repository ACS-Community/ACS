/*
 * Created on Nov 11, 2004 by mschilli
 */
package alma.acs.commandcenter.util;



/**
 *
 * @author mschilli
 */
public class StringRingBuffer {

		protected char[] data;
		protected int next = 0;
		protected boolean isFillingUp = true;

		
		////////////////////////////////////////////////////////
		/// ------------------- API ------------------------ ///
		////////////////////////////////////////////////////////


		public StringRingBuffer(int size) {
			data = new char[size];
		}

		
		/**
		 * Returns the element that gets overwritten by this add-operation,
		 * <code>-1</code> if none is overwritten since the RingBuffer is
		 * still filling up (that is, <code>isFillingUp</code> is true. 
		 */
		public int add (char obj) {
			int overwritten = -1;

			synchronized (data) {
				overwritten = data[next];
				
				data[next] = obj;
				next = inc(next);

				if (isFillingUp && next == 0)
					isFillingUp = false;
			}

			return overwritten;
		}

		public int size () {
			return (isFillingUp)? next : data.length;
		}


		public char[] getAll () {
			synchronized (data) {
				return copyToNewArray(size());
			}
		}

		public boolean equals (String other) {
			
			if (other.length() != this.size())
				return false;

			int idx;
			int otherIdx = 0;
			
			if (isFillingUp) {
				idx = 0;
				while (idx < next) {
					if (data[idx] != other.charAt(otherIdx)) {
						return false;
					}
					idx++;
					otherIdx++;
				}

			} else {
				idx = next;
				while (idx < data.length) {
					if (data[idx] != other.charAt(otherIdx)) {
						return false;
					}
					idx++;
					otherIdx++;
				}
				idx = 0;
				while (idx < next) {
					if (data[idx] != other.charAt(otherIdx)) {
						return false;
					}
					idx++;
					otherIdx++;
				}
			}

			return true;
		}
		
		@Override
		public String toString() {
			return new String(getAll());
		}
		
		////////////////////////////////////////////////////////
		/// ----------------- Internal --------------------- ///
		////////////////////////////////////////////////////////

		private char[] copyToNewArray(int size) {
			
			char[] ret = new char[size];
			
			if (isFillingUp) {
				System.arraycopy(data, 0, ret, 0, next);
			} else {
				System.arraycopy(data, next, ret, 0, data.length - next);
				System.arraycopy(data, 0, ret, data.length - next, next);
			}
			return ret;
		}

		private int inc (int i) {
			i += 1;
			return (i == data.length) ? 0 : i;
		}






	////////////////////////////////////////////////////////
	/// ---------------- Inner Types ------------------- ///
	////////////////////////////////////////////////////////

}

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