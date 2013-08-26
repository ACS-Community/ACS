package com.cosylab.gui.components.r2;

/**
 * Data model used by <code>JCheckList</code> to store the checked state
 * for each entry. The data model provides this functionality transparently.
 * Each object that is added to the model will be wraped together with
 * a boolean variable indicating its checked state. Model provides complete
 * functionality of the <code>javax.swing.DefaultListModel</code>.
 * <p>
 * Creation date: (12/25/2001 16:27:43)
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public class CheckListModel extends javax.swing.DefaultListModel {

	/**
	 * Delegate class used to store objects and provide <code>checked</code> property in 
	 * <code>CheckListModel</code> transparently.
	 * Creation date: (24-Oct-01 11:48:40 AM)
	 * @author: 
	 */
	private class CheckListElement {
		private boolean checked = false;
		private Object element = null;

		/**
		 * CheckListItem constructor comment.
		 */
		public CheckListElement(Object element) {
			super();
			this.element = element;

		}
		public CheckListElement(Object element, boolean isChecked) {
			this(element);
			this.checked = isChecked;

		}
		/**
		 * Implements standard comparison for equality.<p>
		 * Instead of comparing delegate <code>CheckListItem</code> it compares the actual objects stored
		 * in them. Uses the <code>equals</code> method of these objects.
		 * Returns false if invalid type case.
		*/

		public final boolean equals(Object other) {
			if (other instanceof CheckListElement) {
				return element.equals(((CheckListElement) other).getElement());
			} else {
				return false;
			}
		}
		/**
		 * Gets the element stored in this wrapper.
		 * <p>
		 * Creation date: (12/25/2001 16:32:42)
		 * @return java.lang.Object
		 */
		public java.lang.Object getElement() {
			return element;
		}
		/**
		 * Returns checked state of the element.
		 * <p>
		 * Creation date: (12/25/2001 16:23:27)
		 * @return boolean
		 */
		public boolean isChecked() {
			return checked;
		}
		/**
		 * Sets the checked state of this element.
		 * <p>
		 * Creation date: (12/25/2001 16:23:27)
		 * @param newChecked boolean
		 */
		public void setChecked(boolean newChecked) {
			checked = newChecked;
		}
		/**
		 * Sets the element contained in this element.
		 * <p>
		 * Creation date: (12/25/2001 16:32:42)
		 * @param newElement java.lang.Object
		 */
		public void setElement(java.lang.Object newElement) {
			element = newElement;
		}
		/**
		 * Toggles the checked state of this element
		 *
		*/

		public final void toggleChecked() {
			checked = !checked;
		}
		public final String toString() {
			if (element != null) {
				return element.toString();
			} else {
				return "";
			}
		}
	}

/**
 * CheckListModel constructor comment.
 */
public CheckListModel() {
	super();

}
/**
 * Adds element to the model, inserting it at the location specified by index.
 * <p>
 * Creation date: (12/25/2001 16:38:26)
 * @param index int Index of the element
 * @param element java.lang.Object element to insert.
 */
public void add(int index, Object element) {
	super.add(index, convertToModel(element));
}
/**
 * Appends element to the end of the list.
 * <p>
 * Creation date: (12/25/2001 16:43:03)
 * @param element Object element to add
 */
public void addElement(Object element) {
	super.addElement(convertToModel(element));	
}
/**
 * Returns whether the element is in the model.
 * <p>
 * Creation date: (12/25/2001 16:44:17)
 * @return boolean
 * @param element java.lang.Object
 */
public boolean contains(Object element) {
	return super.contains(convertToModel(element));
}
/**
 * Internal method to unwrap the element stored in CheckListElement.
 * <p>
 * Creation date: (12/25/2001 16:34:57)
 * @return java.lang.Object
 * @param element com.cosylab.gui.components.CheckListElement
 */
protected Object convertFromModel(CheckListElement element) {
	return element.getElement();
}
/**
 * Internal method to wrap the element to the list.
 * <p>
 * Creation date: (12/25/2001 16:30:33)
 * @return com.cosylab.gui.components.CheckListElement
 * @param item java.lang.Object
 */
protected CheckListElement convertToModel(Object element) {
	return new CheckListElement(element);
}
/**
 * Copies the contents of this model into the array. Array must contain
 * sufficient space.
 * <p>
 * Creation date: (12/25/2001 16:45:22)
 * @param anArray java.lang.Object[]
 */
public void copyInto(Object[] anArray) {
	int n = size();
	for (int i = 0; i < n; i++)
		anArray[i] = get(i);
}
/**
 * Returns the element at the specified index.
 * <p>
 * Creation date: (12/25/2001 16:46:59)
 * @return java.lang.Object
 * @param index int
 */
public Object elementAt(int index) {
	return get(index);
}
/**
 * Returns first element of the list.
 * <p>
 * Creation date: (12/25/2001 16:47:48)
 * @return java.lang.Object
 */
public Object firstElement() {
	return convertFromModel((CheckListElement)super.firstElement());
}
/**
 * Returns the element at the specified position.
 * <p>
 * Creation date: (12/25/2001 16:48:36)
 * @return java.lang.Object
 * @param index int
 */
public Object get(int index) {
	return convertFromModel((CheckListElement)super.get(index));
}
/**
 * Returns element at the specified position.
 * <p>
 * Creation date: (12/25/2001 16:49:14)
 * @return java.lang.Object
 * @param index int
 */
public Object getElementAt(int index) {
	return get(index);
}
/**
 * Returns index of the object or -1 if the object is not in the list.
 * <p>
 * Creation date: (12/25/2001 16:58:29)
 * @return int
 * @param element java.lang.Object
 */
public int indexOf(Object element) {
	return super.indexOf(convertToModel(element));
}
/**
 * Returns index of first occurence of the object after the specified index.
 * <p>
 * Creation date: (12/25/2001 16:59:13)
 * @return int
 * @param element java.lang.Object
 * @param index int
 */
public int indexOf(Object element, int index) {
	return super.indexOf(convertToModel(element), index);
}
/**
 * Inserts element to the specified position in the list.
 * <p>
 * Creation date: (12/25/2001 16:59:53)
 * @param element java.lang.Object
 * @param index int
 */
public void insertElementAt(Object element, int index) {
	super.insertElementAt(convertToModel(element), index);
}
/**
 * Internal helper routine. Only performs type-cast on the internal element.
 * <p>
 * Creation date: (12/25/2001 18:20:16)
 */
protected CheckListElement internalGet(int index) {
	return (CheckListElement)(super.get(index));
}
/**
 * Returns true if the element at the specified index is checked.
 * <p>
 * Creation date: (12/25/2001 18:19:40)
 * @return boolean
 * @param index int
 */
public boolean isChecked(int index) {
	if ((index < 0) || (index >= size()))
		return false;
	return internalGet(index).isChecked();
}
/**
 * Returns the last element of the list.
 * <p>
 * Creation date: (12/25/2001 17:00:27)
 * @return java.lang.Object
 */
public Object lastElement() {
	return convertFromModel((CheckListElement)super.lastElement());
}
/**
 * Returns index of the last ocurrence of the object or -1 if there is no
 * such entry in the list.
 * <p>
 * Creation date: (12/25/2001 17:01:09)
 * @return int
 * @param element java.lang.Object
 */
public int lastIndexOf(Object element) {
	return super.lastIndexOf(convertToModel(element));
}
/**
 * Returns last occurence of the object in the list after specified index.
 * <p>
 * Creation date: (12/25/2001 17:01:50)
 * @return int
 * @param element java.lang.Object
 * @param index int
 */
public int lastIndexOf(Object element, int index) {
	return super.lastIndexOf(convertFromModel((CheckListElement)element), index);
}
/**
 * Removes element specified by index from the list.
 * <p>
 * Creation date: (12/25/2001 18:15:03)
 * @return java.lang.Object
 * @param index int
 */
public Object remove(int index) {
	return convertFromModel((CheckListElement)super.remove(index));
}
/**
 * Removes the element from the list. If the element is not in the
 * list, this method returns false.
 * <p>
 * Creation date: (12/25/2001 18:16:12)
 * @return boolean
 * @param element java.lang.Object
 */
public boolean removeElement(Object element) {
	return super.removeElement(convertToModel(element));
}
/**
 * Sets new value for the element at the specified index.
 * <p>
 * Creation date: (12/25/2001 18:17:13)
 * @return java.lang.Object
 * @param index int
 * @param element java.lang.Object
 */
public Object set(int index, Object element) {
	return convertFromModel((CheckListElement)super.set(index, convertToModel(element)));
}
/**
 * Sets the checked state of element at the specified index.
 * <p>
 * Creation date: (12/25/2001 18:21:42)
 * @param index int index of the element.
 * @param value boolean new checked state.
 */
public void setChecked(int index, boolean value) {
	CheckListElement cle = internalGet(index);
	cle.setChecked(value);
	fireContentsChanged(cle.getElement(), index, index);
}
/**
 * Returns contents of this list as an array.
 * <p>
 * Creation date: (12/25/2001 18:18:37)
 * @return java.lang.Object[]
 */
public Object[] toArray() {
	Object[] anArray = new Object[size()];
	copyInto(anArray);
	return anArray;
}
/**
 * Toggles the checked state of the element at specified index.
 * <p>
 * Creation date: (12/25/2001 18:21:17)
 * @param index int
 */
public void toggleChecked(int index) {
	CheckListElement cle = internalGet(index);
	cle.toggleChecked();
	fireContentsChanged(cle.getElement(), index, index);
}
}
