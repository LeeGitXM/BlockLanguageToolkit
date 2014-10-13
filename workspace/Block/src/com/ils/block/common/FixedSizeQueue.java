/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block.common;

import java.util.LinkedList;

public class FixedSizeQueue<E> extends LinkedList<E>  {
	private static final long serialVersionUID = 5843873110116467006L;
	private int bufferSize = 10;
	
	public FixedSizeQueue(int length) {
		this.bufferSize = length;
	}
	public void setBufferSize(int size) {
		bufferSize = size;
	}
	@Override
    public boolean add(E o) {
        super.add(o);
        while (size() > bufferSize) { super.remove(); }
        return true;
    }
}
