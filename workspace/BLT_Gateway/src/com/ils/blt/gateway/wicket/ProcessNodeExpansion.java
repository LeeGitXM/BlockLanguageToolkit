package com.ils.blt.gateway.wicket;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.UUID;

import org.apache.wicket.MetaDataKey;
import org.apache.wicket.Session;

import com.ils.blt.gateway.engine.ProcessNode;

/**
 * Custom expansion state:
 * <ul>
 * <li>expanded ProcessNode models are identified by their UUID</li>
 * <li>efficient expansion of all ProcessNode</li>
 * <li>state is stored in the session</li>
 * </ul>
 * 
 * @author svenmeier
 */

public class ProcessNodeExpansion implements Set<ProcessNode>, Serializable {
	private static final long serialVersionUID = -1705071642949427231L;
	private static MetaDataKey<ProcessNodeExpansion> KEY = new MetaDataKey<ProcessNodeExpansion>() {
		private static final long serialVersionUID = -2564082437478673765L;
	};
	private Set<UUID> ids = new HashSet<UUID>();
	private boolean inverse = false;
	
	public void collapseAll(){
		ids.clear();
		inverse = false;
	}
	
	public void expandAll() {
		ids.clear();
		inverse = true;
	}

	@Override
	public boolean add(ProcessNode foo)
	{
		if (inverse) {
			return ids.remove(foo.getSelf());
		}
		else
		{
			return ids.add(foo.getSelf());
		}
	}

	@Override
	public boolean remove(Object o)
	{
		ProcessNode foo = (ProcessNode)o;

		if (inverse)
		{
			return ids.add(foo.getSelf());
		}
		else
		{
			return ids.remove(foo.getSelf());
		}
	}

	@Override
	public boolean contains(Object o)
	{
		ProcessNode foo = (ProcessNode)o;

		if (inverse) {
			return !ids.contains(foo.getSelf());
		}
		else {
			return ids.contains(foo.getSelf());
		}
	}

	@Override
	public void clear() {
		throw new UnsupportedOperationException();
	}

	@Override
	public int size() {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean isEmpty() {
		throw new UnsupportedOperationException();
	}

	@Override
	public <A> A[] toArray(A[] a) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Iterator<ProcessNode> iterator() {
		throw new UnsupportedOperationException();
	}

	@Override
	public Object[] toArray() {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean addAll(Collection<? extends ProcessNode> c) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Get the expansion for the session.
	 * 
	 * @return expansion
	 */
	 public static ProcessNodeExpansion get() {
		 ProcessNodeExpansion expansion = Session.get().getMetaData(KEY);
		 if (expansion == null) {
			 expansion = new ProcessNodeExpansion();
			 Session.get().setMetaData(KEY, expansion);
		 }
		 return expansion;
	 }
}
