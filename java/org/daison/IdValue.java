package org.daison;

public class IdValue<A> {
	private long id;
	private A value;

	public IdValue(long id, A value) {
		this.id    = id;
		this.value = value;
	}
	
	public long getId() { return id;    };
	public A getValue() { return value; };
}
