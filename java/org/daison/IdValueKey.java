package org.daison;

public class IdValueKey<A,B> {
	private long id;
	private A value;
	private B key;

	public IdValueKey(long id, A value, B key) {
		this.id    = id;
		this.value = value;
		this.key   = key;
	}
	
	public long getId() { return id;    };
	public A getValue() { return value; };
	public B getKey()   { return key;   };
}
