package org.daison;

public class IdKey<B> {
	private long id;
	private B key;

	public IdKey(long id, B key) {
		this.id    = id;
		this.key   = key;
	}
	
	public long getId() { return id;    };
	public B getKey()   { return key;   };
}
