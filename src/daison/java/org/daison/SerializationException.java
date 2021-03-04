package org.daison;

public class SerializationException extends Exception {
	public SerializationException(String msg) {
		super(msg);
	}

	public SerializationException(Exception e) {
		super(e);
	}
}
