package org.daison;

import java.io.*;

public class DaisonException extends RuntimeException {
	private int rc;

	public DaisonException(int rc) {
		super(errName(rc));
		this.rc = rc;
	}

	public DaisonException(SerializationException e) {
		super(e);
		this.rc = 0;
	}

	public DaisonException(String err) {
		super(err);
		this.rc = 0;
	}

	private native static String errName(int rc);
}
