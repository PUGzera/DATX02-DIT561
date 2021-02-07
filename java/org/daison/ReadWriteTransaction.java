package org.daison;

import java.io.*;

public class ReadWriteTransaction extends ReadTransaction {
	ReadWriteTransaction(Database db) {
		super(db);
	}
}
