package org.daison;

import java.io.*;
import java.util.*;
import java.nio.*;

public class Database implements Closeable {
	private long ref;
	private long cookie;
	private Map<String, SchemaEntry> schema;

	public Database(File file) throws DaisonException {
		ref = openFile(file.getPath());
		initSchema();
	}

	public Database(Object assetManager, String path) throws DaisonException {
		ref = openAsset(assetManager, path);
		initSchema();
	}

	private native long openFile(String filePath) throws DaisonException;
	private native long openAsset(Object assetManager, String path) throws DaisonException;
	private native void fetchSchema() throws DaisonException;
	public native void close() throws DaisonException;

	private void initSchema() {
		cookie = -1;
		schema = new HashMap<String,SchemaEntry>();

		openTransaction(0);

		try {
			fetchSchema();
		} finally {
			commitTransaction();
		}
	}

	private void registerTable(long id, long bufRef, int size) throws SerializationException {
		DataStream stream = new DataStream(bufRef, size);
		stream.getConstructor();
		String name = stream.getString();
		int tnum    = stream.getInt();
		schema.put(name, new SchemaEntry(id, tnum));
	}

	private static class SchemaEntry {
		public final long id;
		public final int tnum;
		
		public SchemaEntry(long id, int tnum) {
			this.id   = id;
			this.tnum = tnum;
		}
	}
	
	public ReadTransaction newReadTransaction() throws DaisonException {
		openTransaction(0);
		return new ReadTransaction(this);
	}

	public ReadWriteTransaction newReadWriteTransaction() throws DaisonException {
		openTransaction(1);
		return new ReadWriteTransaction(this);
	}

	protected native void openTransaction(int wr) throws DaisonException;
	protected native void commitTransaction() throws DaisonException;
	protected native void rollbackTransaction() throws DaisonException;

	protected long openCursor(String name, int mode, int n, int x) throws DaisonException {
		SchemaEntry entry = schema.get(name);
		if (entry == null)
			throw new DaisonException("Table "+name+" does not exist");
		return openCursor(entry.tnum,mode,n,x);
	}

	protected native long openCursor(int tnum, int mode, int n, int x) throws DaisonException;
	protected native int cursorMoveTo(long cursorRef, long bufRef, long bufSize);
	protected native int getKeySize(long cursorRef);
	protected native long getKey(long cursorRef, int offset, int amt);
	protected native int getDataSize(long cursorRef);
	protected native long getData(long cursorRef, int offset, int amt);
	protected native void closeCursor(long cursorRef);

	static { 
         System.loadLibrary("jdaison");
    }
}
