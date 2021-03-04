package org.daison;

import java.io.*;
import java.util.*;

public class ReadTransaction implements Closeable {
	private Database db;
	private List<Closeable> closeables;

	ReadTransaction(Database db) {
		this.db = db;
		this.closeables = new ArrayList<Closeable>();
	}

	public <A> A at(Table<A> table, long id) {
		long cursorRef = db.openCursor(table.getName(), 0, 0, 0);
		DataStream stream = null;
		try {
			int res = db.cursorMoveTo(cursorRef, 0, id);
			if (res != 0)
				return null;
			int size = db.getDataSize(cursorRef);
			long ref = db.getData(cursorRef, 0, size);
			stream = new DataStream(ref,size);
			return stream.get(table.getRowClass());
		} catch (SerializationException e) {
			throw new DaisonException(e);
		} finally {
			db.closeCursor(cursorRef);
			if (stream != null)
				stream.close();
		}
	}

	public <A,B> CloseableIterable<Long> at(Index<A,B> index, B key) throws DaisonException {
		return new CloseableIterable<Long>() {
			public CloseableIterator<Long> iterator() {
				long cursorRef = db.openCursor(index.getName(), 0, 1, 1);

				DataStream keyStream = new DataStream();
				int  recordSize = 0;
				long recordRef  = 0;
				try {
					keyStream.put(key);
					int res = db.cursorMoveTo(cursorRef, keyStream.getBufRef(), keyStream.getBufPos());
					if (res == 0) {
						int size = db.getKeySize(cursorRef);
						recordSize = size-keyStream.getBufPos();
						recordRef = db.getKey(cursorRef, keyStream.getBufPos(), recordSize);
					}
				} catch (SerializationException e) {
					throw new DaisonException(e);
				} finally {
					keyStream.close();
					db.closeCursor(cursorRef);
				}

				final DataStream stream = new DataStream(recordRef, recordSize);
				CloseableIterator iter = new CloseableIterator<Long>() {
					public boolean hasNext() {
						return (stream.getRemainingBytes() > 0);
					};

					public Long next() {
						try {
							long id = stream.getRest(0,0);
							if (stream.getRemainingBytes() == 0)
								stream.close();
							return Long.valueOf(id);
						} catch (SerializationException e) {
							throw new DaisonException(e);
						}
					};
					
					public void close() {
						stream.close();
					}
				};
				closeables.add(iter);
				return iter;
			};
		};
	}

	public <A> CloseableIterable<IdValue<A>> from(Table<A> table) {
		return null;
	}

	public <A,B> CloseableIterable<IdKey<B>> from(Index<A,B> index) {
		return null;
	}

	public <A,B> CloseableIterable<IdValue<A>> atIndex(Index<A,B> index, B key) throws DaisonException {
		return new CloseableIterable<IdValue<A>>() {
			public CloseableIterator<IdValue<A>> iterator() {
				CloseableIterator<Long> iter = at(index,key).iterator();
				return new CloseableIterator<IdValue<A>>() {
					public boolean hasNext() {
						return iter.hasNext();
					};

					public IdValue<A> next() {
						long id = iter.next();
						A value = at(index.getTable(), id);
						return new IdValue(id,value);
					};
					
					public void close() throws IOException {
						iter.close();
					}
				};
			}
		};
	}

	public <A,B> CloseableIterable<IdValueKey<A,B>> fromIndex(Index<A,B> index) {
		return new CloseableIterable<IdValueKey<A,B>>() {
			public CloseableIterator<IdValueKey<A,B>> iterator() {
				CloseableIterator<IdKey<B>> iter = from(index).iterator();
				return new CloseableIterator<IdValueKey<A,B>>() {
					public boolean hasNext() {
						return iter.hasNext();
					};

					public IdValueKey<A,B> next() {
						IdKey res = iter.next();
						A value = at(index.getTable(), res.getId());
						return new IdValueKey(res.getId(),value,res.getKey());
					};

					public void close() throws IOException {
						iter.close();
					}
				};
			}
		};
	}

	public void close() {
		for (Closeable c : closeables) {
			try {
				c.close();
			} catch (IOException e) {
			}
		}
		db.commitTransaction();
	}
	
	public void rollback() {
		for (Closeable c : closeables) {
			try {
				c.close();
			} catch (IOException e) {
			}
		}
		db.rollbackTransaction();
	}
}
