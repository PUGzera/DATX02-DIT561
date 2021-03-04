package org.daison;

import java.util.*;

public class Table<A> {
	private final String name;
	private final Class<A> rowClass;
	private final List<Index<A,Object>> indices;

	public Table(String name, Class<A> rowClass) {
		this.name = name;
		this.rowClass = rowClass;
		this.indices = new ArrayList<Index<A,Object>>();
	}

	public String getName() {
		return name;
	}

	public Class<A> getRowClass() {
		return rowClass;
	}

	public <B> void addIndex(Index<A,B> index) {
		indices.add((Index<A,Object>) index);
	}
}
