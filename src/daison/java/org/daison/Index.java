package org.daison;

public class Index<A,B> {
	private final Table<A> table;
	private final String name;
	private final Class<B> keyClass;
	private final Getter getter;

	public Index(Table<A> table, String name, Class<B> keyClass, Getter<A,B> getter) {
		this.table = table;
		this.name  = table.getName()+"_"+name;
		this.keyClass = keyClass;
		this.getter = getter;
	}

	public Table<A> getTable() {
		return table;
	}

	public String getName() {
		return name;
	}
}
