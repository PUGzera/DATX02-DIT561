package org.daison;

import java.io.*;
import java.util.*;

public interface CloseableIterable<A> extends Iterable<A> {
	public CloseableIterator<A> iterator();
}
