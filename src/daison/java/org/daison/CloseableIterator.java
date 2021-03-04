package org.daison;

import java.io.*;
import java.util.*;

public interface CloseableIterator<A> extends Iterator<A>, Closeable {
}
