package org.daison;

import java.io.*;
import java.lang.reflect.*;

class DataStream implements Closeable {
	private static enum Tag {
		List(0b00, 2, "a list"),
		Str(0b10, 2, "a string"),
		Int(0b01, 2, "an int"),
		Constructor(0b011, 3, "a constructor"),
		End(0b00111, 5, "an args-end"),
		Float(0b01111, 5, "a float"),
		Double(0b10111, 5, "a double"),
		Rational(0b11111, 5, "a rational"),
		Char(0b100111, 6, "a char"),
		IntList(0b101111, 6, "an integer list"),
		FltList(0b110111, 6, "a list of floats"),
		DblList(0b111111, 6, "a list of doubles");

		public final int code;
		public final int bits;
		public final String msg;

		private Tag(int code, int bits, String msg) {
			this.code = code;
			this.bits = bits;
			this.msg  = msg;
		}
	}
	
	private long bufRef;
	private int pos;
	private int size;
	
	private static final int DELTA = 256;
	private static final int SIZE_OF_FLOAT = 4;
	private static final int SIZE_OF_DOUBLE = 8;

	public DataStream(long bufRef, int size) {
		this.bufRef = bufRef;
		this.pos    = 0;
		this.size   = size;
	}

	public DataStream() {
		this.bufRef = 0;
		this.pos    = 0;
		this.size   = 0;
	}

	private void getTag(Tag tag) throws SerializationException {
		long w = getRawByte() & 0xFF;
		if ((w & ((1 << tag.bits) - 1)) == tag.code)
			return;

		throw new SerializationException("failed to find "+tag.msg);
	}

	private void putTag(Tag tag) throws SerializationException {
		putRawByte((byte) (tag.code & 0xFF));
	}

	private long getVInt(Tag tag) throws SerializationException {
		long w = getRawByte() & 0xFF;
		if ((w & ((1 << tag.bits) - 1)) == tag.code) {
			long n = w >> (tag.bits+1);
			if ((w & (1 << tag.bits)) == 0) {
				if ((w & 0x80) != 0)
					return (n | ((-1) << (7-tag.bits)));
				else
					return n;
			}
			else
				return getRest(7-tag.bits, n);
		} else {
			throw new SerializationException("failed to find "+tag.msg);
		}
	}

	protected long getRest(int bits, long n) throws SerializationException {
		long w = 0;
		do {
			w = getRawByte() & 0xFF;
			n = n | ((w >> 1) << bits);
			bits += 7;
		} while ((w & 1) != 0);
		
		if ((w & 0x80) != 0)
			return (n | ((-128) << bits));
		else
			return n;
	}

	private void putVInt(Tag tag, long n) throws SerializationException {
		int rbits = 7-tag.bits;
		long n0   = (n & ((1 << rbits) - 1)) << (tag.bits+1);
        long n1   = n >> rbits;
		if ((n1 == 0 && ((n0 & 0x80) == 0)) || (n1 == -1 && ((n0 & 0x80) != 0))) {
			putRawByte((byte) (n0 | tag.code));
		} else {
			putRawByte((byte) (n0 | (1 << tag.bits) | tag.code));
			putRest(n1);
		}
	}

	private void putRest(long n) throws SerializationException {
		for (;;) {
			long n0 = (long) ((n & ((1 << 7) - 1)) << 1);
			long n1 = n >> 7;
			if ((n1 == 0 && ((n0 & 0x80) == 0)) || (n1 == -1 && ((n0 & 0x80) != 0))) {
				putRawByte((byte) n0);
				break;
			} else {
				putRawByte((byte) (n0 | 1));
				n = n1;
			}
		}
	}

	public byte getByte() throws SerializationException {
		return (byte) getVInt(Tag.Int);
	}

	public void putByte(byte v) throws SerializationException {
		putVInt(Tag.Int, v);
	}

	public short getShort() throws SerializationException {
		return (short) getVInt(Tag.Int);
	}

	public void putShort(short v) throws SerializationException {
		putVInt(Tag.Int, v);
	}

	public int getInt() throws SerializationException {
		return (int) getVInt(Tag.Int);
	}

	public void putInt(int v) throws SerializationException {
		putVInt(Tag.Int, v);
	}

	public long getLong() throws SerializationException {
		return getVInt(Tag.Int);
	}

	public void putLong(long v) throws SerializationException {
		putVInt(Tag.Int, v);
	}

	public double getDouble() throws SerializationException {
		getTag(Tag.Double);
		
		if (size - pos < SIZE_OF_DOUBLE)
			throw new SerializationException("Buffer underflow");

		return peekDouble(bufRef, pos += SIZE_OF_DOUBLE);
	}

	public void putDouble(double d) throws SerializationException {
		putTag(Tag.Double);

		if (size - pos < SIZE_OF_DOUBLE) {
			bufRef = realloc(bufRef,size += DELTA);
		}
		pokeDouble(bufRef, pos += SIZE_OF_DOUBLE, d);
	}

	public float getFloat() throws SerializationException {
		getTag(Tag.Float);

		if (size - pos < SIZE_OF_FLOAT)
			throw new SerializationException("Buffer underflow");

		return peekFloat(bufRef, pos += SIZE_OF_FLOAT);
	}

	public void putFloat(float f) throws SerializationException {
		putTag(Tag.Float);

		if (size - pos < SIZE_OF_FLOAT) {
			bufRef = realloc(bufRef,size += DELTA);
		}
		pokeFloat(bufRef, pos += SIZE_OF_FLOAT, f);
	}

	public String getString() throws SerializationException {
		long len = getVInt(Tag.Str);
		char[] chars = new char[(int) len*2];

		int i = 0;
		for (int count = 0; count < len; count++) {
			int ucs;
			int c = getRawByte() & 0xFF;
			if (c < 0x80) {
				ucs = c;
			} else {
				int num = (c < 0xe0 ? 1 :
				           c < 0xf0 ? 2 :
				           c < 0xf8 ? 3 :
				           c < 0xfc ? 4 :
				           5
				          );
				long mask = 0x0103070F1f7fL;
				ucs = c & ((int) (mask >> (num * 8)));
				for (int k = 1; k <= num; k++) {
					c = getRawByte() & 0xFF;
					ucs = ucs << 6 | (c & 0x3f);
				}
			}
			if (ucs <= 0xFFFF) {
				chars[i++] = (char) ucs;
			} else {
				ucs -= 0x10000;
				chars[i++] = (char) (0xD800+((ucs >> 10) & 0x3FF));
				chars[i++] = (char) (0xDC00+(ucs & 0x3FF));
			}
		}

		return new String(chars,0,i);
	}

	public void putString(String s) throws SerializationException {
		putVInt(Tag.Str, s.codePointCount(0,s.length()));
		try {
			for (byte b : s.getBytes("UTF-8")) {
				putRawByte(b);
			}
		} catch (UnsupportedEncodingException e) {
			throw new SerializationException(e);
		}
	}

	public int getConstructor() throws SerializationException {
		return (int) getVInt(Tag.Constructor);
	}

	public void putConstructor(int v) throws SerializationException {
		putVInt(Tag.Constructor, v);
	}

	public void getEnd() throws SerializationException {
		getVInt(Tag.End);
	}

	private void setField(Object o, Field field) throws IllegalAccessException, SerializationException {
		Class fieldCls = field.getType();
		DaisonMaybeField mb = field.getAnnotation(DaisonMaybeField.class);

		if (mb != null) {
			int j = getConstructor();
			if (j == 1) {
				field.set(o, null);
			} else if (j == 2) {
				field.set(o, get(fieldCls));
			} else {
				throw new SerializationException("Constructor tag "+j+" out of bounds for DaisonMaybeField");
			}
			getEnd();
		} else if (fieldCls.isAssignableFrom(Byte.class) || fieldCls.isAssignableFrom(byte.class)) {
			field.setByte(o, getByte());
		} else if (fieldCls.isAssignableFrom(Short.class) || fieldCls.isAssignableFrom(short.class)) {
			field.setShort(o, getShort());
		} else if (fieldCls.isAssignableFrom(Integer.class) || fieldCls.isAssignableFrom(int.class)) {
			field.setInt(o, getInt());
		} else if (fieldCls.isAssignableFrom(Long.class) || fieldCls.isAssignableFrom(long.class)) {
			field.setLong(o, getLong());
		} else if (fieldCls.isAssignableFrom(Double.class) || fieldCls.isAssignableFrom(double.class)) {
			field.setDouble(o, getDouble());
		} else if (fieldCls.isAssignableFrom(Float.class) || fieldCls.isAssignableFrom(float.class)) {
			field.setFloat(o, getFloat());
		} else if (fieldCls.isAssignableFrom(String.class)) {
			field.set(o, getString());
		} else if (fieldCls.isArray()) {
			field.set(o, getArray(fieldCls.getComponentType()));
		} else if (fieldCls.isEnum()) {
			field.set(o, getEnum(fieldCls));
		} else {
			field.set(o, getObject(fieldCls));
		}
	}

	private void setAllFields(Object o, Class cls) throws IllegalAccessException, SerializationException {
		if (cls == null)
			return;

		setAllFields(o, cls.getSuperclass());

		for (Field field : cls.getDeclaredFields()) {
			if ((field.getModifiers() & (Modifier.TRANSIENT | Modifier.STATIC)) == 0) {
				setField(o, field);
			}
		}
	}

	private void setFields(Object o, String[] fields) throws IllegalAccessException, SerializationException, NoSuchFieldException {
		for (String fieldName : fields) {
			setField(o, o.getClass().getDeclaredField(fieldName));
		}
	}

	public <A> A getObject(Class<A> cls) throws SerializationException {
		Class con = null;

		int i = getConstructor();
		DaisonDataType dataType = cls.getAnnotation(DaisonDataType.class);
		if (dataType == null) {
			if (i != 1)
				throw new SerializationException("Constructor tag "+i+" out of bounds for class "+cls.getName());
			con = cls;
		} else {
			Class[] cons = dataType.constructors();
			if (i <= 0 || i > cons.length)
				throw new SerializationException("Constructor tag "+i+" out of bounds for class "+cls.getName());
			con = cons[i];
		}

		DaisonDataFields dataFields = (DaisonDataFields) con.getAnnotation(DaisonDataFields.class);

		try {
			Object o = con.newInstance();
			if (dataFields == null)
				setAllFields(o,con);
			else
				setFields(o,dataFields.fields());
			getEnd();
			return (A) o;
		} catch (InstantiationException e) {
			throw new SerializationException(e);
		} catch (IllegalAccessException e) {
			throw new SerializationException(e);
		} catch (NoSuchFieldException e) {
			throw new SerializationException(e);
		}
	}

	public void putObject(Object o) throws SerializationException {
	}

	public <A> Object getArray(Class<A> cls) throws SerializationException {
		int len = (int) getVInt(Tag.List);
		Object a = Array.newInstance(cls, len);
		for (int i = 0; i < len; i++) {
			Array.set(a, i, get(cls));
		}
		return a;
	}

	public <A> A getEnum(Class<A> cls) throws SerializationException {
		int i = getConstructor();
		getEnd();
		return cls.getEnumConstants()[i-1];
	}

	public <A> A get(Class<A> cls) throws SerializationException {
		if (cls.isAssignableFrom(Byte.class) || cls.isAssignableFrom(byte.class)) {
			return (A) Byte.valueOf(getByte());
		} else if (cls.isAssignableFrom(Short.class) || cls.isAssignableFrom(short.class)) {
			return (A) Short.valueOf(getShort());
		} else if (cls.isAssignableFrom(Integer.class) || cls.isAssignableFrom(int.class)) {
			return (A) Integer.valueOf(getInt());
		} else if (cls.isAssignableFrom(Long.class) || cls.isAssignableFrom(long.class)) {
			return (A) Long.valueOf(getLong());
		} else if (cls.isAssignableFrom(Double.class) || cls.isAssignableFrom(double.class)) {
			return (A) Double.valueOf(getDouble());
		} else if (cls.isAssignableFrom(Float.class) || cls.isAssignableFrom(float.class)) {
			return (A) Float.valueOf(getFloat());
		} else if (cls.isAssignableFrom(String.class)) {
			return (A) getString();
		} else if (cls.isArray()) {
			return (A) getArray(cls.getComponentType());
		} else if (cls.isEnum()) {
			return (A) getEnum(cls);
		} else {
			return (A) getObject(cls);
		}
	}

	public <A> void put(A value) throws SerializationException {
		if (value instanceof Byte) {
			putVInt(Tag.Int, ((Byte) value).byteValue());
		} if (value instanceof Short) {
			putVInt(Tag.Int, ((Short) value).shortValue());
		} if (value instanceof Integer) {
			putVInt(Tag.Int, ((Integer) value).intValue());
		} if (value instanceof Long) {
			putVInt(Tag.Int, ((Long) value).longValue());
		} if (value instanceof Float) {
			putFloat(((Float) value).floatValue());
		} if (value instanceof Double) {
			putDouble(((Double) value).doubleValue());
		} if (value instanceof String) {
			putString((String) value);
		} else if (value.getClass().isArray()) {
		} else if (value.getClass().isEnum()) {
		} else {
			putObject(value);
		}
	}

	public void close() {
		free(bufRef);
		this.bufRef = 0;
		this.pos    = 0;
		this.size   = 0;
	}

	private byte getRawByte() throws SerializationException {
		if (size - pos < 1)
			throw new SerializationException("Buffer underflow");
		return peekByte(bufRef, pos++);
	}

	private void putRawByte(byte b) throws SerializationException {
		if (size - pos < 1) {
			bufRef = realloc(bufRef,size += DELTA);
		}
		pokeByte(bufRef, pos++, b);
	}

	private static native byte peekByte(long addr, int offset);
	private static native double peekDouble(long addr, int offset);
	private static native float peekFloat(long addr, int offset);

	private static native long realloc(long addr, int size);

	private static native void pokeByte(long addr, int offset, byte b);
	private static native void pokeDouble(long addr, int offset, double d);
	private static native void pokeFloat(long addr, int offset, float f);
	
	private static native void free(long addr);
	
	public long getBufRef() { return bufRef; };
	public int  getBufPos() { return pos;    };
	public int  getRemainingBytes() { return (size-pos); };
}
