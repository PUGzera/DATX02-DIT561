INSTALL_PATH = /usr/local

C_SOURCES = java/jdaison.c c/sqlite3Btree.c
JAVA_SOURCES = $(wildcard java/org/daison/*.java)

JNI_INCLUDES = $(if $(wildcard /usr/lib/jvm/default-java/include/.*), -I/usr/lib/jvm/default-java/include -I/usr/lib/jvm/default-java/include/linux, \
               $(if $(wildcard /usr/lib/jvm/java-1.11.0-openjdk-amd64/include/.*), -I/usr/lib/jvm/java-1.11.0-openjdk-amd64/include/ -I/usr/lib/jvm/java-1.11.0-openjdk-amd64/include/linux, \
               $(if $(wildcard /System/Library/Frameworks/JavaVM.framework/Versions/A/Headers/.*), -I/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers, \
               $(if $(wildcard /Library/Java/Home/include/.*), -I/Library/Java/Home/include/ -I/Library/Java/Home/include/darwin, \
               $(error No JNI headers found)))))

# For compilation on Windows replace the previous line with something like this:
#
# JNI_INCLUDES  = -I "C:/Program Files/Java/jdk1.8.0_171/include" -I "C:/Program Files/Java/jdk1.8.0_171/include/win32" -I "C:/MinGW/msys/1.0/local/include"
# WINDOWS_LDFLAGS = -L"C:/MinGW/msys/1.0/local/lib" -no-undefined

GCC = gcc
LIBTOOL = $(if $(shell command -v glibtool 2>/dev/null), glibtool, libtool)  --tag=CC

# For cross-compilation from Linux to Windows replace the previous two lines with:
#
# GCC = x86_64-w64-mingw32-gcc
# LIBTOOL = ../c/libtool
# WINDOWS_CCFLAGS = -I$(INSTALL_PATH)/include
# WINDOWS_LDFLAGS = -L$(INSTALL_PATH)/lib -no-undefined

java: libjdaison.la jdaison.jar

libjdaison.la: $(patsubst %.c, %.lo, $(C_SOURCES))
	$(LIBTOOL) --mode=link $(GCC) $(CFLAGS) -O2 -o libjdaison.la -shared $^ -rpath $(INSTALL_PATH)/lib $(WINDOWS_LDFLAGS)

%.lo : %.c
	$(LIBTOOL) --mode=compile $(GCC) $(CFLAGS) -O2 -c $(JNI_INCLUDES) -Ic $(WINDOWS_CCFLAGS) -std=c99 -shared $< -o $@

jdaison.jar: $(patsubst %.java, %.class, $(JAVA_SOURCES))
	jar -cvf $@ $(patsubst java/%, -C java '%', $(wildcard java/org/daison/*.class))

%.class : %.java
	javac $< -Xlint:deprecation -cp java -h java/

install: libjdaison.la jdaison.jar
	$(LIBTOOL) --mode=install install libjdaison.la $(INSTALL_PATH)/lib
	install jdaison.jar $(INSTALL_PATH)/lib


doc:
	javadoc org.grammaticalframework.pgf org.grammaticalframework.sg -d java-api

clean:
	rm -f *.lo
	rm -f *.la
	rm -f -r .libs
	rm -f *.jar
	rm -f java/org/daison/*.class
	rm -f java/org/daison/*.h
