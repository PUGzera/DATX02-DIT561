#ifndef __MINGW32__
#include <alloca.h>
#else
#include <malloc.h>
#endif
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <jni.h>
#include "sqlite3Btree.h"

#define l2p(x) ((void*) (intptr_t) (x))
#define p2l(x) ((jlong) (intptr_t) (x))

#if defined(__STDC_ISO_10646__) && WCHAR_MAX >= 0x10FFFF
#include <wchar.h>
#define GU_UCS_WCHAR
typedef wchar_t GuUCS;
#else
typedef int32_t GuUCS;
#endif

static GuUCS
gu_utf8_decode(const uint8_t** src_inout)
{
	const uint8_t* src = *src_inout;
	uint8_t c = src[0];
	if (c < 0x80) {
		*src_inout = src + 1;
		return (GuUCS) c;
	}
	size_t len = (c < 0xe0 ? 1 :
	              c < 0xf0 ? 2 :
	              c < 0xf8 ? 3 :
	              c < 0xfc ? 4 :
	                         5
	             );
	uint64_t mask = 0x0103070F1f7f;
	uint32_t u = c & (mask >> (len * 8));
	for (size_t i = 1; i <= len; i++) {
		c = src[i];
		u = u << 6 | (c & 0x3f);
	}
	*src_inout = &src[len + 1];
	return (GuUCS) u;
}

static jstring
c2j_string(JNIEnv *env, const char* s) {
	const char* utf8 = s;
	size_t len = strlen(s);

	jchar* utf16 = alloca(len*sizeof(jchar));
	jchar* dst   = utf16;
	while (s-utf8 < len) {
		GuUCS ucs = gu_utf8_decode((const uint8_t**) &s);

		if (ucs <= 0xFFFF) {
			*dst++ = ucs;
		} else {
			ucs -= 0x10000;
			*dst++ = 0xD800+((ucs >> 10) & 0x3FF);
			*dst++ = 0xDC00+(ucs & 0x3FF);
		}
	}

	return (*env)->NewString(env, utf16, dst-utf16);
}

static void*
get_ref(JNIEnv *env, jobject self) {
	jfieldID refId = (*env)->GetFieldID(env, (*env)->GetObjectClass(env, self), "ref", "J");
	return l2p((*env)->GetLongField(env, self, refId));
}

static void
throw_rc_exception(JNIEnv *env, int rc)
{
	jclass exception_class = (*env)->FindClass(env, "org/daison/DaisonException");
	if (!exception_class)
		return;
	jmethodID constrId = (*env)->GetMethodID(env, exception_class, "<init>", "(I)V");
	if (!constrId)
		return;
	jobject exception = (*env)->NewObject(env, exception_class, constrId, rc);
	if (!exception)
		return;
	(*env)->Throw(env, exception);
}

JNIEXPORT jlong JNICALL Java_org_daison_Database_openFile
  (JNIEnv *env, jobject self, jstring j_fpath)
{
	int rc;

	const char *fpath = (*env)->GetStringUTFChars(env, j_fpath, 0);

	Btree* pBtree = NULL;
	rc = sqlite3BtreeOpen(NULL, fpath, &pBtree, 0,
	                      SQLITE_OPEN_READWRITE | 
	                      SQLITE_OPEN_CREATE | 
	                      SQLITE_OPEN_MAIN_DB);

	(*env)->ReleaseStringUTFChars(env, j_fpath, fpath);

	if (rc != SQLITE_OK) {
		throw_rc_exception(env, rc);
		return 0;
	}

	return p2l(pBtree);
}

#ifdef __ANDROID__

#include <unistd.h>
#include <android/log.h>
#include <android/asset_manager.h>
#include <android/asset_manager_jni.h>

#define LOG_TAG "sqlite3Btree"
#define LOG_DEBUG(msg, args...) \
    __android_log_print(ANDROID_LOG_DEBUG, LOG_TAG, msg, ##args)
#define LOG_INFO(msg, args...) \
    __android_log_print(ANDROID_LOG_INFO, LOG_TAG, msg, ##args)
#define LOG_ERROR(msg, args...) \
    __android_log_print(ANDROID_LOG_ERROR, LOG_TAG, msg, ##args)
#define LOG_FATAL(msg, args...) \
    __android_log_print(ANDROID_LOG_FATAL, LOG_TAG, msg, ##args)

typedef struct {
	sqlite3_io_methods const *pMethods;
	int fd;
	sqlite3_int64 offset;
	sqlite3_int64 length;
} assetFile;

static int assetRead(sqlite3_file *file, void *buf, int iAmt, sqlite3_int64 iOfst) {
    assetFile *f = (assetFile *) file;
    int expectReadLen = (iAmt + iOfst > f->length) ? (f->length - iOfst) : iAmt;
    int readLen = pread64(f->fd, buf, expectReadLen, iOfst + f->offset);
    if (readLen < 0) {
        return SQLITE_IOERR_READ;
    } else if (readLen == expectReadLen) {
        return SQLITE_OK;
    } else {
        memset((__uint8_t *) buf + readLen, 0, iAmt - readLen);
        return SQLITE_IOERR_SHORT_READ;
    }
}

static int assetClose(sqlite3_file *vfs_file) {
    assetFile *file = (assetFile *) vfs_file;
    close(file->fd);
    file->fd = 0;
    return SQLITE_OK;
}

static int assetWrite(sqlite3_file *vfs_file, const void *foo, int iAmt, sqlite3_int64 iOfst) {
    return SQLITE_READONLY;
}


static int assetTruncate(sqlite3_file *vfs_file, sqlite3_int64 size) {
    return SQLITE_READONLY;
}


static int assetSync(sqlite3_file *vfs_file, int flags) {
    /* Does nothing. */
    return SQLITE_OK;
}


static int assetFileSize(sqlite3_file *vfs_file, sqlite3_int64 *pSize) {
    assetFile *file = (assetFile *)vfs_file;
    *pSize = file->length;
    return SQLITE_OK;
}


static int assetLock(sqlite3_file *vfs_file, int mode) {
    /* Does nothing. */
    return SQLITE_OK;
}


static int assetUnlock(sqlite3_file *vfs_file, int mode) {
    /* Does nothing. */
    return SQLITE_OK;
}


static int assetCheckReservedLock(sqlite3_file *vfs_file, int *pResOut) {
    *pResOut = 0;
    return SQLITE_OK;
}


static int assetFileControl(sqlite3_file *vfs_file, int op, void *pArg) {
    return SQLITE_NOTFOUND;
}


static int assetSectorSize(sqlite3_file *vfs_file) {
    /* Not applicable, so just return a "minimum write size" of 0 bytes. */
    return 0;
}


static int assetDeviceCharacteristics(sqlite3_file *vfs_file) {
    /* Not applicable, so return an empty set of characteristics. */
    return 0;
}


static const sqlite3_io_methods androidIoMethods = {
   1,                          /* iVersion */
   assetClose,                 /* xClose */
   assetRead,                  /* xRead */
   assetWrite,                 /* xWrite */
   assetTruncate,              /* xTruncate */
   assetSync,                  /* xSync */
   assetFileSize,              /* xFileSize */
   assetLock,                  /* xLock */
   assetUnlock,                /* xUnlock */
   assetCheckReservedLock,     /* xCheckReservedLock */
   assetFileControl,           /* xFileControl */
   assetSectorSize,            /* xSectorSize */
   assetDeviceCharacteristics, /* xDeviceCapabilities */
};

static int assetOpen(
  sqlite3_vfs *pVfs,           /* The VFS for which this is the xOpen method */
  const char *zPath,           /* Pathname of file to be opened */
  sqlite3_file *pFile,         /* The file descriptor to be filled in */
  int flags,                   /* Input flags to control the opening */
  int *pOutFlags               /* Output flags returned to SQLite core */
){
    assetFile *f = (assetFile *) pFile;
    f->pMethods = &androidIoMethods;
    if (3 > sscanf(zPath, "%x_%llx_%llx", &f->fd, &f->offset, &f->length)) {
        return SQLITE_ERROR;
    }
    *pOutFlags = flags;
    return SQLITE_OK;
}

static int assetDelete(sqlite3_vfs *vfs, const char *zName, int syncDir) {
    return SQLITE_ERROR;            // Assets cannot be deleted.
}

static int assetAccess(sqlite3_vfs *vfs, const char *zName, int flags, int *pResOut) {
    *pResOut = 0;
    return SQLITE_OK;
}

static int assetFullPathname(sqlite3_vfs *vfs, const char *zName, int nOut, char *zOut) {
    strncpy(zOut, zName, nOut);
    return SQLITE_OK;
}

static sqlite3_vfs android_asset_vfs = {
    2,                    /* iVersion */
    sizeof(assetFile),    /* szOsFile */
    512,                  /* mxPathname */
    0,                    /* pNext */
    "android-asset",      /* zName */
    NULL,                 /* pAppData */
    assetOpen,            /* xOpen */
    assetDelete,          /* xDelete */
    assetAccess,          /* xAccess */
    assetFullPathname,    /* xFullPathname */
    };

JNIEXPORT jlong JNICALL Java_org_daison_Database_openAsset
  (JNIEnv *env, jobject self, jobject j_assetManager, jstring j_fpath)
{
	AAssetManager *assetManager =
		AAssetManager_fromJava(env, j_assetManager);

	const char *fpath = (*env)->GetStringUTFChars(env, j_fpath, 0);
	AAsset *asset = AAssetManager_open(assetManager,
	                                   fpath, AASSET_MODE_RANDOM);
	(*env)->ReleaseStringUTFChars(env, j_fpath, fpath);

	if (asset == NULL)
		return 0;

	off64_t offset, length;
	int fd = AAsset_openFileDescriptor64(asset, &offset, &length);

	AAsset_close(asset);

	if (fd < 0)
		return 0;

	char asset_id[64];
	snprintf(asset_id, sizeof(asset_id),
	        "%x_%llx_%llx", fd, offset, length);

	sqlite3_vfs_register(&android_asset_vfs, 0);

	Btree* pBtree = NULL;
	int rc = sqlite3BtreeOpen("android-asset", asset_id, &pBtree, 0,
	                          SQLITE_OPEN_READWRITE |
	                          SQLITE_OPEN_CREATE |
	                          SQLITE_OPEN_MAIN_DB);
	if (rc != SQLITE_OK) {
		throw_rc_exception(env, rc);
		return 0;
	}

	return p2l(pBtree);
}
#endif

JNIEXPORT jstring JNICALL Java_org_daison_DaisonException_errName
  (JNIEnv *env, jclass cls, jint rc)
{
	return c2j_string(env, sqlite3BtreeErrName(rc));
}

JNIEXPORT void JNICALL Java_org_daison_Database_fetchSchema
  (JNIEnv *env, jobject self)
{
	int rc;
	Btree* pBtree = get_ref(env, self);
	sqlite3BtreeLockTable(pBtree,1,0);

	u32 cookie;
    sqlite3BtreeGetMeta(pBtree, 1, &cookie);

    jclass self_class = (*env)->GetObjectClass(env, self);

   	jfieldID cookieId = (*env)->GetFieldID(env, self_class, "cookie", "J");
	u32 old_cookie = (u32) (*env)->GetLongField(env, self, cookieId);

	if (cookie == old_cookie)
		return;

	jmethodID registerTableId = (*env)->GetMethodID(env, self_class, "registerTable", "(JJI)V");

   	jfieldID schemaId = (*env)->GetFieldID(env, self_class, "schema", "Ljava/util/Map;");
	jobject schema = (*env)->GetObjectField(env, self, schemaId);
	jmethodID clearId = (*env)->GetMethodID(env, (*env)->GetObjectClass(env, schema), "clear", "()V");
	(*env)->CallVoidMethod(env, schema, clearId);

	BtCursor* pCursor;
	rc = sqlite3BtreeCursor(pBtree, 1, 0, 0, 0, &pCursor);
	if (rc != SQLITE_OK) {
		throw_rc_exception(env, rc);
		return;
	}

	int res;
	rc = sqlite3BtreeFirst(pCursor, &res);
	if (rc != SQLITE_OK) {
		sqlite3BtreeCloseCursor(pCursor);
		throw_rc_exception(env, rc);
		return;
	}

	while (res == 0) {
		i64 key;
		rc = sqlite3BtreeKeySize(pCursor, &key);
		if (rc != SQLITE_OK) {
			sqlite3BtreeCloseCursor(pCursor);
			throw_rc_exception(env, rc);
			return;
		}

		u32 size;
        rc = sqlite3BtreeDataSize(pCursor, &size);
		if (rc != SQLITE_OK) {
			sqlite3BtreeCloseCursor(pCursor);
			throw_rc_exception(env, rc);
			return;
		}

        const void *ptr = 
			sqlite3BtreeDataFetch(pCursor, &size);

		(*env)->CallVoidMethod(env, self, registerTableId, key, p2l(ptr), size);
		if ((*env)->ExceptionCheck(env)) {
			sqlite3BtreeCloseCursor(pCursor);
			return;
		}

		rc = sqlite3BtreeNext(pCursor, &res);
		if (rc != SQLITE_OK) {
			sqlite3BtreeCloseCursor(pCursor);
			throw_rc_exception(env, rc);
			return;
		}
	}

	rc = sqlite3BtreeCloseCursor(pCursor);
	if (rc != SQLITE_OK) {
		throw_rc_exception(env, rc);
		return;
	}
}

JNIEXPORT void JNICALL Java_org_daison_Database_close
  (JNIEnv *env, jobject self)
{
	Btree* btree = get_ref(env, self);
	if (btree != NULL)
		sqlite3BtreeClose(btree);
}

JNIEXPORT void JNICALL Java_org_daison_Database_openTransaction
  (JNIEnv *env, jobject self, jint wr)
{
	int rc;
	Btree* btree = get_ref(env, self);	
	rc = sqlite3BtreeBeginTrans(btree, wr);
	if (rc != SQLITE_OK)
		throw_rc_exception(env,rc);
}

JNIEXPORT void JNICALL Java_org_daison_Database_commitTransaction
  (JNIEnv *env, jobject self)
{
	int rc;
	Btree* btree = get_ref(env, self);
	rc = sqlite3BtreeCommit(btree);
	if (rc != SQLITE_OK)
		throw_rc_exception(env,rc);
}

JNIEXPORT void JNICALL Java_org_daison_Database_rollbackTransaction
  (JNIEnv *env, jobject self)
{
	int rc;
	Btree* btree = get_ref(env, self);	
	rc = sqlite3BtreeRollback(btree, SQLITE_ABORT_ROLLBACK, 0);
	if (rc != SQLITE_OK)
		throw_rc_exception(env,rc);
}


JNIEXPORT jlong JNICALL Java_org_daison_Database_openCursor
  (JNIEnv *env, jobject self, jint tnum, jint mode, jint n, jint x)
{
	int rc;
	Btree* btree = get_ref(env, self);

	rc = sqlite3BtreeLockTable(btree, tnum, mode);
	if (rc != SQLITE_OK) {
		throw_rc_exception(env, rc);
		return 0;
	}

	BtCursor* pCursor = NULL;
	rc = sqlite3BtreeCursor(btree, tnum, mode, n, x, &pCursor);
	if (rc != SQLITE_OK) {
		throw_rc_exception(env, rc);
		return 0;
	}

	return p2l(pCursor);
}

JNIEXPORT jint JNICALL Java_org_daison_Database_cursorMoveTo
  (JNIEnv *env, jobject self, jlong cursorRef, jlong bufRef, jlong bufSize)
{
	int rc;
	BtCursor* pCursor = (BtCursor*) l2p(cursorRef);

	int res;
	rc = sqlite3BtreeMoveTo(pCursor, l2p(bufRef), bufSize, 0, &res);
	if (rc != SQLITE_OK) {
		throw_rc_exception(env, rc);
		return 0;
	}

	return res;
}

JNIEXPORT jint JNICALL Java_org_daison_Database_getKeySize
  (JNIEnv *env, jobject self, jlong cursorRef)
{
	int rc;
	BtCursor* pCursor = (BtCursor*) l2p(cursorRef);

	i64 size;
	rc = sqlite3BtreeKeySize(pCursor, &size);
	if (rc != SQLITE_OK) {
		throw_rc_exception(env, rc);
		return 0;
	}

	return (int) size;
}

JNIEXPORT jlong JNICALL Java_org_daison_Database_getKey
  (JNIEnv *env, jobject self, jlong cursorRef, jint offset, jint amt)
{
	int rc;
	BtCursor* pCursor = (BtCursor*) l2p(cursorRef);

	void *buffer = malloc(amt);
	rc = sqlite3BtreeKey(pCursor, offset, amt, buffer);
	if (rc != SQLITE_OK) {
		free(buffer);
		throw_rc_exception(env, rc);
		return 0;
	}

	return p2l(buffer);
}

JNIEXPORT jint JNICALL Java_org_daison_Database_getDataSize
  (JNIEnv *env, jobject self, jlong cursorRef)
{
	int rc;
	BtCursor* pCursor = (BtCursor*) l2p(cursorRef);

	u32 size;
	rc = sqlite3BtreeDataSize(pCursor, &size);
	if (rc != SQLITE_OK) {
		throw_rc_exception(env, rc);
		return 0;
	}

	return size;
}

JNIEXPORT jlong JNICALL Java_org_daison_Database_getData
  (JNIEnv *env, jobject self, jlong cursorRef, jint offset, jint amt)
{
	int rc;
	BtCursor* pCursor = (BtCursor*) l2p(cursorRef);

	void *buffer = malloc(amt);
	rc = sqlite3BtreeData(pCursor, offset, amt, buffer);
	if (rc != SQLITE_OK) {
		free(buffer);
		throw_rc_exception(env, rc);
		return 0;
	}

	return p2l(buffer);
}

JNIEXPORT void JNICALL Java_org_daison_Database_closeCursor
  (JNIEnv *env, jobject self, jlong cursorRef)
{
	int rc;
	rc = sqlite3BtreeCloseCursor((BtCursor*) l2p(cursorRef));
	if (rc != SQLITE_OK)
		throw_rc_exception(env, rc);
}

JNIEXPORT jbyte JNICALL Java_org_daison_DataStream_peekByte
  (JNIEnv *env, jclass cls, jlong addr, jint offs)
{
	return *((unsigned char*) addr+offs);
}

JNIEXPORT jdouble JNICALL Java_org_daison_DataStream_peekDouble
  (JNIEnv *env, jclass cls, jlong addr, jint offs)
{
	return *((double*) addr+offs);
}

JNIEXPORT jfloat JNICALL Java_org_daison_DataStream_peekFloat
  (JNIEnv *env, jclass cls, jlong addr, jint offs)
{
	return *((float*) addr+offs);
}

JNIEXPORT jlong JNICALL Java_org_daison_DataStream_realloc
  (JNIEnv *env, jclass cls, jlong addr, jint size)
{
	return p2l(realloc(l2p(addr), size));
}

JNIEXPORT void JNICALL Java_org_daison_DataStream_pokeByte
  (JNIEnv *env, jclass cls, jlong addr, jint offs, jbyte b)
{
	*((unsigned char*) addr+offs) = b;
}

JNIEXPORT void JNICALL Java_org_daison_DataStream_pokeDouble
  (JNIEnv *env, jclass cls, jlong addr, jint offs, jdouble d)
{
	*((double*) addr+offs) = d;
}

JNIEXPORT void JNICALL Java_org_daison_DataStream_pokeFloat
  (JNIEnv *env, jclass cls, jlong addr, jint offs, jfloat f)
{
	*((float*) addr+offs) = f;
}

JNIEXPORT void JNICALL Java_org_daison_DataStream_free
  (JNIEnv *env, jclass clas, jlong addr)
{
	free(l2p(addr));
}
