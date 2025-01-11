/* cryptographic functions

// aes

void mmmfreeaescipherctx(EVP_CIPHER_CTX *ctx);
EVP_CIPHER_CTX *mmmnewaesenc(const unsigned char key[16]);
EVP_CIPHER_CTX *mmmnewaesdec(const unsigned char key[16]);

// rsa

struct mmmrsaout {
  // always heap-allocated, never stack-allocated. Always
  // to be freed using free()
  unsigned char *out;
  size_t outlen;
};

void mmmfreersaout(struct mmmrsaout *out);
void mmmfreersakey(EVP_PKEY *key);
EVP_PKEY *mmmnewrsa(int bits);
int mmmrsapubenc(EVP_PKEY *key, const unsigned char *in, size_t inlen, struct
mmmrsaout *out);
int mmmrsaprivdec(EVP_PKEY *key, const unsigned char *in,
size_t inlen, struct mmmrsaout *out);
int mmmwritepubkey(EVP_PKEY *key, struct mmmrsaout *out);

// hash

unsigned char *mmmhashservnam(const unsigned char *serverid, size_t serveridlen,
const unsigned char *sharedsecret, size_t sharedsecretlen, const unsigned char
*verifytoken, size_t verifytokenlen);

// error & lifecycle

char *mmmdumpallerrs();
void mmmopensslfree(void *ptr);

*/

#include <openssl/bio.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <openssl/rsa.h>
#include <openssl/x509.h>
#include <string.h>

#define INTTRY(x)                                                              \
  if ((x) <= 0)                                                                \
  goto err

// aes

static void mmmfreeaescipher(EVP_CIPHER **ctx) { EVP_CIPHER_free(*ctx); }

void mmmfreeaescipherctx(EVP_CIPHER_CTX *ctx) { EVP_CIPHER_CTX_free(ctx); }

static void mmmfreeaescipherctx1(EVP_CIPHER_CTX **ctx) {
  EVP_CIPHER_CTX_free(*ctx);
}

// Minecraft uses AES-128-CFB8 with IV = key. free using mmmfreeaescipherctx
static EVP_CIPHER_CTX *mmmnewaes0(
    int (*new)(EVP_CIPHER_CTX *, const EVP_CIPHER *, const unsigned char *,
               const unsigned char *, const OSSL_PARAM[]),
    const unsigned char key[16]) {

  if (!key)
    return NULL;

  EVP_CIPHER *cipher __attribute__((cleanup(mmmfreeaescipher))) = NULL;
  EVP_CIPHER_CTX *ctx __attribute__((cleanup(mmmfreeaescipherctx1))) = NULL;

  if (!(cipher = EVP_CIPHER_fetch(NULL, "AES-128-CFB8", NULL)))
    goto err;
  if (!(ctx = EVP_CIPHER_CTX_new()))
    goto err;
  INTTRY(new (ctx, cipher, key, key, NULL));

  EVP_CIPHER_CTX *ret = ctx;
  ctx = NULL; // prevent cleanup
  return ret;

err:
  return NULL;
}

EVP_CIPHER_CTX *mmmnewaesenc(const unsigned char key[16]) {
  return mmmnewaes0(EVP_EncryptInit_ex2, key);
}

EVP_CIPHER_CTX *mmmnewaesdec(const unsigned char key[16]) {
  return mmmnewaes0(EVP_DecryptInit_ex2, key);
}

// rsa

void mmmfreersakey(EVP_PKEY *key) { EVP_PKEY_free(key); }

static void mmmfreersakey0(EVP_PKEY **key) { EVP_PKEY_free(*key); }

static void mmmfreersactx0(EVP_PKEY_CTX **ctx) { EVP_PKEY_CTX_free(*ctx); }

// generate RSA key. free using mmmfreersakey
EVP_PKEY *mmmnewrsa(int bits) {
  EVP_PKEY_CTX *ctx __attribute__((cleanup(mmmfreersactx0))) = NULL;
  EVP_PKEY *key __attribute__((cleanup(mmmfreersakey0))) = NULL;
  EVP_PKEY *keyret = NULL;
  if (bits < 1024)
    goto err;
  ctx = EVP_PKEY_CTX_new_id(EVP_PKEY_RSA, NULL);
  if (!ctx)
    goto err;
  INTTRY(EVP_PKEY_keygen_init(ctx));
  INTTRY(EVP_PKEY_CTX_set_rsa_keygen_bits(ctx, bits));
  INTTRY(EVP_PKEY_generate(ctx, &key));
  keyret = key, key = NULL;
err:
  return keyret;
}

struct mmmrsaout {
  // use regular free(), not OPENSSL_free() or anything else
  unsigned char *out;
  size_t outlen;
};

// helper for freeing RSA output
void mmmfreersaout(struct mmmrsaout *out) { free(out->out); }

static int mmmdorsacrypt(int (*initcrypt)(EVP_PKEY_CTX *),
                         int (*docrypt)(EVP_PKEY_CTX *, unsigned char *,
                                        size_t *, const unsigned char *,
                                        size_t),
                         EVP_PKEY *key, const unsigned char *in, size_t inlen,
                         struct mmmrsaout *out) {
  EVP_PKEY_CTX *ctx __attribute__((cleanup(mmmfreersactx0))) = NULL;
  if (!key || !in || inlen > INT_MAX || !out)
    return 0;
  ctx = EVP_PKEY_CTX_new_from_pkey(NULL, key, NULL);
  if (!ctx)
    goto err;
  INTTRY(initcrypt(ctx));
  // measure length
  size_t outlen;
  INTTRY(docrypt(ctx, NULL, &outlen, in, inlen));
  // encrypt/decrypt
  unsigned char *outbuf = malloc(outlen);
  if (!outbuf)
    return 0;
  if (docrypt(ctx, outbuf, &outlen, in, inlen) <= 0) {
    free(outbuf);
    return 0;
  }
  out->out = outbuf;
  out->outlen = outlen;
  return 1;
err:
  return 0;
}

// use free() to free the buffer in out->out
int mmmrsapubenc(EVP_PKEY *key, const unsigned char *in, size_t inlen,
                 struct mmmrsaout *out) {
  return mmmdorsacrypt(EVP_PKEY_encrypt_init, EVP_PKEY_encrypt, key, in, inlen,
                       out);
}

// use free() to free the buffer in out->out
int mmmrsaprivdec(EVP_PKEY *key, const unsigned char *in, size_t inlen,
                  struct mmmrsaout *out) {
  return mmmdorsacrypt(EVP_PKEY_decrypt_init, EVP_PKEY_decrypt, key, in, inlen,
                       out);
}

// ASN.1/DER as Minecraft uses

// RSA public key is stored as SubjectPublicKeyInfo
int mmmwritepubkey(EVP_PKEY *key, struct mmmrsaout *out) {
  BIO *bio = NULL;
  if (!key || !out)
    return 0;

  if (!(bio = BIO_new(BIO_s_mem())))
    goto err;

  INTTRY(i2d_PUBKEY_bio(bio, key));

  BUF_MEM *buf;
  if (BIO_get_mem_ptr(bio, &buf) <= 0 || !buf || !buf->data)
    goto err;

  unsigned char *outbuf = malloc(buf->length);
  if (!outbuf)
    goto err;

  memcpy(outbuf, buf->data, buf->length);
  out->out = outbuf;
  out->outlen = buf->length;
  BIO_free(bio);
  return 1;

err:
  if (bio)
    BIO_free(bio);
  return 0;
}

// OPENSSL_free wrapper

void mmmopensslfree(void *ptr) { OPENSSL_free(ptr); }

// error

// dump all OpenSSL errors to a (potentially multi-line) C string
//
// return NULL on failure
//
// memory must be freed using free(). Do not use OPENSSL_free()
char *mmmdumpallerrs() {
  BIO *bio = NULL;
  char *ibuf = NULL, *obuf = NULL;
  long len;

  // create memory BIO
  if (!(bio = BIO_new(BIO_s_mem())))
    return NULL;

  // write errors to BIO (infallible)
  ERR_print_errors(bio);

  // get data length and buffer
  len = BIO_get_mem_data(bio, &ibuf);
  if (len <= 0 || !ibuf)
    goto cleanup;

  // allocate output buffer (+1 for null terminator)
  if ((unsigned long long)len > SIZE_MAX - 1 ||
      !(obuf = calloc((size_t)len + 1, 1)))
    goto cleanup;

  // copy data
  memcpy(obuf, ibuf, (size_t)len);

cleanup:
  if (bio)
    BIO_free(bio);
  return obuf;
}

#define UC_ARR(name) const unsigned char *name, size_t name##len

static void mmmfreemdctx(EVP_MD_CTX **ctx) {
  if (*ctx)
    EVP_MD_CTX_free(*ctx);
}

static void mmmfreemd(EVP_MD **md) {
  if (*md)
    EVP_MD_free(*md);
}

// hash server name. error if NULL returned. free using free()
unsigned char *mmmhashservnam(UC_ARR(serverid), UC_ARR(sharedsecret),
                              UC_ARR(verifytoken)) {
  if (!serverid || !sharedsecret || !verifytoken)
    return NULL;

  EVP_MD_CTX *ctx __attribute__((cleanup(mmmfreemdctx))) = NULL;
  EVP_MD *typ __attribute__((cleanup(mmmfreemd))) = NULL;
  unsigned char *hash = NULL;

  if (!(ctx = EVP_MD_CTX_new()))
    goto err;
  if (!(typ = EVP_MD_fetch(NULL, "SHA1", NULL)))
    goto err;
  if (EVP_MD_get_size(typ) != 20)
    goto err;

  hash = malloc(20);
  if (!hash)
    goto err;

  INTTRY(EVP_DigestInit_ex2(ctx, typ, NULL));
  INTTRY(EVP_DigestUpdate(ctx, serverid, serveridlen));
  INTTRY(EVP_DigestUpdate(ctx, sharedsecret, sharedsecretlen));
  INTTRY(EVP_DigestUpdate(ctx, verifytoken, verifytokenlen));
  INTTRY(EVP_DigestFinal_ex(ctx, hash, NULL));

  return hash;

err:
  free(hash);
  return NULL;
}
