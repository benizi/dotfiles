/* Based on https://stackoverflow.com/a/9500692/82723
 *
 * But, the only parts that remain are the calls to OpenSSL library functions:
 * - OpenSSL_add_all_algorithms
 * - EVP_get_cipherbyname
 * - EVP_get_digestbyname
 * - EVP_BytesToKey
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <openssl/evp.h>

typedef unsigned char uc;
typedef const char *ccp;

#define EXIT_BAD_PASS 1
#define EXIT_BAD_SALT 2
#define EXIT_SSL_FAIL 3

#define SALT_LENGTH 8
#define IV_LENGTH 8

static void hex(ccp name, const uc *s, int n) {
  if (!getenv("RAW"))
    printf("%-3s=", name);
  for (int i = 0; i < n; i++)
    printf("%02X", s[i]);
  printf("\n");
}

int main(void) {
  uc key[EVP_MAX_KEY_LENGTH], iv[EVP_MAX_IV_LENGTH], salt[SALT_LENGTH];
  ccp cipher_name = getenv("CIPHER");
  ccp digest_name = getenv("DIGEST");
  ccp salt_hex = getenv("SALT");
  ccp password = getenv("PASSWORD");
  if (!cipher_name)
    cipher_name = "blowfish";
  if (!digest_name)
    digest_name = "md5";
  if (!password || !strlen(password))
    return EXIT_BAD_PASS;
  if (salt_hex && strlen(salt_hex) < 2 * SALT_LENGTH)
    return EXIT_BAD_SALT;
  if (salt_hex)
    for (int i = 0; i < SALT_LENGTH; i++)
      sscanf(salt_hex + 2 * i, "%2hhx", salt + i);
  OpenSSL_add_all_algorithms();
  const EVP_CIPHER *ciph = EVP_get_cipherbyname(cipher_name);
  const EVP_MD *dgst = EVP_get_digestbyname(digest_name);
  int keylen = EVP_BytesToKey(ciph, dgst, salt_hex ? salt : NULL,
                              (uc *)password, strlen(password), 1, key, iv);
  if (!keylen)
    return EXIT_SSL_FAIL;
  if (salt_hex)
    hex("salt", salt, SALT_LENGTH);
  hex("key", key, keylen);
  hex("iv", iv, IV_LENGTH);
  return EXIT_SUCCESS;
}
