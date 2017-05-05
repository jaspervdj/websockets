#include <stdint.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

/* Taken from:
 *
 * <http://stackoverflow.com/questions/776508/best-practices-for-circular-shift-rotate-operations-in-c>
 *
 * Intel is little-endian, we have to rotate right
 */
static inline uint32_t rotr32(uint32_t n, unsigned int c) {
  const unsigned int mask = (CHAR_BIT*sizeof(n)-1);
  c &= mask;  // avoid undef behaviour with NDEBUG.  0 overhead for most types / compilers
  return (n>>c) | (n<<( (-c)&mask ));
}

/* - mask_shift is the initial shift of the mask.  It is specified in bytes.
 */
void _hs_mask_chunk(
        uint32_t mask, int mask_shift,
        uint8_t *payload_start, size_t payload_len,
        uint8_t *target) {
    const uint8_t *payload_end = payload_start + payload_len;

    uint8_t *p = payload_start;

#if defined(__x86_64__)
    uint64_t mask64;
    /* Set up 64 byte mask. */
    mask64 = (uint64_t)(rotr32(mask, 8 * (mask_shift % 4)));
    mask64 |= (mask64 << 32);
    /* Take the fast road. */
    while (p < payload_end - 7) {
        *(uint64_t *)target = *(uint64_t*)p ^ mask64;
        p += 8;target += 8;
    }
#elif defined(__i386__)
    /* Set up 32 byte mask. */
    uint32_t mask32;
    mask32 = (uint32_t)(rotr32(mask, 8 * (mask_shift % 4)));

    /* Take the fast road. */
    while (p < payload_end - 3) {
        *(uint32_t *)target = *(uint32_t*)p ^ mask32;
        p += 4;target += 4;
    }
#endif

    /* This is the slow path which also handles the un-aligned suffix. */
    while (p != payload_end) {
        *target = *p ^ (uint8_t)(((uint8_t *)&mask)[mask_shift % 4]);
        p++;target++;
        mask_shift++;
    }
}
