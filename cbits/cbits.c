#include <stdint.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

/* Taken from:
 *
 * <http://stackoverflow.com/questions/776508/best-practices-for-circular-shift-rotate-operations-in-c>
 */
static inline uint32_t rotl32(uint32_t n, unsigned int c) {
    const unsigned int mask = (CHAR_BIT*sizeof(n)-1);
    /* avoid undef behaviour with NDEBUG.  0 overhead for most types and
     * compilers */
    c &= mask;
    return (n<<c) | (n>>( (-c)&mask ));
}

/* - mask_shift is the initial shift of the mask.  This value must be 0, 8, 16
 *   or 24.
 *
 * The new mask_shift is returned from this function.
 */
int _hs_mask_chunk(
        uint32_t mask, int mask_shift,
        uint8_t *payload_start, size_t payload_len) {
    const uint8_t *payload_end = payload_start + payload_len;

    uint32_t mask32;
    uint64_t mask64;

    uint8_t *p = payload_start;

#if defined(__x86_64__)
    /* Un-aligned prefix. */
    while (p != payload_end && ((uintptr_t)p & 0x7)) {
        *p ^= (uint8_t)(mask >> mask_shift);
        p++;
        mask_shift = mask_shift == 24 ? 0 : (mask_shift + 8);
    }

    /* Set up 64 byte mask. */
    mask64 = (uint64_t)(rotl32(mask, mask_shift));
    mask64 |= (mask64 << 32);

    /* Take the fast road. */
    while (p < payload_end - 7) {
        uint64_t *p64 = (uint64_t*)p;
        *p64 ^= mask64;
        p += 8;
    }

#elif defined(__i386__)
    /* Un-aligned prefix. */
    while (p != payload_end && ((uintptr_t)p & 0x3)) {
        *p ^= (uint8_t)(mask >> mask_shift);
        p++;
        mask_shift = mask_shift == 24 ? 0 : (mask_shift + 8);
    }

    /* Set up 32 byte mask. */
    mask32 = (uint64_t)(rotl32(mask, mask_shift));
    mask32 |= (mask32 << 32);

    /* Take the fast road. */
    while (p < payload_end - 3) {
        uint64_t *p32 = (uint64_t*)p;
        *p32 ^= mask32;
        p += 4;
    }

#endif

    /* This is the slow path which also handles the un-aligned suffix. */
    while (p != payload_end) {
        *p ^= (uint8_t)(mask >> mask_shift);
        p++;
        mask_shift = mask_shift == 24 ? 0 : (mask_shift + 8);
    }

    return mask_shift;
}
