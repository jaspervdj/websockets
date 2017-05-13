#include <stdint.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

/* Taken from:
 *
 * <http://stackoverflow.com/questions/776508/best-practices-for-circular-shift-rotate-operations-in-c>
 */
static inline uint32_t rotr32(uint32_t n, unsigned int c) {
    const unsigned int mask = (CHAR_BIT*sizeof(n)-1);
    c &= mask;  /* avoid undef behaviour with NDEBUG.  0 overhead for most types / compilers */
    return (n>>c) | (n<<( (-c)&mask ));
}

/* - `mask` is the 4-byte mask to apply to the source.  It is stored in the
 *   hosts' native byte ordering.
 * - `mask_offset` is the initial offset in the mask.  It is specified in bytes
 *   and should be between 0 and 3 (inclusive).  This is necessary for when we
 *   are dealing with multiple chunks.
 * - `src` is the source pointer.
 * - `len` is the size of the source (and destination) in bytes.
 * - `dst` is the destination.
 */
void _hs_mask_chunk(
        uint32_t mask, int mask_offset,
        uint8_t *src, size_t len,
        uint8_t *dst) {
    const uint8_t *src_end = src + len;

    /* We have two fast paths: one for `x86_64` and one for `i386`
     * architectures.  In these fast paths, we mask 8 (or 4) bytes at a time.
     *
     * Note that we use unaligned loads and stores (allowed on these
     * architectures).  This makes the code much easier to write, since we don't
     * need to guarantee that `src` and `dst` have the same alignment.
     *
     * It only causes a minor slowdown, around 5% on my machine (TM).
     */
#if defined(__x86_64__)
    uint64_t mask64;
    /* Set up 64 byte mask. */
    mask64 = (uint64_t)(rotr32(mask, 8 * mask_offset));
    mask64 |= (mask64 << 32);
    /* Take the fast road. */
    while (src < src_end - 7) {
        *(uint64_t *)dst = *(uint64_t*)src ^ mask64;
        src += 8;
        dst += 8;
    }
#elif defined(__i386__)
    /* Set up 32 byte mask. */
    uint32_t mask32;
    mask32 = (uint32_t)(rotr32(mask, 8 * mask_offset));

    /* Take the fast road. */
    while (src < src_end - 3) {
        *(uint32_t *)dst = *(uint32_t*)src ^ mask32;
        src += 4;
        dst += 4;
    }
#endif

    /* This is the slow path which also handles the un-aligned suffix. */
    uint8_t *mask_ptr = (uint8_t *) &mask;
    while (src != src_end) {
        *dst = *src ^ *(mask_ptr + mask_offset);
        src++;
        dst++;
        mask_offset = (mask_offset + 1) & 0x3;
    }
}
