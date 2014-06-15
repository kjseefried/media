/*
 * This program exercises the PNG CRC algorithm for purposes of testing. It's basically
 * a copy and paste from the PNG specification:
 *
 *      http://www.w3.org/TR/PNG/#D-CRCAppendix
 */

#include <stdio.h>

/* Table of CRCs of all 8-bit messages. */
unsigned long crc_table[256];
   
/* Flag: has the table been computed? Initially false. */
int crc_table_computed = 0;
   
/* Make the table for a fast CRC. */
void make_crc_table(void)
{
    unsigned long c;
    int n, k;
    
    for (n = 0; n < 256; n++) {
        c = (unsigned long) n;
        for (k = 0; k < 8; k++) {
            if (c & 1)
                c = 0xedb88320L ^ (c >> 1);
            else
                c = c >> 1;
        }
        crc_table[n] = c;
    }
    crc_table_computed = 1;
}
  

/* Update a running CRC with the bytes buf[0..len-1]--the CRC should be initialized to all 1's,
 * and the transmitted value is the 1's complement of the final running CRC (see the crc()
 * routine below). */
   
unsigned long update_crc(unsigned long crc, unsigned char *buf, int len)
{
    unsigned long c = crc;
    int n;
   
    if (!crc_table_computed)
        make_crc_table();
    for (n = 0; n < len; n++) {
        c = crc_table[(c ^ buf[n]) & 0xff] ^ (c >> 8);
    }
    return c;
}
   
/* Return the CRC of the bytes buf[0..len-1]. */
unsigned long crc(unsigned char *buf, int len)
{
    return update_crc(0xffffffffL, buf, len) ^ 0xffffffffL;
}

/* ============ */
/* Main Program */
/* ============ */

int main( void )
{
    int i;

    unsigned long result;
    unsigned char buffer1[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    unsigned char buffer2[] = { 0xFF, 0xFE, 0xFD, 0xFC, 0xFB, 0xFA, 0xF9, 0xF8 };
    unsigned char buffer3[] = { };

    printf( "\nSome specific CRC values...\n" );
    printf( "buffer1 => %lu\n", crc( buffer1, 10 ) );
    printf( "buffer2 => %lu\n", crc( buffer2,  8 ) );
    printf( "buffer3 => %lu\n", crc( buffer3,  0 ) );

    printf( "\nThe CRC Table...\n" );
    for( i = 0; i < 256; ++i ) {
        printf( "crc_table[%3d] = %lu\n", i, crc_table[i] );
    }

    return 0;
}
