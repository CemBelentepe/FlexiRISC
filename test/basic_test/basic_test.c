#include <inttypes.h>

int main()
{
    volatile int a = 1 << 18;
    volatile int b = 10;
    volatile uint64_t c = a * b;

    *(volatile int*)(0x00002010) = 1;

    while(1);
}
