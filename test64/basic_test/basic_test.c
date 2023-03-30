#include <inttypes.h>

int main()
{
    volatile int a = 1024;
    volatile int b = 2048;
    volatile uint64_t c = a * b;

    *(volatile int*)(0x00002010) = 1;

    while(1);
}
