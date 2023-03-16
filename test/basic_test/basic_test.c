

int main()
{
    volatile int a = 5;
    volatile int b = 10;
    volatile int c = a + b;

    *(volatile int*)(0x00002010) = 1;

    while(1);
}
