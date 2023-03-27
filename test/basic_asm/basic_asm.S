
.global _start
_start:
    li      x1, 5
    nop
    add     x2, x1, 1
    add     x1, x2, x1

    li      x2, 0x2010
    sw      x2, 0(x2)

end:
    j end