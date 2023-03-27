
.global _start
_start:
    li      x1, 8
    li      x4, 1
    li      x2, 0x1000
    sw      x1, 0(x2)

loop:
    lw      x1, 0(x2)
    add     x4, x4, 1
    add     x1, x1, 1
    li      x3, 15
    sw      x1, 0(x2)
    blt     x1, x3, loop

    li      x1, 0

    li      x2, 0x2010
    sw      x2, 0(x2)

end:
    j end