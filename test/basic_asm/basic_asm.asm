
.global _start
_start:
    auipc   x1, 0x2
    addi    x1, x1, -4
    li      x2, 10
    sw      x2, 0(x1)
    lw      x3, 0(x1)

end:
    j end