
    .define n 0xc0
    .define m 0xc0


    in n
    in m

    lda n

    sub m
    bpl next
    out n
    jmp end
next:
    out m
end:
    brk
