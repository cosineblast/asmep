
    ;;  TODO: Document this
    ;;
    .define n 0xc0

    .define result 0xc1

    .define zero 254
    .at byte zero
    .byte 0 0

    .define one 255
    .at word one
    .byte 0 1

    .at byte 0

    nop     ; My CPU implementation does not work
            ; if the first instruction is not a NOP :P

    in n                        ; n = input()

    lda zero
    sta result                  ; result = 0

    lda n

loop:
    bze end_loop                ; while (n != 0) {

    add result
    sta result                  ; result += n

    lda n
    sub one
    sta n                       ; n = n - 1

    jmp loop                    ; }
end_loop:

    out result                  ; output(result)

.at byte 0x20
    brk
