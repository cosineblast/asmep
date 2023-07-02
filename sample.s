
    ; Hello, fellow students

    ;; This file intends to demonstrate the compiler's abilities.
    ;; It contains a program that receives a number N and computes
    ;; the sum of the first N positive integers.

    ;; As one may see, comments are defined with ; and ;; is
    ;; recommended for lines that begin with comments.

    .define n 200
    .define result 201

    .define zero 254
    .at zero
    .byte 0

    .define one 255
    .at one
    .byte 1

    .at 0

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

    brk
