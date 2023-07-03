    ;; Lê numeros do pino de entrada até que um destes seja negativo

    .define n 100

try_again:                      ; do {

    in n                        ; n = input();
    lda n

    bpl try_again               ; } while (n >= 0);
    bze try_again

    out n                       ; write(n)

    brk
