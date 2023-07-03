    ;; Lê dois numeros e escreve o menor deles

    .define n 100
    .define m 101

    in n                        ; n = input();
    in m                        ; m = input();

    lda n
    sub m
    bpl skip_assignment         ; if (n - m  <= 0) {

    lda n
    sta m                       ; m = n

skip_assignment:                 ; }

    out m                       ; write(m)

    brk
