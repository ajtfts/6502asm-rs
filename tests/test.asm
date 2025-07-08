          .org $0200
  xxxx    .equ $10
  yyyy    .equ $4000
  zzzz    .equ 255
  begin   LDA #$FF    ; load accum 
          LDA #255
          LDA #'A'
          LDA #%00000011
          LDA byte2,X
          BNE begin
          BEQ debug
          STA $1500   ; save it
          LDA yyyy
          JMP begin
          SBC $44,X
          ROR $AAAA
          STA $4400,Y
clrlp     sta (xxxx),y
          iny
          dex
          bne clrlp
  debug   inx
          lda xxxx+1
          lda yyyy+10
          JMP ($5597)
  byte1   .db $0f,$44,15,$ee,80
  byte2   .db 'ABCD',%00001111
  area1   .ds 5
  byte2   .db $55
          .end