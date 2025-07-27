     *    = $0200
  xxxx    = $10
  yyyy    = $4000
  zzzz    = 255
  begin   LDA #$FF    ; load accum 
          LDA #255
          LDA #'A'
          LDA #%00000011
          LDA xxxx,X
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