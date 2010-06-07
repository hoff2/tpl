  0:  LDC    6,4(0)                     * first avail. addr
  1:  LDC    4,0(0)                     * 
  2:  ST     4,0(6)                     * first stack frame
  3:  ST     6,2(6)                     * 
  4:  LDA    5,2(6)                     * start stack
  5:  LDC    3,1(0)                     * addr of cmd-args
  6:  LD     2,0(3)                     * command-line arg
  7:  ST     2,3(6)                     * 
  8:  LD     2,1(3)                     * command-line arg
  9:  ST     2,4(6)                     * 
 10:  LD     2,2(3)                     * command-line arg
 11:  ST     2,5(6)                     * 
 12:  LDA    5,3(5)                     * update sp
 13:  LDA    4,2(7)                     * compute return addr
 14:  ST     4,1(6)                     * store it
 15:  LDC    7,19(0)                    * jump to main
 16:  LD     2,0(6)                     * get return value
 17:  OUT    2,0,0                      * print it
 18:  HALT   0,0,0                      * 
 19:  LD     2,3(6)                     * a
 20:  LDA    5,1(5)                     * push-temp: increment sp
 21:  ST     2,0(5)                     * store at sp
 22:  LD     2,4(6)                     * b
 23:  LDA    5,1(5)                     * push-temp: increment sp
 24:  ST     2,0(5)                     * store at sp
 25:  LD     1,0(5)                     * pop-temp-lr: load right
 26:  LD     0,-1(5)                    * pop-temp-lr: load left
 27:  LDA    5,-2(5)                    * decrement sp by 2
 28:  ADD    2,0,1                      * +
 29:  LDA    5,1(5)                     * push-temp: increment sp
 30:  ST     2,0(5)                     * store at sp
 31:  LD     2,5(6)                     * c
 32:  LDA    5,1(5)                     * push-temp: increment sp
 33:  ST     2,0(5)                     * store at sp
 34:  LD     1,0(5)                     * pop-temp-lr: load right
 35:  LD     0,-1(5)                    * pop-temp-lr: load left
 36:  LDA    5,-2(5)                    * decrement sp by 2
 37:  MUL    2,0,1                      * *
 38:  LDA    5,1(5)                     * push-temp: increment sp
 39:  ST     2,0(5)                     * store at sp
 40:  LD     2,0(5)                     * pop-temp: load
 41:  LDA    5,-1(5)                    * decrement sp
 42:  ST     2,0(6)                     * store return-value
 43:  LDA    5,0(6)                     * point sp to it
 44:  LD     4,1(6)                     * fetch return address
 45:  LD     6,2(6)                     * restore caller's fp
 46:  LDA    7,0(4)                     * jump to return addr
