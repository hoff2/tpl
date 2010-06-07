  0:  LDC    6,3(0)                     * first avail. addr
  1:  LDC    4,0(0)                     * 
  2:  ST     4,0(6)                     * first stack frame
  3:  ST     6,2(6)                     * 
  4:  LDA    5,2(6)                     * start stack
  5:  LDC    3,1(0)                     * addr of cmd-args
  6:  LD     2,0(3)                     * command-line arg
  7:  ST     2,3(6)                     * 
  8:  LD     2,1(3)                     * command-line arg
  9:  ST     2,4(6)                     * 
 10:  LDA    5,2(5)                     * update sp
 11:  LDA    4,2(7)                     * compute return addr
 12:  ST     4,1(6)                     * store it
 13:  LDC    7,17(0)                    * jump to main
 14:  LD     2,0(6)                     * get return value
 15:  OUT    2,0,0                      * print it
 16:  HALT   0,0,0                      * 
 17:  LD     2,3(6)                     * a
 18:  LDA    5,1(5)                     * push-temp: increment sp
 19:  ST     2,0(5)                     * store at sp
 20:  LD     2,4(6)                     * b
 21:  LDA    5,1(5)                     * push-temp: increment sp
 22:  ST     2,0(5)                     * store at sp
 23:  LD     1,0(5)                     * pop-temp-lr: load right
 24:  LD     0,-1(5)                    * pop-temp-lr: load left
 25:  LDA    5,-2(5)                    * decrement sp by 2
 26:  ADD    2,0,1                      * +
 27:  LDA    5,1(5)                     * push-temp: increment sp
 28:  ST     2,0(5)                     * store at sp
 29:  LD     2,0(5)                     * pop-temp: load
 30:  LDA    5,-1(5)                    * decrement sp
 31:  ST     2,0(6)                     * store return-value
 32:  LDA    5,0(6)                     * point sp to it
 33:  LD     4,1(6)                     * fetch return address
 34:  LD     6,2(6)                     * restore caller's fp
 35:  LDA    7,0(4)                     * jump to return addr
