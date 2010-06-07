  0:  LDC    6,3(0)                     * first avail. addr
  1:  LDC    4,0(0)                     * 
  2:  ST     4,0(6)                     * first stack frame
  3:  ST     6,2(6)                     * 
  4:  LDA    5,3(6)                     * start stack
  5:  LDC    3,1(0)                     * addr of cmd-args
  6:  LD     2,0(3)                     * command-line arg
  7:  ST     2,3(6)                     * 
  8:  LD     2,1(3)                     * command-line arg
  9:  ST     2,4(6)                     * 
 10:  LDA    4,2(7)                     * compute return addr
 11:  ST     4,1(6)                     * store it
 12:  LDC    7,16(0)                    * jump to main
 13:  LD     2,0(6)                     * get return value
 14:  OUT    2,0,0                      * print it
 15:  HALT   0,0,0                      * 
 16:  LD     2,3(6)                     * a
 17:  LDA    5,1(5)                     * push-temp: increment sp
 18:  ST     2,0(5)                     * store at sp
 19:  LD     2,4(6)                     * b
 20:  LDA    5,1(5)                     * push-temp: increment sp
 21:  ST     2,0(5)                     * store at sp
 22:  LD     1,0(5)                     * pop-temp-lr: load right
 23:  LD     0,-1(5)                    * pop-temp-lr: load left
 24:  LDA    5,-2(5)                    * decrement sp by 2
 25:  DIV    2,0,1                      * /
 26:  LDA    5,1(5)                     * push-temp: increment sp
 27:  ST     2,0(5)                     * store at sp
 28:  LD     2,0(5)                     * pop-temp: load
 29:  LDA    5,-1(5)                    * decrement sp
 30:  ST     2,0(6)                     * store return-value
 31:  LDA    5,0(6)                     * point sp to it
 32:  LD     4,1(6)                     * fetch return address
 33:  LD     6,2(6)                     * restore caller's fp
 34:  LDA    7,0(4)                     * jump to return addr
