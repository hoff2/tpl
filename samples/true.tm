  0:  LDC    6,1(0)                     * first avail. addr
  1:  LDC    4,0(0)                     * 
  2:  ST     4,0(6)                     * first stack frame
  3:  ST     6,2(6)                     * 
  4:  LDA    5,3(6)                     * start stack
  5:  LDC    3,1(0)                     * addr of cmd-args
  6:  LDA    4,2(7)                     * compute return addr
  7:  ST     4,1(6)                     * store it
  8:  LDC    7,12(0)                    * jump to main
  9:  LD     2,0(6)                     * get return value
 10:  OUT    2,0,0                      * print it
 11:  HALT   0,0,0                      * 
 12:  LDC    2,1(0)                     * literal true
 13:  LDA    5,1(5)                     * push-temp: increment sp
 14:  ST     2,0(5)                     * store at sp
 15:  LD     2,0(5)                     * pop-temp: load
 16:  LDA    5,-1(5)                    * decrement sp
 17:  ST     2,0(6)                     * store return-value
 18:  LDA    5,0(6)                     * point sp to it
 19:  LD     4,1(6)                     * fetch return address
 20:  LD     6,2(6)                     * restore caller's fp
 21:  LDA    7,0(4)                     * jump to return addr
