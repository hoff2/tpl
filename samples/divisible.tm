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
 13:  LDC    7,72(0)                    * jump to main
 14:  LD     2,0(6)                     * get return value
 15:  OUT    2,0,0                      * print it
 16:  HALT   0,0,0                      * 
 17:  LD     2,3(6)                     * x
 18:  LDA    5,1(5)                     * push-temp: increment sp
 19:  ST     2,0(5)                     * store at sp
 20:  LDC    2,0(0)                     * int literal
 21:  LDA    5,1(5)                     * push-temp: increment sp
 22:  ST     2,0(5)                     * store at sp
 23:  LD     1,0(5)                     * pop-temp-lr: load right
 24:  LD     0,-1(5)                    * pop-temp-lr: load left
 25:  LDA    5,-2(5)                    * decrement sp by 2
 26:  SUB    0,0,1                      * =: subtract right from left
 27:  JEQ    0,2(7)                     * result = 0 if left = right
 28:  LDC    2,0(0)                     * false
 29:  LDA    7,1(7)                     * jump over
 30:  LDC    2,1(0)                     * false
 31:  LDA    5,1(5)                     * push-temp: increment sp
 32:  ST     2,0(5)                     * store at sp
 33:  LD     2,0(5)                     * pop-temp: load
 34:  LDA    5,-1(5)                    * decrement sp
 35:  ST     2,0(6)                     * store return-value
 36:  LDA    5,0(6)                     * point sp to it
 37:  LD     4,1(6)                     * fetch return address
 38:  LD     6,2(6)                     * restore caller's fp
 39:  LDA    7,0(4)                     * jump to return addr
 40:  LD     2,3(6)                     * a
 41:  LDA    5,1(5)                     * push-temp: increment sp
 42:  ST     2,0(5)                     * store at sp
 43:  LD     2,4(6)                     * b
 44:  LDA    5,1(5)                     * push-temp: increment sp
 45:  ST     2,0(5)                     * store at sp
 46:  LD     1,0(5)                     * pop-temp-lr: load right
 47:  LD     0,-1(5)                    * pop-temp-lr: load left
 48:  LDA    5,-2(5)                    * decrement sp by 2
 49:  DIV    2,0,1                      * /
 50:  LDA    5,1(5)                     * push-temp: increment sp
 51:  ST     2,0(5)                     * store at sp
 52:  LDC    2,2(0)                     * int literal
 53:  LDA    5,1(5)                     * push-temp: increment sp
 54:  ST     2,0(5)                     * store at sp
 55:  LD     1,0(5)                     * pop-temp-lr: load right
 56:  LD     0,-1(5)                    * pop-temp-lr: load left
 57:  LDA    5,-2(5)                    * decrement sp by 2
 58:  SUB    0,0,1                      * =: subtract right from left
 59:  JEQ    0,2(7)                     * result = 0 if left = right
 60:  LDC    2,0(0)                     * false
 61:  LDA    7,1(7)                     * jump over
 62:  LDC    2,1(0)                     * false
 63:  LDA    5,1(5)                     * push-temp: increment sp
 64:  ST     2,0(5)                     * store at sp
 65:  LD     2,0(5)                     * pop-temp: load
 66:  LDA    5,-1(5)                    * decrement sp
 67:  ST     2,0(6)                     * store return-value
 68:  LDA    5,0(6)                     * point sp to it
 69:  LD     4,1(6)                     * fetch return address
 70:  LD     6,2(6)                     * restore caller's fp
 71:  LDA    7,0(4)                     * jump to return addr
 72:  LDA    3,1(5)                     * call: compute new fp
 73:  LDC    2,0(0)                     * setup new stack frame
 74:  ST     2,0(3)                     * return value
 75:  ST     6,2(3)                     * save current fp
 76:  LDA    5,2(3)                     * set sp to push args
 77:  LD     2,4(6)                     * b
 78:  LDA    5,1(5)                     * push-temp: increment sp
 79:  ST     2,0(5)                     * store at sp
 80:  LDA    6,0(3)                     * set fp
 81:  LDA    4,2(7)                     * compute return address
 82:  ST     4,1(6)                     * store it
 83:  LDC    7,17(0)                    * isZero
 84:  LD     0,0(5)                     * pop-temp: load
 85:  LDA    5,-1(5)                    * decrement sp
 86:  JEQ    0,2(7)                     * or: if left = 0 jump to r
 87:  LDC    2,1(0)                     * result = true
 88:  LDA    7,21(7)                    * short-circuit
 89:  LDA    3,1(5)                     * call: compute new fp
 90:  LDC    2,0(0)                     * setup new stack frame
 91:  ST     2,0(3)                     * return value
 92:  ST     6,2(3)                     * save current fp
 93:  LDA    5,2(3)                     * set sp to push args
 94:  LD     2,3(6)                     * a
 95:  LDA    5,1(5)                     * push-temp: increment sp
 96:  ST     2,0(5)                     * store at sp
 97:  LD     2,4(6)                     * b
 98:  LDA    5,1(5)                     * push-temp: increment sp
 99:  ST     2,0(5)                     * store at sp
100:  LDA    6,0(3)                     * set fp
101:  LDA    4,2(7)                     * compute return address
102:  ST     4,1(6)                     * store it
103:  LDC    7,40(0)                    * double
104:  LD     1,0(5)                     * pop-temp: load
105:  LDA    5,-1(5)                    * decrement sp
106:  JEQ    1,2(7)                     * if right = 0 skip
107:  LDC    2,1(0)                     * result = true
108:  LDA    7,1(7)                     * skip
109:  LDC    2,0(0)                     * result = false
110:  LDA    5,1(5)                     * push-temp: increment sp
111:  ST     2,0(5)                     * store at sp
112:  LD     0,0(5)                     * pop-temp: load
113:  LDA    5,-1(5)                    * decrement sp
114:  JEQ    0,4(7)                     * skip true case
115:  LDC    2,99(0)                    * int literal
116:  LDA    5,1(5)                     * push-temp: increment sp
117:  ST     2,0(5)                     * store at sp
118:  LDA    7,12(7)                    * skip false case
119:  LD     2,3(6)                     * a
120:  LDA    5,1(5)                     * push-temp: increment sp
121:  ST     2,0(5)                     * store at sp
122:  LD     2,4(6)                     * b
123:  LDA    5,1(5)                     * push-temp: increment sp
124:  ST     2,0(5)                     * store at sp
125:  LD     1,0(5)                     * pop-temp-lr: load right
126:  LD     0,-1(5)                    * pop-temp-lr: load left
127:  LDA    5,-2(5)                    * decrement sp by 2
128:  DIV    2,0,1                      * /
129:  LDA    5,1(5)                     * push-temp: increment sp
130:  ST     2,0(5)                     * store at sp
131:  LD     2,0(5)                     * pop-temp: load
132:  LDA    5,-1(5)                    * decrement sp
133:  ST     2,0(6)                     * store return-value
134:  LDA    5,0(6)                     * point sp to it
135:  LD     4,1(6)                     * fetch return address
136:  LD     6,2(6)                     * restore caller's fp
137:  LDA    7,0(4)                     * jump to return addr
