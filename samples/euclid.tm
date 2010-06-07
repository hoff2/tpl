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
 13:  LDC    7,125(0)                   * jump to main
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
 26:  SUB    0,0,1                      * <: subtract right from left
 27:  JLT    0,2(7)                     * result < 0 if left < right
 28:  LDC    2,0(0)                     * false
 29:  LDA    7,1(7)                     * jump over
 30:  LDC    2,1(0)                     * true
 31:  LDA    5,1(5)                     * push-temp: increment sp
 32:  ST     2,0(5)                     * store at sp
 33:  LD     0,0(5)                     * pop-temp: load
 34:  LDA    5,-1(5)                    * decrement sp
 35:  JEQ    0,4(7)                     * skip true case
 36:  LD     2,3(6)                     * a
 37:  LDA    5,1(5)                     * push-temp: increment sp
 38:  ST     2,0(5)                     * store at sp
 39:  LDA    7,23(7)                    * skip false case
 40:  LDC    2,0(0)                     * setup new stack frame
 41:  ST     2,1(5)                     * return value
 42:  ST     6,3(5)                     * save current fp
 43:  LDA    5,3(5)                     * set sp to push args
 44:  LD     2,3(6)                     * a
 45:  LDA    5,1(5)                     * push-temp: increment sp
 46:  ST     2,0(5)                     * store at sp
 47:  LD     2,4(6)                     * b
 48:  LDA    5,1(5)                     * push-temp: increment sp
 49:  ST     2,0(5)                     * store at sp
 50:  LD     1,0(5)                     * pop-temp-lr: load right
 51:  LD     0,-1(5)                    * pop-temp-lr: load left
 52:  LDA    5,-2(5)                    * decrement sp by 2
 53:  SUB    2,0,1                      * -
 54:  LDA    5,1(5)                     * push-temp: increment sp
 55:  ST     2,0(5)                     * store at sp
 56:  LD     2,4(6)                     * b
 57:  LDA    5,1(5)                     * push-temp: increment sp
 58:  ST     2,0(5)                     * store at sp
 59:  LDA    6,-4(5)                    * compute new fp
 60:  LDA    4,2(7)                     * compute return address
 61:  ST     4,1(6)                     * store it
 62:  LDC    7,17(0)                    * remainder
 63:  LD     2,0(5)                     * pop-temp: load
 64:  LDA    5,-1(5)                    * decrement sp
 65:  ST     2,0(6)                     * store return-value
 66:  LDA    5,0(6)                     * point sp to it
 67:  LD     4,1(6)                     * fetch return address
 68:  LD     6,2(6)                     * restore caller's fp
 69:  LDA    7,0(4)                     * jump to return addr
 70:  LD     2,4(6)                     * b
 71:  LDA    5,1(5)                     * push-temp: increment sp
 72:  ST     2,0(5)                     * store at sp
 73:  LDC    2,0(0)                     * int literal
 74:  LDA    5,1(5)                     * push-temp: increment sp
 75:  ST     2,0(5)                     * store at sp
 76:  LD     1,0(5)                     * pop-temp-lr: load right
 77:  LD     0,-1(5)                    * pop-temp-lr: load left
 78:  LDA    5,-2(5)                    * decrement sp by 2
 79:  SUB    0,0,1                      * =: subtract right from left
 80:  JEQ    0,2(7)                     * result = 0 if left = right
 81:  LDC    2,0(0)                     * false
 82:  LDA    7,1(7)                     * jump over
 83:  LDC    2,1(0)                     * false
 84:  LDA    5,1(5)                     * push-temp: increment sp
 85:  ST     2,0(5)                     * store at sp
 86:  LD     0,0(5)                     * pop-temp: load
 87:  LDA    5,-1(5)                    * decrement sp
 88:  JEQ    0,4(7)                     * skip true case
 89:  LD     2,3(6)                     * a
 90:  LDA    5,1(5)                     * push-temp: increment sp
 91:  ST     2,0(5)                     * store at sp
 92:  LDA    7,25(7)                    * skip false case
 93:  LDC    2,0(0)                     * setup new stack frame
 94:  ST     2,1(5)                     * return value
 95:  ST     6,3(5)                     * save current fp
 96:  LDA    5,3(5)                     * set sp to push args
 97:  LD     2,4(6)                     * b
 98:  LDA    5,1(5)                     * push-temp: increment sp
 99:  ST     2,0(5)                     * store at sp
100:  LDC    2,0(0)                     * setup new stack frame
101:  ST     2,1(5)                     * return value
102:  ST     6,3(5)                     * save current fp
103:  LDA    5,3(5)                     * set sp to push args
104:  LD     2,3(6)                     * a
105:  LDA    5,1(5)                     * push-temp: increment sp
106:  ST     2,0(5)                     * store at sp
107:  LD     2,4(6)                     * b
108:  LDA    5,1(5)                     * push-temp: increment sp
109:  ST     2,0(5)                     * store at sp
110:  LDA    6,-4(5)                    * compute new fp
111:  LDA    4,2(7)                     * compute return address
112:  ST     4,1(6)                     * store it
113:  LDC    7,17(0)                    * remainder
114:  LDA    6,-4(5)                    * compute new fp
115:  LDA    4,2(7)                     * compute return address
116:  ST     4,1(6)                     * store it
117:  LDC    7,70(0)                    * gcd
118:  LD     2,0(5)                     * pop-temp: load
119:  LDA    5,-1(5)                    * decrement sp
120:  ST     2,0(6)                     * store return-value
121:  LDA    5,0(6)                     * point sp to it
122:  LD     4,1(6)                     * fetch return address
123:  LD     6,2(6)                     * restore caller's fp
124:  LDA    7,0(4)                     * jump to return addr
125:  LDC    2,0(0)                     * setup new stack frame
126:  ST     2,1(5)                     * return value
127:  ST     6,3(5)                     * save current fp
128:  LDA    5,3(5)                     * set sp to push args
129:  LD     2,3(6)                     * a
130:  LDA    5,1(5)                     * push-temp: increment sp
131:  ST     2,0(5)                     * store at sp
132:  LD     2,4(6)                     * b
133:  LDA    5,1(5)                     * push-temp: increment sp
134:  ST     2,0(5)                     * store at sp
135:  LDA    6,-4(5)                    * compute new fp
136:  LDA    4,2(7)                     * compute return address
137:  ST     4,1(6)                     * store it
138:  LDC    7,70(0)                    * gcd
139:  LD     2,0(5)                     * pop-temp: load
140:  LDA    5,-1(5)                    * decrement sp
141:  ST     2,0(6)                     * store return-value
142:  LDA    5,0(6)                     * point sp to it
143:  LD     4,1(6)                     * fetch return address
144:  LD     6,2(6)                     * restore caller's fp
145:  LDA    7,0(4)                     * jump to return addr
