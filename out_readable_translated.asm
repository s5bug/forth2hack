@16383
D=A
@SP
M=D
@2047
D=A
@LCL
M=D
@_ENTRY
0;JMP
(PLUS)
@SP
M=M+1
A=M-1
D=M
A=A+1
M=D+M
@LCL
M=M+1
A=M-1
A=M
0;JMP
(_ENTRY)
@2
D=A
@SP
M=M-1
A=M+1
M=D
@3
D=A
@SP
M=M-1
A=M+1
M=D
@STMT_2
D=A
@LCL
M=M-1
A=M+1
M=D
@PLUS
0;JMP
(STMT_2)