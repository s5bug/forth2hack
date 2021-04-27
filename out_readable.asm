addr 16383
move d, a
addr SP
move m, d
addr 2047
move d, a
addr LCL
move m, d
addr _ENTRY
jump
PLUS:
    addr SP
    move m, m+1
    move a, m-1
    move d, m
    move a, a+1
    move m, d+m
    addr LCL
    move m, m+1
    move a, m-1
    move a, m
    jump
_ENTRY:
    addr 2
    move d, a
    addr SP
    move m, m-1
    move a, m+1
    move m, d
    
    addr 3
    move d, a
    addr SP
    move m, m-1
    move a, m+1
    move m, d
    
    addr STMT_2
    move d, a
    addr LCL
    move m, m-1
    move a, m+1
    move m, d
    addr PLUS
    jump
    STMT_2:
