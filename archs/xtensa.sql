BEGIN TRANSACTION;
CREATE TABLE instructions(platform TEXT, mnem TEXT, description TEXT);INSERT INTO "instructions" VALUES('xtensa', 'ABS', 'Absolute Value
Assembler Syntax
ABS ar, at
Description
ABS calculates the absolute value of the contents of address register at and writes it to 
address register ar. Arithmetic overflow is not detected.
Operation
AR[r] ← if AR[t]31 then −AR[t] else AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ABS.S', 'Absolute Value Single
Assembler Syntax
ABS.S fr, fs
Description
ABS.S computes the single-precision absolute value of the contents of floating-point 
register fs and writes the result to floating-point register fr.
Operation
FR[r] ← abss(FR[s])
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'ADD', 'Add
Assembler Syntax
ADD ar, as, at
Description
ADD calculates the two’s complement 32-bit sum of address registers as and at. The 
low 32 bits of the sum are written to address register ar. Arithmetic overflow is not 
detected.
ADD is a 24-bit instruction. The ADD.N density-option instruction performs the same 
operation in a 16-bit encoding.
Assembler Note
The assembler may convert ADD instructions to ADD.N when the Code Density Option is 
enabled. Prefixing the ADD instruction with an underscore (_ADD) disables this optimiza-
tion and forces the assembler to generate the wide form of the instruction.
Operation
AR[r] ← AR[s] + AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ADD.N', 'Narrow Add
Assembler Syntax
ADD.N ar, as, at
Description
This performs the same operation as the ADD instruction in a 16-bit encoding.
ADD.N calculates the two’s complement 32-bit sum of address registers as and at. The 
low 32 bits of the sum are written to address register ar. Arithmetic overflow is not 
detected.
Assembler Note
The assembler may convert ADD.N instructions to ADD. Prefixing the ADD.N instruction 
with an underscore (_ADD.N) disables this optimization and forces the assembler to 
generate the narrow form of the instruction.
Operation
AR[r] ← AR[s] + AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ADD.S', 'Add Single
Assembler Syntax
ADD.S fr, fs, ft
Description
ADD.S computes the IEEE754 single-precision sum of the contents of floating-point 
registers fs and ft, and writes the result to floating-point register fr.
Operation
FR[r] ← FR[s] +s FR[t]
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'ADDI', 'Add Immediate
Assembler Syntax
ADDI at, as, -128..127
Description
ADDI calculates the two’s complement 32-bit sum of address register as and a constant 
encoded in the imm8 field. The low 32 bits of the sum are written to address register at. 
Arithmetic overflow is not detected.
The immediate operand encoded in the instruction can range from -128 to 127. It is de-
coded by sign-extending imm8.
ADDI is a 24-bit instruction. The ADDI.N density-option instruction performs a similar 
operation (the immediate operand has less range) in a 16-bit encoding.
Assembler Note
The assembler may convert ADDI instructions to ADDI.N when the Code Density 
Option is enabled and the immediate operand falls within the available range. If the im-
mediate is too large the assembler may substitute an equivalent sequence. Prefixing the 
ADDI instruction with an underscore (_ADDI) disables these optimizations and forces 
the assembler to generate the wide form of the instruction or an error instead.
Operation
AR[t] ← AR[s] + (imm8724||imm8)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ADDI.N', 'Narrow Add Immediate
Assembler Syntax
ADDI.N ar, as, imm
Description
ADDI.N is similar to ADDI, but has a 16-bit encoding and supports a smaller range of 
immediate operand values encoded in the instruction word.
ADDI.N calculates the two’s complement 32-bit sum of address register as and an 
operand encoded in the t field. The low 32 bits of the sum are written to address regis-
ter ar. Arithmetic overflow is not detected.
The operand encoded in the instruction can be -1 or one to 15. If t is zero, then a value 
of -1 is used, otherwise the value is the zero-extension of t.
Assembler Note
The assembler may convert ADDI.N instructions to ADDI. Prefixing the ADDI.N instruc-
tion with an underscore (_ADDI.N) disables this optimization and forces the assembler 
to generate the narrow form of the instruction. In the assembler syntax, the number to 
be added to the register operand is specified. When the specified value is -1, the assem-
bler encodes it as zero.
Operation
AR[r] ← AR[s] + (if t = 04 then 132 else 028||t)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ADDMI', 'Add Immediate with Shift by 8
Assembler Syntax
ADDMI at, as, -32768..32512
Description
ADDMI extends the range of constant addition. It is often used in conjunction with load 
and store instructions to extend the range of the base, plus offset the calculation.
ADDMI calculates the two’s complement 32-bit sum of address register as and an oper-
and encoded in the imm8 field. The low 32 bits of the sum are written to address register 
at. Arithmetic overflow is not detected.
The operand encoded in the instruction can have values that are multiples of 256 rang-
ing from -32768 to 32512. It is decoded by sign-extending imm8 and shifting the result 
left by eight bits.
Assembler Note
In the assembler syntax, the value to be added to the register operand is specified. The 
assembler encodes this into the instruction by dividing by 256.
Operation
AR[t] ← AR[s] + (imm8716||imm8||08)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ADDX2', 'Add with Shift by 1
Assembler Syntax
ADDX2 ar, as, at
Description
ADDX2 calculates the two’s complement 32-bit sum of address register as shifted left by 
one bit and address register at. The low 32 bits of the sum are written to address regis-
ter ar. Arithmetic overflow is not detected.
ADDX2 is frequently used for address calculation and as part of sequences to multiply by 
small constants.
Operation
AR[r] ← (AR[s]30..0||0) + AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ADDX4', 'Add with Shift by 2
Core Architecture (See Section 4.2 on page 50r)
Assembler Syntax
ADDX4 ar, as, at
Description
ADDX4 calculates the two’s complement 32-bit sum of address register as shifted left by 
two bits and address register at. The low 32 bits of the sum are written to address reg-
ister ar. Arithmetic overflow is not detected.
ADDX4 is frequently used for address calculation and as part of sequences to multiply by 
small constants.
Operation
AR[r] ← (AR[s]29..0||02) + AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ADDX8', 'Add with Shift by 3
Assembler Syntax
ADDX8 ar, as, at
Description
ADDX8 calculates the two’s complement 32-bit sum of address register as shifted left by 
ar. Arithmetic overflow is not detected.
ADDX8 is frequently used for address calculation and as part of sequences to multiply by 
small constants.
Operation
AR[r] ← (AR[s]28..0||03) + AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ALL4', 'All 4 Booleans True
Assembler Syntax
ALL4 bt, bs
Description
ALL4 sets Boolean register bt to the logical and of the four Boolean registers bs+0, 
bs+1, bs+2, and bs+3. bs must be a multiple of four (b0, b4, b8, or b12); otherwise the 
operation of this instruction is not defined. ALL4 reduces four test results such that the 
result is true if all four tests are true.
When the sense of the bs Booleans is inverted (0 → true, 1 → false), use ANY4 and an 
inverted test of the result.
Operation
BRt ← BRs+3 and BRs+2 and BRs+1 and BRs+0
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ALL8', 'All 8 Booleans True
Assembler Syntax
ALL8 bt, bs
Description
ALL8 sets Boolean register bt to the logical and of the eight Boolean registers bs+0, 
bs+1, … bs+6, and bs+7. bs must be a multiple of eight (b0 or b8); otherwise the oper-
ation of this instruction is not defined. ALL8 reduces eight test results such that the re-
sult is true if all eight tests are true.
When the sense of the bs Booleans is inverted (0 → true, 1 → false), use ANY8 and an 
inverted test of the result.
Operation
BRt ← BRs+7 and ... and BRs+0
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'AND', 'Bitwise Logical And
Assembler Syntax
AND ar, as, at
Description
AND calculates the bitwise logical and of address registers as and at. The result is 
written to address register ar.
Operation
AR[r] ← AR[s] and AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ANDB', 'Boolean And
Assembler Syntax
ANDB br, bs, bt
Description
ANDB performs the logical and of Boolean registers bs and bt and writes the result to 
Boolean register br.
When the sense of one of the source Booleans is inverted (0 → true, 1 → false), use 
ANDBC. When the sense of both of the source Booleans is inverted, use ORB and an 
inverted test of the result.
Operation
BRr ← BRs and BRt
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ANDBC', 'Boolean And with Complement
Assembler Syntax
ANDBC br, bs, bt
Description
ANDBC performs the logical and of Boolean register bs with the logical complement of 
Boolean register bt, and writes the result to Boolean register br.
Operation
BRr ← BRs and not BRt
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ANY4', 'Any 4 Booleans True
Assembler Syntax
ANY4 bt, bs
Description
ANY4 sets Boolean register bt to the logical or of the four Boolean registers bs+0, 
bs+1, bs+2, and bs+3. bs must be a multiple of four (b0, b4, b8, or b12); otherwise the 
operation of this instruction is not defined. ANY4 reduces four test results such that the 
result is true if any of the four tests are true.
When the sense of the bs Booleans is inverted (0 → true, 1 → false), use ALL4 and an 
inverted test of the result.
Operation
BRt ← BRs+3 or BRs+2 or BRs+1 or BRs+0
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ANY8', 'Any 8 Booleans True
Assembler Syntax
ANY8 bt, bs
Description
ANY8 sets Boolean register bt to the logical or of the eight Boolean registers bs+0, 
bs+1, … bs+6, and bs+7. bs must be a multiple of eight (b0 or b8); otherwise the oper-
ation of this instruction is not defined. ANY8 reduces eight test results such that the re-
sult is true if any of the eight tests are true.
When the sense of the bs Booleans is inverted (0 → true, 1 → false), use ALL8 and an 
inverted test of the result.
Operation
BRt ← BRs+7 or ... or BRs+0
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BALL', 'Branch if All Bits Set
Assembler Syntax
BALL as, at, label
Description
BALL branches if all the bits specified by the mask in address register at are set in ad-
dress register as. The test is performed by taking the bitwise logical and of at and the 
complement of as, and testing if the result is zero.
The target instruction address of the branch is given by the address of the BALL instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If any of the 
masked bits are clear, execution continues with the next sequential instruction.
The inverse of BALL is BNALL.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BALL) disables 
this feature and forces the assembler to generate an error in this case.
Operation
if ((not AR[s]) and AR[t]) = 032 then
nextPC ← PC + (imm8724||imm8) + 4
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BANY', 'Branch if Any Bit Set
Assembler Syntax
BANY as, at, label
Description
BANY branches if any of the bits specified by the mask in address register at are set in 
address register as. The test is performed by taking the bitwise logical and of as and at 
and testing if the result is non-zero.
The target instruction address of the branch is given by the address of the BANY instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If all of the 
masked bits are clear, execution continues with the next sequential instruction.
The inverse of BANY is BNONE.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BANY) disables 
this feature and forces the assembler to generate an error in this case.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if (AR[s] and AR[t]) ≠ 032 then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BBC', 'Branch if Bit Clear
Assembler Syntax
BBC as, at, label
Description
BBC branches if the bit specified by the low five bits of address register at is clear in ad-
dress register as. For little-endian processors, bit 0 is the least significant bit and bit 31 
is the most significant bit. For big-endian processors, bit 0 is the most significant bit and 
bit 31 is the least significant bit.
The target instruction address of the branch is given by the address of the BBC instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If the specified 
bit is set, execution continues with the next sequential instruction.
The inverse of BBC is BBS.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BBC) disables this 
feature and forces the assembler to generate an error in this case.
Operation
b ← AR[t]4..0 xor msbFirst5
if AR[s]b = 0 then
endif
Exceptions
nextPC ← PC + (imm8724||imm8) + 4
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BBCI', 'Branch if Bit Clear Immediate
bbi3..0
Assembler Syntax
BBCI as, 0..31, label
Description
BBCI branches if the bit specified by the constant encoded in the bbi field of the in-
struction word is clear in address register as. For little-endian processors, bit 0 is the 
least significant bit and bit 31 is the most significant bit. For big-endian processors bit 0 
is the most significant bit and bit 31 is the least significant bit. The bbi field is split, with 
bits 3..0 in bits 7..4 of the instruction word, and bit 4 in bit 12 of the instruction word.
The target instruction address of the branch is given by the address of the BBCI instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If the specified 
bit is set, execution continues with the next sequential instruction.
The inverse of BBCI is BBSI.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BBCI) disables 
this feature and forces the assembler to generate an error in this case.
Operation
b ← bbi xor msbFirst5
if AR[s]b = 0 then
endif
Exceptions
nextPC ← PC + (imm8724||imm8) + 4
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BBCI.L', 'Branch if Bit Clear Immediate LE
bbi3..0
Assembler Macro
Assembler Syntax
BBCI.L as, 0..31, label
Description
BBCI.L is an assembler macro for BBCI that always uses little-endian bit numbering. 
That is, it branches if the bit specified by its immediate is clear in address register as, 
where bit 0 is the least significant bit and bit 31 is the most significant bit.
The inverse of BBCI.L is BBSI.L.
Assembler Note
For little-endian processors, BBCI.L and BBCI are identical. For big-endian processors, 
the assembler will convert BBCI.L instructions to BBCI by changing the encoded imme-
diate value to 31-imm.
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BBS', 'Branch if Bit Set
Assembler Syntax
BBS as, at, label
Description
BBS branches if the bit specified by the low five bits of address register at is set in ad-
dress register as. For little-endian processors, bit 0 is the least significant bit and bit 31 
is the most significant bit. For big-endian processors, bit 0 is the most significant bit and 
bit 31 is the least significant bit.
The target instruction address of the branch is given by the address of the BBS instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If the specified 
bit is clear, execution continues with the next sequential instruction.
The inverse of BBS is BBC.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BBS) disables this 
feature and forces the assembler to generate an error in this case.
Operation
b ← AR[t]4..0 xor msbFirst5
if AR[s]b ≠ 0 then
endif
Exceptions
nextPC ← PC + (imm8724||imm8) + 4
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BBSI', 'Branch if Bit Set Immediate
bbi3..0
Assembler Syntax
BBSI as, 0..31, label
Description
BBSI branches if the bit specified by the constant encoded in the bbi field of the in-
struction word is set in address register as. For little-endian processors, bit 0 is the least 
significant bit and bit 31 is the most significant bit. For big-endian processors, bit 0 is the 
most significant bit and bit 31 is the least significant bit. The bbi field is split, with bits 
The target instruction address of the branch is given by the address of the BBSI instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If the specified 
bit is clear, execution continues with the next sequential instruction.
The inverse of BBSI is BBCI.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BBSI) disables 
this feature and forces the assembler to generate an error in this case.
Operation
b ← bbi xor msbFirst5
if AR[s]b ≠ 0 then
endif
Exceptions
nextPC ← PC + (imm8724||imm8) + 4
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BBSI.L', 'Branch if Bit Set Immediate LE
bbi
Assembler Macro
Assembler Syntax
BBSI.L as, 0..31, label
Description
BBSI.L is an assembler macro for BBSI that always uses little-endian bit numbering. 
That is, it branches if the bit specified by its immediate is set in address register as, 
where bit 0 is the least significant bit and bit 31 is the most significant bit.
The inverse of BBSI.L is BBCI.L.
Assembler Note
For little-endian processors, BBSI.L and BBSI are identical. For big-endian processors, 
the assembler will convert BBSI.L instructions to BBSI by changing the encoded imme-
diate value to 31-imm.
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BEQ', 'Branch if Equal
Assembler Syntax
BEQ as, at, label
Description
BEQ branches if address registers as and at are equal.
The target instruction address of the branch is given by the address of the BEQ instruc-
tion plus the sign-extended 8-bit imm8 field of the instruction plus four. If the registers 
are not equal, execution continues with the next sequential instruction.
The inverse of BEQ is BNE.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BEQ) disables this 
feature and forces the assembler to generate an error in this case.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if AR[s] = AR[t] then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BEQI', 'Branch if Equal Immediate
Assembler Syntax
BEQI as, imm, label
Description
BEQI branches if address register as and a constant encoded in the r field are equal. 
The constant values encoded in the r field are not simply 0..15. For the constant values 
that can be encoded by r, see Table 3–17 on page 41.
The target instruction address of the branch is given by the address of the BEQI instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If the register is 
not equal to the constant, execution continues with the next sequential instruction.
The inverse of BEQI is BNEI.
Assembler Note
The assembler may convert BEQI instructions to BEQZ or BEQZ.N when given an imme-
diate operand that evaluates to zero. The assembler will substitute an equivalent se-
quence of instructions when the label is out of range. Prefixing the instruction mnemonic 
with an underscore (_BEQI) disables these features and forces the assembler to gener-
ate an error instead.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if AR[s] = B4CONST(r) then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BEQZ', 'Branch if Equal to Zero
Assembler Syntax
BEQZ as, label
Description
BEQZ branches if address register as is equal to zero. BEQZ provides 12 bits of target 
range instead of the eight bits available in most conditional branches.
The target instruction address of the branch is given by the address of the BEQZ instruc-
tion, plus the sign-extended 12-bit imm12 field of the instruction plus four. If register as 
is not equal to zero, execution continues with the next sequential instruction.
The inverse of BEQZ is BNEZ.
Assembler Note
The assembler may convert BEQZ instructions to BEQZ.N when the Code Density 
Option is enabled and the branch target is reachable with the shorter instruction. The 
assembler will substitute an equivalent sequence of instructions when the label is out of 
range. Prefixing the instruction mnemonic with an underscore (_BEQZ) disables these 
features and forces the assembler to generate the wide form of the instruction and an 
error when the label is out of range).
Operation
nextPC ← PC + (imm121120||imm12) + 4
if AR[s] = 032 then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BEQZ.N', 'Narrow Branch if Equal Zero
imm63..0
Assembler Syntax
BEQZ.N as, label
Description
This performs the same operation as the BEQZ instruction in a 16-bit encoding. BEQZ.N 
branches if address register as is equal to zero. BEQZ.N provides six bits of target 
range instead of the 12 bits available in BEQZ.
The target instruction address of the branch is given by the address of the BEQZ.N in-
struction, plus the zero-extended 6-bit imm6 field of the instruction plus four. Because 
the offset is unsigned, this instruction can only be used to branch forward. If register as 
is not equal to zero, execution continues with the next sequential instruction.
The inverse of BEQZ.N is BNEZ.N.
Assembler Note
The assembler may convert BEQZ.N instructions to BEQZ. The assembler will substitute 
an equivalent sequence of instructions when the label is out of range. Prefixing the in-
struction mnemonic with an underscore (_BEQZ.N) disables these features and forces 
the assembler to generate the narrow form of the instruction and an error when the label 
is out of range.
Operation
nextPC ← PC + (026||imm6) + 4
if AR[s] = 032 then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BF', 'Branch if False
Assembler Syntax
BF bs, label
Description
BF branches to the target address if Boolean register bs is false.
The target instruction address of the branch is given by the address of the BF instruction 
plus the sign-extended 8-bit imm8 field of the instruction plus four. If the Boolean register 
bs is true, execution continues with the next sequential instruction.
The inverse of BF is BT.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BF) disables this 
feature and forces the assembler to generate an error when the label is out of range.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if not BRs then
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BGE', 'Branch if Greater Than or Equal
Assembler Syntax
BGE as, at, label
Description
BGE branches if address register as is two’s complement greater than or equal to ad-
dress register at.
The target instruction address of the branch is given by the address of the BGE instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If the address 
register as is less than address register at, execution continues with the next sequen-
tial instruction.
The inverse of BGE is BLT.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BGE) disables this 
feature and forces the assembler to generate an error in this case.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if AR[s] ≥ AR[t] then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BGEI', 'Branch if Greater Than or Equal Immediate
Assembler Syntax
BGEI as, imm, label
Description
BGEI branches if address register as is two’s complement greater than or equal to the 
constant encoded in the r field. The constant values encoded in the r field are not sim-
ply 0..15. For the constant values that can be encoded by r, see Table 3–17 on page 41.
The target instruction address of the branch is given by the address of the BGEI instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If the address 
register as is less than the constant, execution continues with the next sequential 
instruction.
The inverse of BGEI is BLTI.
Assembler Note
The assembler may convert BGEI instructions to BGEZ when given an immediate oper-
and that evaluates to zero. The assembler will substitute an equivalent sequence of in-
structions when the label is out of range. Prefixing the instruction mnemonic with an un-
derscore (_BGEI) disables these features and forces the assembler to generate an error 
instead.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if AR[s] ≥ B4CONST(r) then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BGEU', 'Branch if Greater Than or Equal Unsigned
Assembler Syntax
BGEU as, at, label
Description
BGEU branches if address register as is unsigned greater than or equal to address reg-
ister at.
The target instruction address of the branch is given by the address of the BGEU instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If the address 
register as is unsigned less than address register at, execution continues with the next 
sequential instruction.
The inverse of BGEU is BLTU.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BGEU) disables 
this feature and forces the assembler to generate an error in this case.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if (0||AR[s]) ≥ (0||AR[t]) then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BGEUI', 'Branch if Greater Than or Eq Unsigned Imm
Assembler Syntax
BGEUI as, imm, label
Description
BGEUI branches if address register as is unsigned greater than or equal to the constant 
encoded in the r field. The constant values encoded in the r field are not simply 0..15. 
For the constant values that can be encoded by r, see Table 3–18 on page 42.
The target instruction address of the branch is given by the address of the BGEUI in-
struction plus the sign-extended 8-bit imm8 field of the instruction plus four. If the ad-
dress register as is less than the constant, execution continues with the next sequential 
instruction.
The inverse of BGEUI is BLTUI.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BGEUI) disables 
this feature and forces the assembler to generate an error in this case.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if (0||AR[s]) ≥ (0||B4CONSTU(r)) then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BGEZ', 'Branch if Greater Than or Equal to Zero
Assembler Syntax
BGEZ as, label
Description
BGEZ branches if address register as is greater than or equal to zero (the most signifi-
cant bit is clear). BGEZ provides 12 bits of target range instead of the eight bits available 
in most conditional branches.
The target instruction address of the branch is given by the address of the BGEZ instruc-
tion plus the sign-extended 12-bit imm12 field of the instruction plus four. If register as is 
less than zero, execution continues with the next sequential instruction.
The inverse of BGEZ is BLTZ.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BGEZ) disables 
this feature and forces the assembler to generate an error in this case.
Operation
nextPC ← PC + (imm121120||imm12) + 4
if AR[s]31 = 0 then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BLT', 'Branch if Less Than
Assembler Syntax
BLT as, at, label
Description
BLT branches if address register as is two’s complement less than address register at.
The target instruction address of the branch is given by the address of the BLT instruc-
tion plus the sign-extended 8-bit imm8 field of the instruction plus four. If the address 
register as is greater than or equal to address register at, execution continues with the 
next sequential instruction.
The inverse of BLT is BGE.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BLT) disables this 
feature and forces the assembler to generate an error in this case.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if AR[s] < AR[t] then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BLTI', 'Branch if Less Than Immediate
Assembler Syntax
BLTI as, imm, label
Description
BLTI branches if address register as is two’s complement less than the constant encod-
ed in the r field. The constant values encoded in the r field are not simply 0..15. For the 
constant values that can be encoded by r, see Table 3–17 on page 41.
The target instruction address of the branch is given by the address of the BLTI instruc-
tion plus the sign-extended 8-bit imm8 field of the instruction plus four. If the address 
register as is greater than or equal to the constant, execution continues with the next 
sequential instruction.
The inverse of BLTI is BGEI.
Assembler Note
The assembler may convert BLTI instructions to BLTZ when given an immediate oper-
and that evaluates to zero. The assembler will substitute an equivalent sequence of in-
structions when the label is out of range. Prefixing the instruction mnemonic with an un-
derscore (_BLTI) disables these features and forces the assembler to generate an error 
instead.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if AR[s] < B4CONST(r) then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BLTU', 'Branch if Less Than Unsigned
Assembler Syntax
BLTU as, at, label
Description
BLTU branches if address register as is unsigned less than address register at.
The target instruction address of the branch is given by the address of the BLTU instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If the address 
register as is greater than or equal to address register at, execution continues with the 
next sequential instruction.
The inverse of BLTU is BGEU.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BLTU) disables 
this feature and forces the assembler to generate an error in this case.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if (0||AR[s]) < (0||AR[t]) then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BLTUI', 'Branch if Less Than Unsigned Immediate
Assembler Syntax
BLTUI as, imm, label
Description
BLTUI branches if address register as is unsigned less than the constant encoded in 
the r field. The constant values encoded in the r field are not simply 0..15. For the 
constant values that can be encoded by r, see Table 3–18 on page 42.
The target instruction address of the branch is given by the address of the BLTUI in-
struction, plus the sign-extended 8-bit imm8 field of the instruction plus four. If the ad-
dress register as is greater than or equal to the constant, execution continues with the 
next sequential instruction.
The inverse of BLTUI is BGEUI.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BLTUI) disables 
this feature and forces the assembler to generate an error in this case.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if (0||AR[s]) < (0||B4CONSTU(r)) then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BLTZ', 'Branch if Less Than Zero
Assembler Syntax
BLTZ as, label
Description
BLTZ branches if address register as is less than zero (the most significant bit is set). 
BLTZ provides 12 bits of target range instead of the eight bits available in most condi-
tional branches.
The target instruction address of the branch is given by the address of the BLTZ instruc-
tion, plus the sign-extended 12-bit imm12 field of the instruction plus four. If register as 
is greater than or equal to zero, execution continues with the next sequential instruction.
The inverse of BLTZ is BGEZ.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BLTZ) disables 
this feature and forces the assembler to generate an error in this case.
Operation
nextPC ← PC + (imm121120||imm12) + 4
if AR[s]31 ≠ 0 then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BNALL', 'Branch if Not-All Bits Set
Assembler Syntax
BNALL as, at, label
Description
BNALL branches if any of the bits specified by the mask in address register at are clear 
in address register as (that is, if they are not all set). The test is performed by taking the 
bitwise logical and of at with the complement of as and testing if the result is non-zero.
The target instruction address of the branch is given by the address of the BNALL in-
struction, plus the sign-extended 8-bit imm8 field of the instruction plus four. If all of the 
masked bits are set, execution continues with the next sequential instruction.
The inverse of BNALL is BALL.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BNALL) disables 
this feature and forces the assembler to generate an error in this case.
Operation
if ((not AR[s]) and AR[t]) ≠ 032 then
nextPC ← PC + (imm8724||imm8) + 4
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BNE', 'Branch if Not Equal
Assembler Syntax
BNE as, at, label
Description
BNE branches if address registers as and at are not equal.
The target instruction address of the branch is given by the address of the BNE instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If the registers 
are equal, execution continues with the next sequential instruction.
The inverse of BNE is BEQ.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BNE) disables this 
feature and forces the assembler to generate an error in this case.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if AR[s] ≠ AR[t] then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BNEI', 'Branch if Not Equal Immediate
Assembler Syntax
BNEI as, imm, label
Description
BNEI branches if address register as and a constant encoded in the r field are not 
equal. The constant values encoded in the r field are not simply 0..15. For the constant 
values that can be encoded by r, see Table 3–17 on page 41.
The target instruction address of the branch is given by the address of the BNEI instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If the register is 
equal to the constant, execution continues with the next sequential instruction.
The inverse of BNEI is BEQI.
Assembler Note
The assembler may convert BNEI instructions to BNEZ or BNEZ.N when given an imme-
diate operand that evaluates to zero. The assembler will substitute an equivalent se-
quence of instructions when the label is out of range. Prefixing the instruction mnemonic 
with an underscore (_BNEI) disables these features and forces the assembler to gener-
ate an error instead.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if AR[s] ≠ B4CONST(r) then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BNEZ', 'Branch if Not-Equal to Zero
Assembler Syntax
BNEZ as, label
Description
BNEZ branches if address register as is not equal to zero. BNEZ provides 12 bits of tar-
get range instead of the eight bits available in most conditional branches.
The target instruction address of the branch is given by the address of the BNEZ instruc-
tion, plus the sign-extended 12-bit imm12 field of the instruction plus four. If register as 
is equal to zero, execution continues with the next sequential instruction.
The inverse of BNEZ is BEQZ.
Assembler Note
The assembler may convert BNEZ instructions to BNEZ.N when the Code Density 
Option is enabled and the branch target is reachable with the shorter instruction. The 
assembler will substitute an equivalent sequence of instructions when the label is out of 
range. Prefixing the instruction mnemonic with an underscore (_BNEZ) disables these 
features and forces the assembler to generate the BNEZ form of the instruction and an 
error when the label is out of range.
Operation
nextPC ← PC + (imm121120||imm12) + 4
if AR[s] ≠ 032 then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BNEZ.N', 'Narrow Branch if Not Equal Zero
imm63..0
imm65..4
Assembler Syntax
BNEZ.N as, label
Description
This performs the same operation as the BNEZ instruction in a 16-bit encoding. BNEZ.N 
branches if address register as is not equal to zero. BNEZ.N provides six bits of target 
range instead of the 12 bits available in BNEZ.
The target instruction address of the branch is given by the address of the BNEZ.N in-
struction, plus the zero-extended 6-bit imm6 field of the instruction plus four. Because 
the offset is unsigned, this instruction can only be used to branch forward. If register as 
is equal to zero, execution continues with the next sequential instruction.
The inverse of BNEZ.N is BEQZ.N.
Assembler Note
The assembler may convert BNEZ.N instructions to BNEZ. The assembler will substitute 
an equivalent sequence of instructions when the label is out of range. Prefixing the in-
struction mnemonic with an underscore (_BNEZ.N) disables these features and forces 
the assembler to generate the narrow form of the instruction and an error when the label 
is out of range.
Operation
nextPC ← PC + (026||imm6) + 4
if AR[s] ≠ 032 then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BNONE', 'Branch if No Bit Set
Assembler Syntax
BNONE as, at, label
Description
BNONE branches if all of the bits specified by the mask in address register at are clear in 
address register as (that is, if none of them are set). The test is performed by taking the 
bitwise logical and of as with at and testing if the result is zero.
The target instruction address of the branch is given by the address of the BNONE in-
struction, plus the sign-extended 8-bit imm8 field of the instruction plus four. If any of the 
masked bits are set, execution continues with the next sequential instruction.
The inverse of BNONE is BANY.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BNONE) disables 
this feature and forces the assembler to generate an error in this case.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if (AR[s] and AR[t]) = 032 then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'BREAK', 'Breakpoint
Assembler Syntax
BREAK 0..15, 0..15
Description
This instruction simply raises an exception when it is executed and PS.INTLEVEL < 
DEBUGLEVEL. The high-priority vector for DEBUGLEVEL is used. The DEBUGCAUSE reg-
ister is written as part of raising the exception to indicate that BREAK raised the debug 
exception. The address of the BREAK instruction is stored in EPC[DEBUGLEVEL]. The s 
and t fields of the instruction word are not used by the processor; they are available for 
use by the software. When PS.INTLEVEL ≥ DEBUGLEVEL, BREAK is a no-op.
The BREAK instruction typically calls a debugger when program execution reaches a 
certain point (a “breakpoint”). The instruction at the breakpoint is replaced with the 
BREAK instruction. To continue execution after a breakpoint is reached, the debugger 
must re-write the BREAK to the original instruction, single-step by one instruction, and 
then put back the BREAK instruction again.
Writing instructions requires special consideration. See the ISYNC instruction for more 
information.
When it is not possible to write the instruction memory (for example, for ROM code), the 
IBREAKA feature provides breakpoint capabilities (see Debug Option).
Software can also use BREAK to indicate an error condition that requires the program-
mer’s attention. The s and t fields may encode information about the situation.
BREAK is a 24-bit instruction. The BREAK.N density-option instruction performs a similar 
operation in a 16-bit encoding.
Assembler Note
The assembler may convert BREAK instructions to BREAK.N when the Code Density 
Option is enabled and the second imm is zero. Prefixing the instruction mnemonic with 
an underscore (_BREAK) disables this optimization and forces the assembler to gener-
ate the wide form of the instruction.
Operation
if PS.INTLEVEL < DEBUGLEVEL then
EPC[DEBUGLEVEL] ← PC
EPS[DEBUGLEVEL] ← PS
DEBUGCAUSE ← 001000
nextPC ← InterruptVector[DEBUGLEVEL]
PS.EXCM ← 1
PS.INTLEVEL ← DEBUGLEVEL
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) DebugExcep(BREAK) if Debug Option
');
INSERT INTO "instructions" VALUES('xtensa', 'BREAK.N', 'Narrow Breakpoint
Section 4.3.1 on page 53)
Assembler Syntax
BREAK.N 0..15
Description
BREAK.N is similar in operation to BREAK (page 293), except that it is encoded in a  
DEBUGCAUSE. Use this instruction to set breakpoints on 16-bit instructions.
Assembler Note
The assembler may convert BREAK.N instructions to BREAK. Prefixing the BREAK.N 
instruction with an underscore (_BREAK.N) disables this optimization and forces the 
assembler to generate the narrow form of the instruction.
Operation
if PS.INTLEVEL < DEBUGLEVEL then
EPC[DEBUGLEVEL] ← PC
EPS[DEBUGLEVEL] ← PS
DEBUGCAUSE ← 010000
nextPC ← InterruptVector[DEBUGLEVEL]
PS.EXCM ← 1
PS.INTLEVEL ← DEBUGLEVEL
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) DebugExcep(BREAK.N) if Debug Option
');
INSERT INTO "instructions" VALUES('xtensa', 'BT', 'Branch if True
Assembler Syntax
BT bs, label
Description
BT branches to the target address if Boolean register bs is true.
The target instruction address of the branch is given by the address of the BT instruc-
tion, plus the sign-extended 8-bit imm8 field of the instruction plus four. If the Boolean 
register bs is false, execution continues with the next sequential instruction.
The inverse of BT is BF.
Assembler Note
The assembler will substitute an equivalent sequence of instructions when the label is 
out of range. Prefixing the instruction mnemonic with an underscore (_BT) disables this 
feature and forces the assembler to generate an error when the label is out of range.
Operation
nextPC ← PC + (imm8724||imm8) + 4
if BRs then
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'CALL0', 'Non-windowed Call
offset
Assembler Syntax
CALL0 label
Description
CALL0 calls subroutines without using register windows. The return address is placed in 
a0, and the processor then branches to the target address. The return address is the 
address of the CALL0 instruction plus three.
The target instruction address must be 32-bit aligned. This allows CALL0 to have a larg-
er effective range (-524284 to 524288 bytes). The target instruction address of the call is 
given by the address of the CALL0 instruction with the least significant two bits set to 
zero plus the sign-extended 18-bit offset field of the instruction shifted by two, plus 
four.
The RET and RET.N instructions are used to return from a subroutine called by CALL0.
See the CALLX0 instruction (page 304) for calling routines where the target address is 
given by the contents of a register.
To call using the register window mechanism, see the CALL4, CALL8, and CALL12 in-
structions.
Operation
AR[0] ← PC + 3
nextPC ← (PC31..2 + (offset1712||offset) + 1)||00
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'CALL4', 'Call PC-relative, Rotate Window by 4
offset
Assembler Syntax
CALL4 label
Description
CALL4 calls subroutines using the register windows mechanism, requesting the callee 
rotate the window by four registers. The CALL4 instruction does not rotate the window it-
self, but instead stores the window increment for later use by the ENTRY instruction. The 
return address and window increment are placed in the caller’s a4 (the callee’s a0), and 
the processor then branches to the target address. The return address is the address of 
the next instruction (the address of the CALL4 instruction plus three). The window incre-
ment is also stored in the CALLINC field of the PS register, where it is accessed by the 
ENTRY instruction.
The target instruction address must be a 32-bit aligned ENTRY instruction. This allows 
CALL4 to have a larger effective range (−524284 to 524288 bytes). The target instruc-
tion address of the call is given by the address of the CALL4 instruction with the two 
least significant bits set to zero plus the sign-extended 18-bit offset field of the instruc-
tion shifted by two, plus four.
See the CALLX4 instruction for calling routines where the target address is given by the 
contents of a register.
Use the RETW and RETW.N instructions to return from a subroutine called by CALL4.
The window increment stored with the return address register in a4 occupies the two 
most significant bits of the register, and therefore those bits must be filled in by the sub-
routine return. The RETW and RETW.N instructions fill in these bits from the two most sig-
nificant bits of their own address. This prevents register-window calls from being used to 
call a routine in a different 1GB region of the address space.
See the CALL0 instruction for calling routines using the non-windowed subroutine proto-
col.
The caller’s a4..a15 are the same registers as the callee’s a0..a11 after the callee 
executes the ENTRY instruction. You can use these registers for parameter passing. The 
caller’s a0..a3 are hidden by CALL4, and therefore you can use them to keep values 
that are live across the call.
Operation
WindowCheck (00, 00, 01)
PS.CALLINC ← 01
AR[0100] ← 01||(PC + 3)29..0
nextPC ← (PC31..2 + (offset1712||offset) + 1)||00
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'CALL8', 'Call PC-relative, Rotate Window by 8
offset
Assembler Syntax
CALL8 label
Description
CALL8 calls subroutines using the register windows mechanism, requesting the callee 
rotate the window by eight registers. The CALL8 instruction does not rotate the window 
itself, but instead stores the window increment for later use by the ENTRY instruction. 
The return address and window increment are placed in the caller’s a8 (the callee’s a0), 
and the processor then branches to the target address. The return address is the ad-
dress of the next instruction (the address of the CALL8 instruction plus three). The win-
dow increment is also stored in the CALLINC field of the PS register, where it is access-
ed by the ENTRY instruction.
The target instruction address must be a 32-bit aligned ENTRY instruction. This allows 
CALL8 to have a larger effective range (−524284 to 524288 bytes). The target instruc-
tion address of the call is given by the address of the CALL8 instruction with the two 
least significant bits set to zero, plus the sign-extended 18-bit offset field of the in-
struction shifted by two, plus four.
See the CALLX8 instruction for calling routines where the target address is given by the 
contents of a register.
Use the RETW and RETW.N instructions to return from a subroutine called by CALL8.
The window increment stored with the return address register in a8 occupies the two 
most significant bits of the register, and therefore those bits must be filled in by the sub-
routine return. The RETW and RETW.N instructions fill in these bits from the two most sig-
nificant bits of their own address. This prevents register-window calls from being used to 
call a routine in a different 1GB region of the address space.
See the CALL0 instruction for calling routines using the non-windowed subroutine proto-
col.
The caller’s a8..a15 are the same registers as the callee’s a0..a7 after the callee exe-
cutes the ENTRY instruction. You can use these registers for parameter passing. The 
caller’s a0..a7 are hidden by CALL8, and therefore you may use them to keep values 
that are live across the call.
Operation
WindowCheck (00, 00, 10)
PS.CALLINC ← 10
AR[1000] ← 10||(PC + 3)29..0
nextPC ← (PC31..2 + (offset1712||offset) + 1)||00
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'CALL12', 'Call PC-relative, Rotate Window by 12
offset
Assembler Syntax
CALL12 label
Description
CALL12 calls subroutines using the register windows mechanism, requesting the callee 
rotate the window by 12 registers. The CALL12 instruction does not rotate the window it-
self, but instead stores the window increment for later use by the ENTRY instruction. The 
return address and window increment are placed in the caller’s a12 (the callee’s a0), 
and the processor then branches to the target address. The return address is the ad-
dress of the next instruction (the address of the CALL12 instruction plus three). The win-
dow increment is also stored in the CALLINC field of the PS register, where it is access-
ed by the ENTRY instruction.
The target instruction address must be a 32-bit aligned ENTRY instruction. This allows 
CALL12 to have a larger effective range (−524284 to 524288 bytes). The target instruc-
tion address of the call is given by the address of the CALL12 instruction with the two 
least significant bits set to zero, plus the sign-extended 18-bit offset field of the in-
struction shifted by two, plus four.
See the CALLX12 instruction for calling routines where the target address is given by 
the contents of a register.
The RETW and RETW.N instructions return from a subroutine called by CALL12.
The window increment stored with the return address register in a12 occupies the two 
most significant bits of the register, and therefore those bits must be filled in by the sub-
routine return. The RETW and RETW.N instructions fill in these bits from the two most sig-
nificant bits of their own address. This prevents register-window calls from being used to 
call a routine in a different 1GB region of the address space.
');
INSERT INTO "instructions" VALUES('xtensa', 'CALL12', 'Call PC-relative, Rotate Window by 12
See the CALL0 instruction for calling routines using the non-windowed subroutine proto-
col.
The caller’s a12..a15 are the same registers as the callee’s a0..a3 after the callee exe-
cutes the ENTRY instruction. You can use these registers for parameter passing. The 
caller’s a0..a11 are hidden by CALL12, and therefore you may use them to keep values 
that are live across the call.
Operation
WindowCheck (00, 00, 11)
PS.CALLINC ← 11
AR[1100] ← 11||(PC + 3)29..0
nextPC ← (PC31..2 + (offset1712||offset) + 1)||00
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'CALLX0', 'Non-windowed Call Register
Assembler Syntax
CALLX0 as
Description
CALLX0 calls subroutines without using register windows. The return address is placed 
in a0, and the processor then branches to the target address. The return address is the 
address of the CALLX0 instruction, plus three.
The target instruction address of the call is given by the contents of address register as.
The RET and RET.N instructions return from a subroutine called by CALLX0.
To call using the register window mechanism, see the CALLX4, CALLX8, and CALLX12 
instructions.
Operation
nextPC ← AR[s]
AR[0] ← PC + 3
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'CALLX4', 'Call Register, Rotate Window by 4
Assembler Syntax
CALLX4 as
Description
CALLX4 calls subroutines using the register windows mechanism, requesting the callee 
rotate the window by four registers. The CALLX4 instruction does not rotate the window 
itself, but instead stores the window increment for later use by the ENTRY instruction. 
The return address and window increment are placed in the caller’s a4 (the callee’s a0), 
and the processor then branches to the target address. The return address is the ad-
dress of the next instruction (the address of the CALLX4 instruction plus three). The win-
dow increment is also stored in the CALLINC field of the PS register, where it is access-
ed by the ENTRY instruction.
The target instruction address of the call is given by the contents of address register as. 
The target instruction must be an ENTRY instruction.
See the CALL4 instruction for calling routines where the target address is given by a PC-
relative offset in the instruction.
The RETW and RETW.N instructions return from a subroutine called by CALLX4.
The window increment stored with the return address register in a4 occupies the two 
most significant bits of the register, and therefore those bits must be filled in by the sub-
routine return. The RETW and RETW.N instructions fill in these bits from the two most sig-
nificant bits of their own address. This prevents register-window calls from being used to 
call a routine in a different 1GB region of the address space.
See the CALLX0 instruction for calling routines using the non-windowed subroutine 
protocol.
The caller’s a4..a15 are the same registers as the callee’s a0..a11 after the callee exe-
cutes the ENTRY instruction. You can use these registers for parameter passing. The 
caller’s a0..a3 are hidden by CALLX4, and therefore you may use them to keep values 
that are live across the call.
Operation
WindowCheck (00, 00, 01)
PS.CALLINC ← 01
AR[01||00] ← 01||(PC + 3)29..0
nextPC ← AR[s]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'CALLX8', 'Call Register, Rotate Window by 8
Assembler Syntax
CALLX8 as
Description
CALLX8 calls subroutines using the register windows mechanism, requesting the callee 
rotate the window by eight registers. The CALLX8 instruction does not rotate the window 
itself, but instead stores the window increment for later use by the ENTRY instruction. 
The return address and window increment are placed in the caller’s a8 (the callee’s a0), 
and the processor then branches to the target address. The return address is the ad-
dress of the next instruction (the address of the CALLX8 instruction plus three). The win-
dow increment is also stored in the CALLINC field of the PS register, where it is access-
ed by the ENTRY instruction.
The target instruction address of the call is given by the contents of address register as. 
The target instruction must be an ENTRY instruction.
See the CALL8 instruction for calling routines where the target address is given by a PC-
relative offset in the instruction.
The RETW and RETW.N (page 482) instructions return from a subroutine called by 
CALLX8.
The window increment stored with the return address register in a8 occupies the two 
most significant bits of the register, and therefore those bits must be filled in by the sub-
routine return. The RETW and RETW.N instructions fill in these bits from the two most sig-
nificant bits of their own address. This prevents register-window calls from being used to 
call a routine in a different 1GB region of the address space.
See the CALLX0 instruction for calling routines using the non-windowed subroutine pro-
tocol.
The caller’s a8..a15 are the same registers as the callee’s a0..a7 after the callee exe-
cutes the ENTRY instruction. You can use these registers for parameter passing. The 
caller’s a0..a7 are hidden by CALLX8, and therefore you may use them to keep values 
that are live across the call.
Operation
WindowCheck (00, 00, 10)
PS.CALLINC ← 10
AR[10||00] ← 10||(PC + 3)29..0
nextPC ← AR[s]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'CALLX12', 'Call Register, Rotate Window by 12
Assembler Syntax
CALLX12 as
Description
CALLX12 calls subroutines using the register windows mechanism, requesting the 
callee rotate the window by 12 registers. The CALLX12 instruction does not rotate the 
window itself, but instead stores the window increment for later use by the ENTRY in-
struction. The return address and window increment are placed in the caller’s a12 (the 
callee’s a0), and the processor then branches to the target address. The return address 
is the address of the next instruction (the address of the CALLX12 instruction plus 
three). The window increment is also stored in the CALLINC field of the PS register, 
where it is accessed by the ENTRY instruction.
The target instruction address of the call is given by the contents of address register as. 
The target instruction must be an ENTRY instruction.
See the CALL12 instruction for calling routines where the target address is given by a 
PC-relative offset in the instruction.
The RETW and RETW.N instructions return from a subroutine called by CALLX12.
The window increment stored with the return address register in a12 occupies the two 
most significant bits of the register, and therefore those bits must be filled in by the sub-
routine return. The RETW and RETW.N instructions fill in these bits from the two most sig-
nificant bits of their own address. This prevents register-window calls from being used to 
call a routine in a different 1GB region of the address space. 
See the CALLX0 instruction for calling routines using the non-windowed subroutine 
protocol.
');
INSERT INTO "instructions" VALUES('xtensa', 'CALLX12', 'Call Register, Rotate Window by 12
The caller’s a12..a15 are the same registers as the callee’s a0..a3 after the callee exe-
cutes the ENTRY instruction. These registers may be used for parameter passing. The 
caller’s a0..a11 are hidden by CALLX12, and therefore may be used to keep values that 
are live across the call.
Operation
WindowCheck (00, 00, 11)
PS.CALLINC ← 11
AR[11||00] ← 11||(PC + 3)29..0
nextPC ← AR[s]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'CEIL.S', 'Ceiling Single to Fixed
Assembler Syntax
CEIL.S ar, fs, 0..15
Description
CEIL.S converts the contents of floating-point register fs from single-precision to 
signed integer format, rounding toward +∞. The single-precision value is first scaled by a 
power of two constant value encoded in the t field, with 0..15 representing 1.0, 2.0, 4.0, 
…, 32768.0. The scaling allows for a fixed point notation where the binary point is at the 
right end of the integer for t=0 and moves to the left as t increases, until for t=15 there 
are 15 fractional bits represented in the fixed point number. For positive overflow (value 
≥ 32''h7fffffff), positive infinity, or NaN, 32''h7fffffff is returned; for negative 
overflow (value ≤ 32''h80000000) or negative infinity, 32''h80000000 is returned. The 
result is written to address register ar.
Operation
AR[r] ← ceils(FR[s] ×s pows(2.0,t))
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'CLAMPS', 'Signed Clamp
Miscellaneous Operations Option (See Section 4.3.8 on page 62)
Assembler Syntax
CLAMPS ar, as, 7..22
Description
CLAMPS tests whether the contents of address register as fits as a signed value of 
imm+1 bits (in the range 7 to 22). If so, the value is written to address register ar; if not, 
the largest value of imm+1 bits with the same sign as as is written to ar. Thus CLAMPS 
performs the function
y ← min(max(x, −2imm), 2imm−1)
CLAMPS may be used in conjunction with instructions such as ADD, SUB, MUL16S, and 
so forth to implement saturating arithmetic.
Assembler Note
The immediate values accepted by the assembler are 7 to 22. The assembler encodes 
these in the t field of the instruction using 0 to 15.
Operation
sign ← AR[s]31
AR[r] ← if AR[s]30..t+7 = sign24-t 
then AR[s] 
else sign25-t||(not sign)t+7
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'DHI', 'Data Cache Hit Invalidate
Data Cache Option (See Section 4.5.5 on page 118)
Assembler Syntax
DHI as, 0..1020
Description
DHI invalidates the specified line in the level-1 data cache, if it is present. If the specified 
address is not in the data cache, then this instruction has no effect. If the specified ad-
dress is present, it is invalidated even if it contains dirty data. If the specified line has 
been locked by a DPFL instruction, then no invalidation is done and no exception is 
raised because of the lock. The line remains in the cache and must be unlocked by a 
DHU or DIU instruction before it can be invalidated. This instruction is useful before a 
DMA write to memory that overwrites the entire line.
DHI forms a virtual address by adding the contents of address register as and an 8-bit 
zero-extended constant value encoded in the instruction word shifted left by two. There-
fore, the offset can specify multiples of four from zero to 1020. If the Region Translation 
Option (page 156) or the MMU Option (page 158) is enabled, the virtual address is 
translated to the physical address. If not, the physical address is identical to the virtual 
address. If the translation encounters an error (for example, protection violation), the 
processor raises an exception (see Section 4.4.1.5 on page 89) as if it were loading 
from the virtual address.
Because the organization of caches is implementation-specific, the operation below 
specifies only a call to the implementation’s dhitinval function.
DHI is a privileged instruction.
Assembler Note
To form a virtual address DHI calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
vAddr ← AR[s] + (022||imm8||02)
(pAddr, attributes, cause) ← ltranslate(vAddr, CRING)
if invalid(attributes) then
EXCVADDR ← vAddr
Exception (cause)
dhitinval(vAddr, pAddr)
else
endif
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) Memory Group (see page 244)
(cid:132) GenExcep(LoadProhibitedCause) if Region Protection Option or MMU Option
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'DHU', 'Data Cache Hit Unlock
imm4
Data Cache Index Lock Option (See Section 4.5.7 on page 122)
Assembler Syntax
DHU as, 0..240
Description
DHU performs a data cache unlock if hit. The purpose of DHU is to remove the lock creat-
ed by a DPFL instruction. Xtensa ISA implementations that do not implement cache lock-
ing must raise an illegal instruction exception when this opcode is executed.
DHU checks whether the line containing the specified address is present in the data 
cache, and if so, it clears the lock associated with that line. To unlock by index without 
knowing the address of the locked line, use the DIU instruction.
DHU forms a virtual address by adding the contents of address register as and a 4-bit 
zero-extended constant value encoded in the instruction word shifted left by four. There-
fore, the offset can specify multiples of 16 from zero to 240. If the Region Translation 
Option (page 156) or the MMU Option (page 158) is enabled, the virtual address is 
translated to the physical address. If not, the physical address is identical to the virtual 
address. If the translation encounters an error (for example, protection violation), the 
processor raises an exception (see Section 4.4.1.5 on page 89) as if it were loading 
from the virtual address.
DHU is a privileged instruction.
Assembler Note
To form a virtual address DHU calculates the sum of address register as and the imm4 
field of the instruction word times 16. Therefore, the machine-code offset is in terms of 
instruction by dividing by 16.
Operation
Data Cache Hit Unlock
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
vAddr ← AR[s] + (024||imm4||04)
(pAddr, attributes, cause) ← ltranslate(vAddr, CRING)
if invalid(attributes) then
EXCVADDR ← vAddr
Exception (cause)
dhitunlock(vAddr, pAddr)
else
endif
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) Memory Group (see page 244)
(cid:132) GenExcep(LoadProhibitedCause) if Region Protection Option or MMU Option
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'DHWB', 'Data Cache Hit Writeback
Data Cache Option (See Section 4.5.5 on page 118)
Assembler Syntax
DHWB as, 0..1020
Description
This instruction forces dirty data in the data cache to be written back to memory. If the 
specified address is not in the data cache or is present but unmodified, then this instruc-
tion has no effect. If the specified address is present and modified in the data cache, the 
line containing it is written back, and marked unmodified. This instruction is useful be-
fore a DMA read from memory, to force writes to a frame buffer to become visible, or to 
force writes to memory shared by two processors.
DHWB forms a virtual address by adding the contents of address register as and an 8-bit 
zero-extended constant value encoded in the instruction word shifted left by two. There-
fore, the offset can specify multiples of four from zero to 1020. If the Region Translation 
Option (page 156) or the MMU Option (page 158) is enabled, the virtual address is 
translated to the physical address. If not, the physical address is identical to the virtual 
address. If the translation encounters an error (for example, protection violation), the 
processor raises an exception (see Section 4.4.1.5 on page 89) as if it were loading 
from the virtual address.
Because the organization of caches is implementation-specific, the operation below 
specifies only a call to the implementation’s dhitwriteback function.
Assembler Note
To form a virtual address DHWB calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Operation
Data Cache Hit Writeback
vAddr ← AR[s] + (022||imm8||02)
(pAddr, attributes, cause) ← ltranslate(vAddr, CRING)
if invalid(attributes) then
EXCVADDR ← vAddr
Exception (cause)
dhitwriteback(vAddr, pAddr)
else
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) Memory Group (see page 244)
(cid:132) GenExcep(LoadProhibitedCause) if Region Protection Option or MMU Option
Implementation Notes
Some Xtensa ISA implementations do not support write-back caches. For these imple-
mentations, the DHWB instruction performs no operation.
');
INSERT INTO "instructions" VALUES('xtensa', 'DHWBI', 'Data Cache Hit Writeback Invalidate
Data Cache Option (See Section 4.5.5 on page 118)
Assembler Syntax
DHWBI as, 0..1020
Description
DHWBI forces dirty data in the data cache to be written back to memory. If the specified 
address is not in the data cache, then this instruction has no effect. If the specified ad-
dress is present and modified in the data cache, the line containing it is written back. 
After the write-back, if any, the line containing the specified address is invalidated if 
present. If the specified line has been locked by a DPFL instruction, then no invalidation 
is done and no exception is raised because of the lock. The line is written back but re-
mains in the cache unmodified and must be unlocked by a DHU or DIU instruction before 
it can be invalidated. This instruction is useful in the same circumstances as DHWB and 
before a DMA write to memory or write from another processor to memory. If the line is 
certain to be completely overwritten by the write, you can use a DHI (as it is faster), but 
otherwise use a DHWBI.
DHWBI forms a virtual address by adding the contents of address register as and an  
Therefore, the offset can specify multiples of four from zero to 1020. If the Region Trans-
lation Option (page 156) or the MMU Option (page 158) is enabled, the virtual address is 
translated to the physical address. If not, the physical address is identical to the virtual 
address. If the translation encounters an error (for example, protection violation), the 
processor raises an exception (see Section 4.4.1.5 on page 89) as if it were loading 
from the virtual address.
Because the organization of caches is implementation-specific, the operation section 
below specifies only a call to the implementation’s dhitwritebackinval function.
Assembler Note
To form a virtual address, DHWBI calculates the sum of address register as and the 
imm8 field of the instruction word times four. Therefore, the machine-code offset is in 
terms of 32-bit (4 byte) units. However, the assembler expects a byte offset and encodes 
this into the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
(pAddr, attributes, cause) ← ltranslate(vAddr, CRING)
if invalid(attributes) then
EXCVADDR ← vAddr
Exception (cause)
dhitwritebackinval(vAddr, pAddr)
else
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) Memory Group (see page 244)
(cid:132) GenExcep(LoadProhibitedCause) if Region Protection Option or MMU Option
Implementation Notes
Some Xtensa ISA implementations do not support write-back caches. For these imple-
mentations DHWBI is identical to DHI.
');
INSERT INTO "instructions" VALUES('xtensa', 'DII', 'Data Cache Index Invalidate
Data Cache Option (See Section 4.5.5 on page 118))
Assembler Syntax
DII as, 0..1020
Description
DII uses the virtual address to choose a location in the data cache and invalidates the 
specified line. If the chosen line has been locked by a DPFL instruction, then no invalida-
tion is done and no exception is raised because of the lock. The line remains in the 
cache and must be unlocked by a DHU or DIU instruction before it can be invalidat-
ed.The method for mapping the virtual address to a data cache location is implementa-
tion-specific. This instruction is primarily useful for data cache initialization after power-
up.
DII forms a virtual address by adding the contents of address register as and an 8-bit 
zero-extended constant value encoded in the instruction word shifted left by two. There-
fore, the offset can specify multiples of four from zero to 1020. The virtual address 
chooses a cache line without translation and without raising the associated exceptions.
Because the organization of caches is implementation-specific, the operation section 
below specifies only a call to the implementation’s dindexinval function.
DII is a privileged instruction.
Assembler Note
To form a virtual address, DII calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
Implementation Notes
x ← ceil(log2(DataCacheBytes))
y ← log2(DataCacheBytes ÷ DataCacheWayCount)
z ← log2(DataCacheLineBytes)
The cache line specified by index Addrx-1..z in a direct-mapped cache or way  
Addrx-1..y and index Addry-1..z in a set-associative cache is the chosen line. If the 
specified cache way is not valid (the fourth way of a three way cache) the instruction 
does nothing. In some implementations all ways at index Addry-1..z are invalidated 
regardless of the specified way, but for future compatibility this behavior should not be 
assumed.
The additional ways invalidated in some implementations mean that care is needed in 
using this instruction with write-back caches. Dirty data in any way (at the specified in-
dex) of the cache will be lost and not just dirty data in the specified way. Because the in-
struction is primarily used at reset, this will not usually cause any difficulty.
');
INSERT INTO "instructions" VALUES('xtensa', 'DIU', 'Data Cache Index Unlock
imm4
Data Cache Index Lock Option (See Section 4.5.7 on page 122)
Assembler Syntax
DIU as, 0..240
Description
DIU uses the virtual address to choose a location in the data cache and unlocks the 
chosen line. The purpose of DIU is to remove the lock created by a DPFL instruction. 
The method for mapping the virtual address to a data cache location is implementation-
specific. This instruction is primarily useful for unlocking the entire data cache. Xtensa 
ISA implementations that do not implement cache locking must raise an illegal instruc-
tion exception when this opcode is executed.
To unlock a specific cache line if it is in the cache, use the DHU instruction.
DII forms a virtual address by adding the contents of address register as and a 4-bit 
zero-extended constant value encoded in the instruction word shifted left by four. There-
fore, the offset can specify multiples of 16 from zero to 240. The virtual address chooses 
a cache line without translation and without raising the associated exceptions.
Because the organization of caches is implementation-specific, the operation section 
below specifies only a call to the implementation’s dindexunlock function.
DIU is a privileged instruction.
Assembler Note
To form a virtual address DIU calculates the sum of address register as and the imm4 
field of the instruction word times 16. Therefore, the machine-code offset is in terms of 
instruction by dividing by 16.
Operation
Data Cache Index Unlock
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
vAddr ← AR[s] + (024||imm4||04)
dindexunlock(vAddr)
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
(cid:132) MemoryErrorException if Memory ECC/Parity Option
Implementation Notes
x ← ceil(log2(DataCacheBytes))
y ← log2(DataCacheBytes ÷ DataCacheWayCount)
z ← log2(DataCacheLineBytes)
The cache line specified by index Addrx-1..z in a direct-mapped cache or way  
Addrx-1..y and index Addry-1..z in a set-associative cache is the chosen line. If the 
specified cache way is not valid (the fourth way of a three way cache), the instruction 
does nothing.
');
INSERT INTO "instructions" VALUES('xtensa', 'DIWB', 'Data Cache Index Write Back
imm4
Data Cache Option (See Section 4.5.5 on page 118) (added in T1050)
Assembler Syntax
DIWB as, 0..240
Description
DIWB uses the virtual address to choose a line in the data cache and writes that line 
back to memory if it is dirty. The method for mapping the virtual address to a data cache 
line is implementation-specific. This instruction is primarily useful for forcing all dirty data 
in the cache back to memory. If the chosen line is present but unmodified, then this in-
struction has no effect. If the chosen line is present and modified in the data cache, it is 
written back, and marked unmodified. For set-associative caches, only one line out of 
one way of the cache is written back. Some Xtensa ISA implementations do not support 
writeback caches. For these implementations DIWB does nothing.
This instruction is useful for the same purposes as DHWB, but when either the address is 
not known or when the range of addresses is large enough that it is faster to operate on 
the entire cache.
DIWB forms a virtual address by adding the contents of address register as and a 4-bit 
zero-extended constant value encoded in the instruction word shifted left by four. There-
fore, the offset can specify multiples of 16 from zero to 240. The virtual address chooses 
a cache line without translation and without raising the associated exceptions.
Because the organization of caches is implementation-specific, the operation section 
below specifies only a call to the implementation’s dindexwriteback function.
DIWB is a privileged instruction.
Assembler Note
To form a virtual address DIWB calculates the sum of address register as and the imm4 
field of the instruction word times 16. Therefore, the machine-code offset is in terms of 
struction by dividing by 16.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
vAddr ← AR[s] + (024||imm4||04)
dindexwriteback(vAddr)
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
(cid:132) MemoryErrorException if Memory ECC/Parity Option
Implementation Notes
x ← ceil(log2(DataCacheBytes))
y ← log2(DataCacheBytes ÷ DataCacheWayCount)
z ← log2(DataCacheLineBytes)
The cache line specified by index Addrx-1..z in a direct-mapped cache or way  
Addrx-1..y and index Addry-1..z in a set-associative cache is the chosen line. If the 
specified cache way is not valid (the fourth way of a three way cache), the instruction 
does nothing.
Some Xtensa ISA implementations do not support write-back caches. For these imple-
mentations, the DIWB instruction has no effect.
');
INSERT INTO "instructions" VALUES('xtensa', 'DIWBI', 'Data Cache Index Write Back Invalidate
imm4
Data Cache Option (See Section 4.5.5 on page 118) (added in T1050)
Assembler Syntax
DIWBI as, 0..240
Description
DIWBI uses the virtual address to choose a line in the data cache and forces that line to 
be written back to memory if it is dirty. After the writeback, if any, the line is invalidated. 
The method for mapping the virtual address to a data cache location is implementation-
specific. If the chosen line is already invalid, then this instruction has no effect. If the 
chosen line has been locked by a DPFL instruction, then dirty data is written back but no 
invalidation is done and no exception is raised because of the lock. The line remains in 
the cache and must be unlocked by a DHU or DIU instruction before it can be invalidat-
ed. For set-associative caches, only one line out of one way of the cache is written back 
and invalidated. Some Xtensa ISA implementations do not support write-back caches. 
For these implementations DIWBI is similar to DII but invalidates only one line.
This instruction is useful for the same purposes as the DHWBI but when either the ad-
dress is not known, or when the range of addresses is large enough that it is faster to 
operate on the entire cache.
DIWBI forms a virtual address by adding the contents of address register as and a 4-bit 
zero-extended constant value encoded in the instruction word shifted left by four. There-
fore, the offset can specify multiples of 16 from zero to 240. The virtual address chooses 
a cache line without translation and without raising the associated exceptions.
Because the organization of caches is implementation-specific, the operation section 
below specifies only a call to the implementation’s dindexwritebackinval function.
DIWBI is a privileged instruction.
Assembler Note
To form a virtual address, DIWBI calculates the sum of address register as and the 
imm4 field of the instruction word times 16. Therefore, the machine-code offset is in 
terms of 16 byte units. However, the assembler expects a byte offset and encodes this 
into the instruction by dividing by 16.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
vAddr ← AR[s] + (024||imm4||04)
dindexwritebackinval(vAddr)
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
(cid:132) MemoryErrorException if Memory ECC/Parity Option
Implementation Notes
x ← ceil(log2(DataCacheBytes))
y ← log2(DataCacheBytes ÷ DataCacheWayCount)
z ← log2(DataCacheLineBytes)
The cache line specified by index Addrx-1..z in a direct-mapped cache or way  
Addrx-1..y and index Addry-1..z in a set-associative cache is the chosen line. If the 
specified cache way is not valid (the fourth way of a three way cache), the instruction 
does nothing.
');
INSERT INTO "instructions" VALUES('xtensa', 'DPFL', 'Data Cache Prefetch and Lock
imm4
Data Cache Index Lock Option (See Section 4.5.7 on page 122)
Assembler Syntax
DPFL as, 0..240
Description
DPFL performs a data cache prefetch and lock. The purpose of DPFL is to improve per-
formance, and not to affect state defined by the ISA. Xtensa ISA implementations that 
do not implement cache locking must raise an illegal instruction exception when this op-
code is executed. In general, the performance improvement from using this instruction is 
implementation-dependent.
DPFL checks if the line containing the specified address is present in the data cache, 
and if not, it begins the transfer of the line from memory to the cache. The line is placed 
in the data cache and the line marked as locked, that is not replaceable by ordinary data 
cache misses. To unlock the line, use DHU or DIU. To prefetch without locking, use the 
DPFR, DPFW, DPFRO, or DPFWO instructions.
DPFL forms a virtual address by adding the contents of address register as and a 4-bit 
zero-extended constant value encoded in the instruction word shifted left by four. There-
fore, the offset can specify multiples of 16 from zero to 240. If the Region Translation 
Option (page 156) or the MMU Option (page 158) is enabled, the virtual address is 
translated to the physical address. If not, the physical address is identical to the virtual 
address. If the translation encounters an error (for example, protection violation), the 
processor raises one of several exceptions (see Section 4.4.1.5 on page 89).
Because the organization of caches is implementation-specific, the operation section 
below specifies only a call to the implementation’s dprefetch function.
DPFL is a privileged instruction.
Assembler Note
To form a virtual address, DPFL calculates the sum of address register as and the imm4 
field of the instruction word times 16. Therefore, the machine-code offset is in terms of 
instruction by dividing by 16.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
vAddr ← AR[s] + (024||imm4||04)
(pAddr, attributes, cause) ← ltranslate(vAddr, CRING)
if invalid(attributes) then
EXCVADDR ← vAddr
Exception (cause)
dprefetch(vAddr, pAddr, 0, 0, 1)
else
endif
endif
Exceptions
(cid:132) Memory Group (see page 244)
(cid:132) GenExcep(LoadProhibitedCause) if Region Protection Option or MMU Option
(cid:132) GenExcep(PrivilegedCause) if Exception Option
Implementation Notes
If, before the instruction executes, there are not two available DataCache ways at the re-
quired index, a Load Store Error exception is raised.
');
INSERT INTO "instructions" VALUES('xtensa', 'DPFR', 'Data Cache Prefetch for Read
Data Cache Option (See Section 4.5.5 on page 118)
Assembler Syntax
DPFR as, 0..1020
Description
DPFR performs a data cache prefetch for read. The purpose of DPFR is to improve per-
formance, but not to affect state defined by the ISA. Therefore, some Xtensa ISA imple-
mentations may choose to implement this instruction as a simple “no-operation” instruc-
tion. In general, the performance improvement from using this instruction is 
implementation-dependent.
In some Xtensa ISA implementations, DPFR checks whether the line containing the 
specified address is present in the data cache, and if not, it begins the transfer of the 
line from memory. The four data prefetch instructions provide different “hints” about how 
the data is likely to be used in the future. DPFR indicates that the data is only likely to be 
read, possibly more than once, before it is replaced by another line in the cache.
DPFR forms a virtual address by adding the contents of address register as and an 8-bit 
zero-extended constant value encoded in the instruction word shifted left by two. There-
fore, the offset can specify multiples of four from zero to 1020. If the Region Translation 
Option (page 156) or the MMU Option (page 158) is enabled, the virtual address is 
translated to the physical address. If not, the physical address is identical to the virtual 
address. If the translation or memory reference encounters an error (for example, pro-
tection violation or non-existent memory), the processor performs no operation. This al-
lows the instruction to be used to speculatively fetch an address that does not exist or is 
protected without either causing an error or allowing inappropriate action.
Because the organization of caches is implementation-specific, the operation section 
below specifies only a call to the implementation’s dprefetch function.
Assembler Note
To form a virtual address, DPFR calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
(pAddr, attributes, cause) ← ltranslate(vAddr, CRING)
if not invalid(attributes) then
endif
Exceptions
dprefetch(vAddr, pAddr, 0, 0, 0)
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'DPFRO', 'Data Cache Prefetch for Read Once
Data Cache Option (See Section 4.5.5 on page 118)
Assembler Syntax
DPFRO as, 0..1020
Description
DPFRO performs a data cache prefetch for read once. The purpose of DPFRO is to im-
prove performance, but not to affect state defined by the ISA. Therefore, some Xtensa 
ISA implementations may choose to implement this instruction as a simple “no-opera-
tion” instruction. In general, the performance improvement from using this instruction is 
implementation-dependent.
In some Xtensa ISA implementations, DPFRO checks whether the line containing the 
specified address is present in the data cache, and if not, it begins the transfer of the 
line from memory. Four data prefetch instructions provide different “hints” about how the 
data is likely to be used in the future. DPFRO indicates that the data is only likely to be 
read once before it is replaced by another line in the cache. In some implementations, 
this hint might be used to select a specific cache way or to select a streaming buffer 
instead of the cache.
DPFRO forms a virtual address by adding the contents of address register as and an 8-
bit zero-extended constant value encoded in the instruction word shifted left by two. 
Therefore, the offset can specify multiples of four from zero to 1020. If the Region Trans-
lation Option (page 156) or the MMU Option (page 158) is enabled, the virtual address is 
translated to the physical address. If not, the physical address is identical to the virtual 
address. If the translation or memory reference encounters an error (for example, pro-
tection violation or non-existent memory), the processor performs no operation. This al-
lows the instruction to be used to speculatively fetch an address that does not exist or is 
protected without either causing an error or allowing inappropriate action.
Because the organization of caches is implementation-specific, the operation section 
below specifies only a call to the implementation’s dprefetch function.
Assembler Note
To form a virtual address, DPFRO calculates the sum of address register as and the 
imm8 field of the instruction word times four. Therefore, the machine-code offset is in 
terms of 32-bit (4 byte) units. However, the assembler expects a byte offset and encodes 
this into the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
(pAddr, attributes, cause) ← ltranslate(vAddr, CRING)
if not invalid(attributes) then
endif
Exceptions
dprefetch(vAddr, pAddr, 0, 1, 0)
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'DPFW', 'Data Cache Prefetch for Write
Data Cache Option (See Section 4.5.5 on page 118)
Assembler Syntax
DPFW as, 0..1020
Description
DPFW performs a data cache prefetch for write. The purpose of DPFW is to improve per-
formance, but not to affect the ISA state. Therefore, some Xtensa ISA implementations 
may choose to implement this instruction as a simple “no-operation” instruction. In gen-
eral, the performance improvement from using this instruction is implementation-depen-
dent.
In some Xtensa ISA implementations, DPFW checks whether the line containing the 
specified address is present in the data cache, and if not, begins the transfer of the line 
from memory. Four data prefetch instructions provide different “hints” about how the 
data is likely to be used in the future. DPFW indicates that the data is likely to be written 
before it is replaced by another line in the cache. In some implementations, this fetches 
the data with write permission (for example, in a system with shared and exclusive 
states).
DPFW forms a virtual address by adding the contents of address register as and an 8-bit 
zero-extended constant value encoded in the instruction word shifted left by two. There-
fore, the offset can specify multiples of four from zero to 1020. If the Region Translation 
Option (page 156) or the MMU Option (page 158) is enabled, the virtual address is 
translated to the physical address. If not, the physical address is identical to the virtual 
address. If the translation or memory reference encounters an error (for example, pro-
tection violation or non-existent memory), the processor performs no operation. This al-
lows the instruction to be used to speculatively fetch an address that does not exist or is 
protected without either causing an error or allowing inappropriate action.
Because the organization of caches is implementation-specific, the operation section 
below specifies only a call to the implementation’s dprefetch function.
Assembler Note
To form a virtual address DPFW calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
into the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
(pAddr, attributes, cause) ← ltranslate(vAddr, CRING)
if not invalid(attributes) then
endif
Exceptions
dprefetch(vAddr, pAddr, 1, 0, 0)
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'DPFWO', 'Data Cache Prefetch for Write Once
Data Cache Option (See Section 4.5.5 on page 118)
Assembler Syntax
DPFWO as, 0..1020
Description
DPFWO performs a data cache prefetch for write once. The purpose of DPFWO is to im-
prove performance, but not to affect the ISA state. Therefore, some Xtensa ISA imple-
mentations may choose to implement this instruction as a simple “no-operation” instruc-
tion. In general, the performance improvement from using this instruction is 
implementation-dependent.
In some Xtensa ISA implementations, DPFWO checks whether the line containing the 
specified address is present in the data cache, and if not, begins the transfer of the line 
from memory. Four data prefetch instructions provide different “hints” about how the 
data is likely to be used in the future. DPFWO indicates that the data is likely to be read 
and written once before it is replaced by another line in the cache. In some implementa-
tions, this write hint fetches the data with write permission (for example, in a system with 
shared and exclusive states). The write-once hint might be used to select a specific 
cache way or to select a streaming buffer instead of the cache.
DPFWO forms a virtual address by adding the contents of address register as and an  
Therefore, the offset can specify multiples of four from zero to 1020. If the Region Trans-
lation Option (page 156) or the MMU Option (page 158) is enabled, the virtual address is 
translated to the physical address. If not, the physical address is identical to the virtual 
address. If the translation or memory reference encounters an error (for example, pro-
tection violation or non-existent memory), the processor performs no operation. This al-
lows the instruction to be used to speculatively fetch an address that does not exist or is 
protected without either causing an error or allowing inappropriate action.
Because the organization of caches is implementation-specific, the operation section 
below specifies only a call to the implementation’s dprefetch function.
Assembler Note
To form a virtual address DPFWO calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
(pAddr, attributes, cause) ← ltranslate(vAddr, CRING)
if not invalid(attributes) then
endif
Exceptions
dprefetch(vAddr, pAddr, 1, 1, 0)
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'DSYNC', 'Load/Store Synchronize
Assembler Syntax
DSYNC
Description
DSYNC waits for all previously fetched WSR.*, XSR.*, WDTLB, and IDTLB instructions to 
be performed before interpreting the virtual address of the next load or store instruction. 
This operation is also performed as part of ISYNC, RSYNC, and ESYNC.
This instruction is appropriate after WSR.DBREAKC* and WSR.DBREAKA* instructions. 
See the Special Register Tables in Section 5.3 on page 208 and Section 5.5 on 
page 239 for a complete description of the uses of the DSYNC instruction.
Because the instruction execution pipeline is implementation-specific, the operation sec-
tion below specifies only a call to the implementation’s dsync function.
Operation
dsync()
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ENTRY', 'Subroutine Entry
Assembler Syntax
ENTRY as, 0..32760
Description
ENTRY is intended to be the first instruction of all subroutines called with CALL4, CALL8, 
CALL12, CALLX4, CALLX8, or CALLX12. This instruction is not intended to be used by a 
routine called by CALL0 or CALLX0.
ENTRY serves two purposes:
quested by the caller (as recorded in the PS.CALLINC field). 
stack frame. The as operand specifies the stack pointer register; it must specify one 
of a0..a3 or the operation of ENTRY is undefined. It is read before the window is 
moved, the stack frame size is subtracted, and then the as register in the moved 
window is written.
The stack frame size is specified as the 12-bit unsigned imm12 field in units of eight 
bytes. The size is zero-extended, shifted left by 3, and subtracted from the caller’s stack 
pointer to get the callee’s stack pointer. Therefore, stack frames up to 32760 bytes can 
be specified. The initial stack frame size must be a constant, but subsequently the 
MOVSP instruction can be used to allocate dynamically-sized objects on the stack, or to 
further extend a constant stack frame larger than 32760 bytes.
The windowed subroutine call protocol is described in Section 4.7.1.5 on page 187.
ENTRY is undefined if PS.WOE is 0 or if PS.EXCM is 1. Some implementations raise an 
illegal instruction exception in these cases, as a debugging aid.
In the assembler syntax, the number of bytes to be subtracted from the stack pointer is 
specified as the immediate. The assembler encodes this into the instruction by dividing 
by eight.
Operation
WindowCheck (00, PS.CALLINC, 00)
if as > 3 | PS.WOE = 0 | PS.EXCM = 1 then
else
-- undefined operation
-- may raise illegal instruction exception
AR[PS.CALLINC||s1..0] ← AR[s] − (017||imm12||03)
WindowBase ← WindowBase + (02||PS.CALLINC)
WindowStartWindowBase ← 1
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ESYNC', 'Execute Synchronize
Assembler Syntax
ESYNC
Description
ESYNC waits for all previously fetched WSR.*, and XSR.* instructions to be performed 
before the next instruction uses any register values. This operation is also performed as 
part of ISYNC and RSYNC. DSYNC is performed as part of this instruction.
This instruction is appropriate after WSR.EPC* instructions. See the Special Register 
Tables in Section 5.3 on page 208 for a complete description of the uses of the ESYNC 
instruction.
Because the instruction execution pipeline is implementation-specific, the operation sec-
tion below specifies only a call to the implementation’s esync function.
Operation
esync()
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'EXCW', 'Exception Wait
Exception Option (See Section 4.4.1 on page 82)
Assembler Syntax
EXCW
Description
EXCW waits for any exceptions of previously fetched instructions to be handled. Some 
Xtensa ISA implementations may have imprecise exceptions; on these implementations 
EXCW waits until all previous instruction exceptions are taken or the instructions are 
known to be exception-free. Because the instruction execution pipeline and exception 
handling is implementation-specific, the operation section below specifies only a call to 
the implementation’s ExceptionWait function.
Operation
ExceptionWait()
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'EXTUI', 'Extract Unsigned Immediate
op2
sa4
sae3..0
Assembler Syntax
EXTUI ar, at, shiftimm, maskimm
Description
EXTUI performs an unsigned bit field extraction from a 32-bit register value. Specifically, 
it shifts the contents of address register at right by the shift amount shiftimm, which is 
a value 0..31 stored in bits 16 and 11..8 of the instruction word (the sa fields). The 
shift result is then ANDed with a mask of maskimm least-significant 1 bits and the result 
is written to address register ar. The number of mask bits, maskimm, may take the val-
ues 1..16, and is stored in the op2 field as maskimm-1. The bits extracted are there-
fore sa+op2..sa.
The operation of this instruction when sa+op2 > 31 is undefined and reserved for future 
use.
Operation
mask ← 031-op2||1op2+1
AR[r] ← (032||AR[t])31+sa..sa and mask
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'EXTW', 'External Wait
Assembler Syntax
EXTW
Description
EXTW is a superset of MEMW. EXTW ensures that both
-
-
all previous load, store, acquire, release, prefetch, and cache instructions; and
any other effect of any previous instruction which is visible at the pins of the 
Xtensa processor
complete (or perform as described in Section 4.3.12.1 on page 74) before either
-
-
any subsequent load, store, acquire, release, prefetch, or cache instructions; or
external effects of the execution of any following instruction is visible at the pins 
of the Xtensa processor (not including instruction prefetch or TIE Queue pops)
is allowed to begin.
While MEMW is intended to implement the volatile attribute of languages such as C 
and C++, EXTW is intended to be an ordering guarantee for all external effects that the 
processor can have, including processor pins defined in TIE.
Because the instruction execution pipeline is implementation-specific, the operation sec-
tion below specifies only a call to the implementation’s extw function.
Operation
extw()
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'FLOAT.S', 'Convert Fixed to Single
Assembler Syntax
FLOAT.S fr, as, 0..15
Description
FLOAT.S converts the contents of address register as from signed integer to single-pre-
cision format, rounding according to the current rounding mode. The converted integer 
value is then scaled by a power of two constant value encoded in the t field, with 0..15 
representing 1.0, 0.5, 0.25, …, 1.0÷s32768.0. The scaling allows for a fixed point nota-
tion where the binary point is at the right end of the integer for t=0 and moves to the left 
as t increases until for t=15 there are 15 fractional bits represented in the fixed point 
number. The result is written to floating-point register fr.
Operation
FR[r] ← floats(AR[s]) ×s pows(2.0,-t)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'FLOOR.S', 'Floor Single to Fixed
Assembler Syntax
FLOOR.S ar, fs, 0..15
Description
FLOOR.S converts the contents of floating-point register fs from single-precision to 
signed integer format, rounding toward -∞. The single-precision value is first scaled by a 
power of two constant value encoded in the t field, with 0..15 representing 1.0, 2.0, 4.0, 
…, 32768.0. The scaling allows for a fixed point notation where the binary point is at the 
right end of the integer for t=0 and moves to the left as t increases until for t=15 there 
are 15 fractional bits represented in the fixed point number. For positive overflow (value 
≥ 32''h7fffffff), positive infinity, or NaN, 32''h7fffffff is returned; for negative 
overflow (value ≤ 32''h80000000) or negative infinity, 32''h80000000 is returned. The 
result is written to address register ar.
Operation
AR[r] ← floors(FR[s] ×s pows(2.0,t))
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'IDTLB', 'Invalidate Data TLB Entry
Region Protection Option (see Section 4.6.3 on page 150) or MMU Option (see 
Section 4.6.5 on page 158)
Assembler Syntax
IDTLB as
Description
IDTLB invalidates the data TLB entry specified by the contents of address register as. 
See Section 4.6 on page 138 for information on the address register formats for specific 
Memory Protection and Translation Options. The point at which the invalidation is effect-
ed is implementation-specific. Any translation that would be affected by this invalidation 
before the execution of a DSYNC instruction is therefore undefined.
IDTLB is a privileged instruction.
The representation of validity in Xtensa TLBs is implementation-specific, and thus the 
operation section below writes the implementation-specific value  
InvalidDataTLBEntry.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
(vpn, ei, wi) ← SplitDataTLBEntrySpec(AR[s])
DataTLB[wi][ei] ← InvalidDataTLBEntry
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'IHI', 'Instruction Cache Hit Invalidate
Instruction Cache Option (See Section 4.5.2 on page 115)
Assembler Syntax
IHI as, 0..1020
Description
IHI performs an instruction cache hit invalidate. It invalidates the specified line in the in-
struction cache, if it is present. If the specified address is not in the instruction cache, 
then this instruction has no effect. If the specified line is already invalid, then this instruc-
tion has no effect. If the specified line has been locked by an IPFL instruction, then no 
invalidation is done and no exception is raised because of the lock. The line remains in 
the cache and must be unlocked by an IHU or IIU instruction before it can be invalidat-
ed. Otherwise, if the specified line is present, it is invalidated.
This instruction is required before executing instructions from the instruction cache that 
have been written by this processor, another processor, or DMA. The writes must first be 
forced out of the data cache, either by using DHWB or by using stores that bypass or 
write through the data cache. An ISYNC instruction should then be used to guarantee 
that the modified instructions are visible to instruction cache misses. The instruction 
cache should then be invalidated for the affected addresses using a series of IHI in-
structions. An ISYNC instruction should then be used to guarantee that this processor’s 
fetch pipeline does not contain instructions from the invalidated lines.
Because the organization of caches is implementation-specific, the operation section 
below specifies only a call to the implementation’s ihitinval function.
IHI forms a virtual address by adding the contents of address register as and an 8-bit 
zero-extended constant value encoded in the instruction word shifted left by two. There-
fore, the offset can specify multiples of four from zero to 1020. If the Region Translation 
Option (page 156) or the MMU Option (page 158) is enabled, the virtual address is 
translated to the physical address. If not, the physical address is identical to the virtual 
address. If the translation encounters an error (for example, protection violation), the 
processor raises one of several exceptions (see Section 4.4.1.5 on page 89). The trans-
lation is done as if the address were for an instruction fetch.
Assembler Note
To form a virtual address, IHI calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
(pAddr, attributes, cause) ← ftranslate(vAddr, CRING)
if invalid(attributes) then
EXCVADDR ← vAddr
Exception (cause)
ihitinval(vAddr, pAddr)
else
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) MemoryErrorException if Memory ECC/Parity Option
');
INSERT INTO "instructions" VALUES('xtensa', 'IHU', 'Instruction Cache Hit Unlock
imm4
Instruction Cache Index Lock Option (See Section 4.5.4 on page 117)
Assembler Syntax
IHU as, 0..240
Description
IHU performs an instruction cache unlock if hit. The purpose of IHU is to remove the 
lock created by an IPFL instruction. Xtensa ISA implementations that do not implement 
cache locking must raise an illegal instruction exception when this opcode is executed.
IHU checks whether the line containing the specified address is present in the instruc-
tion cache, and if so, it clears the lock associated with that line. To unlock by index with-
out knowing the address of the locked line, use the IIU instruction.
IHU forms a virtual address by adding the contents of address register as and a 4-bit 
zero-extended constant value encoded in the instruction word shifted left by four. There-
fore, the offset can specify multiples of 16 from zero to 240. If the Region Translation 
Option (page 156) or the MMU Option (page 158) is enabled, the virtual address is 
translated to the physical address. If not, the physical address is identical to the virtual 
address. If the translation encounters an error (for example or protection violation), the 
processor takes one of several exceptions (see Section 4.4.1.5 on page 89). The trans-
lation is done as if the address were for an instruction fetch.
IHU is a privileged instruction.
Assembler Note
To form a virtual address, IHU calculates the sum of address register as and the imm4 
field of the instruction word times 16. Therefore, the machine-code offset is in terms of 
instruction by dividing by 16.
Operation
Instruction Cache Hit Unlock
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
vAddr ← AR[s] + (024||imm4||04)
(pAddr, attributes, cause) ← ftranslate(vAddr, CRING)
if invalid(attributes) then
EXCVADDR ← vAddr
Exception (cause)
ihitunlock(vAddr, pAddr)
else
endif
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
(cid:132) MemoryErrorException if Memory ECC/Parity Option
');
INSERT INTO "instructions" VALUES('xtensa', 'III', 'Instruction Cache Index Invalidate
Instruction Cache Option (See Section 4.5.2 on page 115)
Assembler Syntax
III as, 0..1020
Description
III performs an instruction cache index invalidate. This instruction uses the virtual 
address to choose a location in the instruction cache and invalidates the specified line. 
The method for mapping the virtual address to an instruction cache location is imple-
mentation-specific. If the chosen line is already invalid, then this instruction has no 
effect. If the chosen line has been locked by an IPFL instruction, then no invalidation is 
done and no exception is raised because of the lock. The line remains in the cache and 
must be unlocked by an IHU or IIU instruction before it can be invalidated. This instruc-
tion is useful for instruction cache initialization after power-up or for invalidating the 
entire instruction cache.
III forms a virtual address by adding the contents of address register as and an 8-bit 
zero-extended constant value encoded in the instruction word shifted left by two. There-
fore, the offset can specify multiples of four from zero to 1020. The virtual address 
chooses a cache line without translation and without raising the associated exceptions.
Because the organization of caches is implementation-specific, the operation section 
below specifies only a call to the implementation’s iindexinval function.
III is a privileged instruction.
Assembler Note
To form a virtual address, III calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Operation
Instruction Cache Index Invalidate
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
vAddr ← AR[s] + (022||imm8||02)
iindexinval(vAddr, pAddr)
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
Implementation Notes
x ← ceil(log2(InstCacheBytes))
y ← log2(InstCacheBytes ÷ InstCacheWayCount)
z ← log2(InstCacheLineBytes)
The cache line specified by index Addrx-1..z in a direct-mapped cache or way  
Addrx-1..y and index Addry-1..z in a set-associative cache is the chosen line. If the 
specified cache way is not valid (the fourth way of a three way cache), the instruction 
does nothing. In some implementations all ways at index Addry-1..z are invalidated 
regardless of the specified way, but for future compatibility this behavior should not be 
assumed.
');
INSERT INTO "instructions" VALUES('xtensa', 'IITLB', 'Invalidate Instruction TLB Entry
Region Protection Option (see Section 4.6.3 on page 150) or MMU Option (see 
Section 4.6.5 on page 158)
Assembler Syntax
IITLB as
Description
IITLB invalidates the instruction TLB entry specified by the contents of address register 
as. See Section 4.6 on page 138 for information on the address register formats for spe-
cific Memory Protection and Translation options. The point at which the invalidation is 
effected is implementation-specific. Any translation that would be affected by this invali-
dation before the execution of an ISYNC instruction is therefore undefined.
IITLB is a privileged instruction.
The representation of validity in Xtensa TLBs is implementation-specific, and thus the 
operation section below writes the implementation-specific value  
InvalidInstTLBEntry.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
(vpn, ei, wi) ← SplitInstTLBEntrySpec(AR[s])
InstTLB[wi][ei] ← InvalidInstTLBEntry
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'IIU', 'Instruction Cache Index Unlock
imm4
Instruction Cache Index Lock Option (See Section 4.5.4 on page 117)
Assembler Syntax
IIU as, 0..240
Description
IIU uses the virtual address to choose a location in the instruction cache and unlocks 
the chosen line. The purpose of IIU is to remove the lock created by an IPFL instruc-
tion. The method for mapping the virtual address to an instruction cache location is 
implementation-specific. This instruction is primarily useful for unlocking the entire 
instruction cache. Xtensa ISA implementations that do not implement cache locking 
must raise an illegal instruction exception when this opcode is executed.
To unlock a specific cache line if it is in the cache, use the IHU instruction.
IIU forms a virtual address by adding the contents of address register as and a 4-bit 
zero-extended constant value encoded in the instruction word shifted left by four. There-
fore, the offset can specify multiples of 16 from zero to 240. The virtual address chooses 
a cache line without translation and without raising the associated exceptions.
Because the organization of caches is implementation-specific, the operation section 
below specifies only a call to the implementation’s iindexunlock function.
IIU is a privileged instruction.
Assembler Note
To form a virtual address IIU calculates the sum of address register as and the imm4 
field of the instruction word times 16. Therefore, the machine-code offset is in terms of 
instruction by dividing by 16.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
vAddr ← AR[s] + (024||imm4||04)
iindexunlock(vAddr)
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
(cid:132) MemoryErrorException if Memory ECC/Parity Option
Implementation Notes
x ← ceil(log2(InstCacheBytes))
y ← log2(InstCacheBytes ÷ InstCacheWayCount)
z ← log2(InstCacheLineBytes)
The cache line specified by index Addrx-1..z in a direct-mapped cache or way  
Addrx-1..y and index Addry-1..z in a set-associative cache is the chosen line. If the 
specified cache way is not valid (the fourth way of a three way cache), the instruction 
does nothing.
');
INSERT INTO "instructions" VALUES('xtensa', 'ILL', 'Illegal Instruction
Exception Option (See Section 4.4.1 on page 82) 
Assembler Syntax
ILL
Description
ILL is an opcode that is guaranteed to raise an illegal instruction exception in all imple-
mentations. 
Operation
Exception(IllegalInstructionCause)
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(IllegalInstructionCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'ILL.N', 'Narrow Illegal Instruction
Section 4.4.1 on page 82)
Assembler Syntax
ILL.N
Description
ILL.N is a 16-bit opcode that is guaranteed to raise an illegal instruction exception.
Operation
Exception(IllegalInstructionCause)
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(IllegalInstructionCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'IPF', 'Instruction Cache Prefetch
Instruction Cache Option (See Section 4.5.2 on page 115)
Assembler Syntax
IPF as, 0..1020
Description
IPF performs an instruction cache prefetch. The purpose of IPF is to improve perfor-
mance, but not to affect state defined by the ISA. Therefore, some Xtensa ISA imple-
mentations may choose to implement this instruction as a simple “no-operation” instruc-
tion. In general, the performance improvement from using this instruction is 
implementation-dependent. In some implementations, IPF checks whether the line con-
taining the specified address is present in the instruction cache, and if not, it begins the 
transfer of the line from memory to the instruction cache. Prefetching an instruction line 
may prevent the processor from taking an instruction cache miss later.
IPF forms a virtual address by adding the contents of address register as and an 8-bit 
zero-extended constant value encoded in the instruction word shifted left by two. There-
fore, the offset can specify multiples of four from zero to 1020. If the Region Translation 
Option (page 156) or the MMU Option (page 158) is enabled, the virtual address is 
translated to the physical address. If not, the physical address is identical to the virtual 
address. If the translation or memory reference encounters an error (for example, pro-
tection violation, or non-existent memory), the processor performs no operation. This 
allows the instruction to be used to speculatively fetch an address that does not exist or 
is protected without either causing an error or allowing inappropriate action. The transla-
tion is done as if the address were for an instruction fetch.
Assembler Note
To form a virtual address, IPF calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
(pAddr, attributes, cause) ← ftranslate(vAddr, CRING)
if not invalid(attributes) then
iprefetch(vAddr, pAddr, 0)
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'IPFL', 'Instruction Cache Prefetch and Lock
imm4
Instruction Cache Index Lock Option (See Section 4.5.4 on page 117)
Assembler Syntax
IPFL as, 0..240
Description
IPFL performs an instruction cache prefetch and lock. The purpose of IPFL is to 
improve performance, but not to affect state defined by the ISA. Xtensa ISA implementa-
tions that do not implement cache locking must raise an illegal instruction exception 
when this opcode is executed. In general, the performance improvement from using this 
instruction is implementation-dependent as implementations may not overlap the cache 
fill with the execution of other instructions.
In some implementations, IPFL checks whether the line containing the specified 
address is present in the instruction cache, and if not, begins the transfer of the line from 
memory to the instruction cache. The line is placed in the instruction cache and marked 
as locked, so it is not replaceable by ordinary instruction cache misses. To unlock the 
line, use IHU or IIU. To prefetch without locking, use the IPF instruction.
IPFL forms a virtual address by adding the contents of address register as and a 4-bit 
zero-extended constant value encoded in the instruction word shifted left by four. There-
fore, the offset can specify multiples of 16 from zero to 240. If the Region Translation 
Option (page 156) or the MMU Option (page 158) is enabled, the virtual address is 
translated to the physical address. If not, the physical address is identical to the virtual 
address. If the translation encounters an error (for example, protection violation), the 
processor raises one of several exceptions (see Section 4.4.1.5 on page 89). The trans-
lation is done as if the address were for an instruction fetch. If the line cannot be cached, 
an exception is raised with cause InstructionFetchErrorCause.
IPFL is a privileged instruction.
Assembler Note
To form a virtual address, IPFL calculates the sum of address register as and the imm4 
field of the instruction word times 16. Therefore, the machine-code offset is in terms of 
instruction by dividing by 16.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
vAddr ← AR[s] + (024||imm4||04)
(pAddr, attributes, cause) ← ftranslate(vAddr, CRING)
if invalid(attributes) then
EXCVADDR ← vAddr
Exception (cause)
iprefetch(vAddr, pAddr, 1)
else
endif
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
Implementation Notes
If there are not two available InstCache ways at the required index before the instruction 
executes, an exception is raised.
');
INSERT INTO "instructions" VALUES('xtensa', 'ISYNC', 'Instruction Fetch Synchronize
Assembler Syntax
ISYNC
Description
ISYNC waits for all previously fetched load, store, cache, TLB, WSR.*, and XSR.* 
instructions that affect instruction fetch to be performed before fetching the next instruc-
tion. RSYNC, ESYNC, and DSYNC are performed as part of this instruction.
The proper sequence for writing instructions and then executing them is:
(cid:132) write instructions
use DHWB to force the data out of the data cache (this step may be skipped if write-
through, bypass, or no allocate stores were used)
use ISYNC to wait for the writes to be visible to instruction cache misses
use multiple IHI instructions to invalidate the instruction cache for any lines that 
were modified (this step is not appropriate if the affected instructions are in InstRAM 
or cannot be cached)
use ISYNC to ensure that fetch pipeline will see the new instructions
(cid:132)
(cid:132)
(cid:132)
(cid:132)
(cid:132)
(cid:132)
(cid:132)
This instruction also waits for all previously executed WSR.* and XSR.* instructions that 
affect instruction fetch or register access processor state, including:
WSR.LCOUNT, WSR.LBEG, WSR.LEND
WSR.IBREAKENABLE, WSR.IBREAKA[i]
WSR.CCOMPAREn 
See the Special Register Tables in Section 5.3 on page 208 and Section 5.7 on 
page 240, for a complete description of the ISYNC instruction’s uses.
Operation
isync()
Exceptions
(cid:132)
EveryInst Group (see page 244)
Implementation Notes
In many implementations, ISYNC consumes considerably more cycles than RSYNC, 
ESYNC, or DSYNC.
');
INSERT INTO "instructions" VALUES('xtensa', 'J', 'Unconditional Jump
offset
Assembler Syntax
J label
Description
J performs an unconditional branch to the target address. It uses a signed, 18-bit PC-
relative offset to specify the target address. The target address is given by the address 
of the J instruction plus the sign-extended 18-bit offset field of the instruction plus 
four, giving a range of -131068 to +131075 bytes.
Operation
nextPC ← PC + (offset1714||offset) + 4
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'J.L', 'Unconditional Jump Long
offset
Assembler Macro
Assembler Syntax
J.L label, an
Description
J.L is an assembler macro which generates exactly a J instruction as long as the offset 
will reach the label. If the offset is not long enough, the assembler relaxes the instruction 
to a literal load into an followed by a JX an.. The AR register an may or may not be 
modified.
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'JX', 'Unconditional Jump Register
Assembler Syntax
JX as
Description
JX performs an unconditional jump to the address in register as.
Operation
nextPC ← AR[s]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'L8UI', 'Load 8-bit Unsigned
Assembler Syntax
L8UI at, as, 0..255
Description
L8UI is an 8-bit unsigned load from memory. It forms a virtual address by adding the 
contents of address register as and an 8-bit zero-extended constant value encoded in 
the instruction word. Therefore, the offset ranges from 0 to 255. Eight bits (one byte) are 
read from the physical address. This data is then zero-extended and written to address 
register at.
If the Region Translation Option (page 156) or the MMU Option (page 158)is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Operation
vAddr ← AR[s] + (024||imm8)
(mem8, error) ← Load8(vAddr)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
AR[t] ← 024||mem8
else
endif
Exceptions
(cid:132) Memory Group (see page 244)
(cid:132) GenExcep(LoadProhibitedCause) if Region Protection Option or MMU Option
(cid:132) DebugExcep(DBREAK) if Debug Option
');
INSERT INTO "instructions" VALUES('xtensa', 'L16SI', 'Load 16-bit Signed
Assembler Syntax
L16SI at, as, 0..510
Description
L16SI is a 16-bit signed load from memory. It forms a virtual address by adding the con-
tents of address register as and an 8-bit zero-extended constant value encoded in the 
instruction word shifted left by 1. Therefore, the offset can specify multiples of two from 
zero to 510. Sixteen bits (two bytes) are read from the physical address. This data is 
then sign-extended and written to address register at.
If the Region Translation Option (page 156) or the MMU Option (page 158)is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation, non-existent memory), the processor raises one 
of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the least significant address bit is ig-
nored; a reference to an odd address produces the same result as a reference to the ad-
dress minus one. With the Unaligned Exception Option, such an access raises an 
exception.
Assembler Note
To form a virtual address, L16SI calculates the sum of address register as and the 
imm8 field of the instruction word times two. Therefore, the machine-code offset is in 
terms of 16-bit (2 byte) units. However, the assembler expects a byte offset and encodes 
this into the instruction by dividing by two.
Operation
vAddr ← AR[s] + (023||imm8||0)
(mem16, error) ← Load16(vAddr)
');
INSERT INTO "instructions" VALUES('xtensa', 'L16SI', 'Load 16-bit Signed
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
AR[t] ← mem161516||mem16
else
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'L16UI', 'Load 16-bit Unsigned
Assembler Syntax
L16UI at, as, 0..510
Description
L16UI is a 16-bit unsigned load from memory. It forms a virtual address by adding the 
contents of address register as and an 8-bit zero-extended constant value encoded in 
the instruction word shifted left by 1. Therefore, the offset can specify multiples of two 
from zero to 510. Sixteen bits (two bytes) are read from the physical address. This data 
is then zero-extended and written to address register at.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the least significant address bit is ig-
nored; a reference to an odd address produces the same result as a reference to the ad-
dress minus one. With the Unaligned Exception Option, such an access raises an 
exception.
Assembler Note
To form a virtual address, L16UI calculates the sum of address register as and the 
imm8 field of the instruction word times two. Therefore, the machine-code offset is in 
terms of 16-bit (2 byte) units. However, the assembler expects a byte offset and encodes 
this into the instruction by dividing by two.
Operation
vAddr ← AR[s] + (023||imm8||0)
(mem16, error) ← Load16(vAddr)
');
INSERT INTO "instructions" VALUES('xtensa', 'L16UI', 'Load 16-bit Unsigned
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
AR[t] ← 016||mem16
else
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'L32AI', 'Load 32-bit Acquire
Multiprocessor Synchronization Option (See Section 4.3.12 on page 74)
Assembler Syntax
L32AI at, as, 0..1020
Description
L32AI is a 32-bit load from memory with “acquire” semantics. This load performs before 
any subsequent loads, stores, acquires, or releases are performed. It is typically used to 
test a synchronization variable protecting a critical region (for example, to acquire a 
lock).
L32AI forms a virtual address by adding the contents of address register as and an  
Therefore, the offset can specify multiples of four from zero to 1020. 32 bits (four bytes) 
are read from the physical address. This data is then written to address register at. 
L32AI causes the processor to delay processing of subsequent loads, stores, acquires, 
and releases until the L32AI is performed. In some Xtensa ISA implementations, this 
occurs automatically and L32AI is identical to L32I. Other implementations (for exam-
ple, those with multiple outstanding loads and stores) delay processing as described 
above. Because the method of delay is implementation-dependent, this is indicated in 
the operation section below by the implementation function acquire.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
Assembler Note
To form a virtual address, L32AI calculates the sum of address register as and the 
imm8 field of the instruction word times four. Therefore, the machine-code offset is in 
terms of 32-bit (4 byte) units. However, the assembler expects a byte offset and encodes 
this into the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
(mem32, error) ← Load32(vAddr)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
AR[t] ← mem32
acquire()
else
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'L32E', 'Load 32-bit for Window Exceptions
Assembler Syntax
L32E at, as, -64..-4
Description
L32E is a 32-bit load instruction similar to L32I but with semantics required by window 
overflow and window underflow exception handlers. In particular, memory access check-
ing is done with PS.RING instead of CRING, and the offset used to form the virtual ad-
dress is a 4-bit one-extended immediate. Therefore, the offset can specify multiples of 
four from -64 to -4. In configurations without the MMU Option, there is no PS.RING, and 
L32E is similar to L32I with a negative offset.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
L32E is a privileged instruction.
Assembler Note
To form a virtual address, L32E calculates the sum of address register as and the r field 
of the instruction word times four (and one extended). Therefore, the machine-code 
offset is in terms of 32-bit (4 byte) units. However, the assembler expects a byte offset 
and encodes this into the instruction by dividing by four.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
vAddr ← AR[s] + (126||r||02)
ring ← if MMU Option then PS.RING else 0
(mem32, error) ← Load32Ring(vAddr, ring)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
AR[t] ← mem32
else
endif
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'L32I', 'Load 32-bit
Assembler Syntax
L32I at, as, 0..1020
Description
L32I is a 32-bit load from memory. It forms a virtual address by adding the contents of 
address register as and an 8-bit zero-extended constant value encoded in the instruc-
tion word shifted left by two. Therefore, the offset can specify multiples of four from zero 
to 1020. Thirty-two bits (four bytes) are read from the physical address. This data is then 
written to address register at.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation, non-existent memory), the processor raises one 
of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
L32I is one of only a few memory reference instructions that can access instruction 
RAM/ROM.
Assembler Note
The assembler may convert L32I instructions to L32I.N when the Code Density 
Option is enabled and the immediate operand falls within the available range. Prefixing 
the L32I instruction with an underscore (_L32I) disables this optimization and forces 
the assembler to generate the wide form of the instruction.
To form a virtual address, L32I calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
(mem32, error) ← Load32(vAddr)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
AR[t] ← mem32
else
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'L32I.N', 'Narrow Load 32-bit
imm4
Assembler Syntax
L32I.N at, as, 0..60
Description
L32I.N is similar to L32I, but has a 16-bit encoding and supports a smaller range of 
offset values encoded in the instruction word.
L32I.N is a 32-bit load from memory. It forms a virtual address by adding the contents 
of address register as and a 4-bit zero-extended constant value encoded in the instruc-
tion word shifted left by two. Therefore, the offset can specify multiples of four from zero 
to 60. Thirty-two bits (four bytes) are read from the physical address. This data is then 
written to address register at.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
L32I.N is one of only a few memory reference instructions that can access instruction 
RAM/ROM.
Assembler Note
The assembler may convert L32I.N instructions to L32I. Prefixing the L32I.N instruc-
tion with an underscore (_L32I.N) disables this optimization and forces the assembler 
to generate the narrow form of the instruction.
To form a virtual address, L32I.N calculates the sum of address register as and the 
imm4 field of the instruction word times four. Therefore, the machine-code offset is in 
terms of 32-bit (4 byte) units. However, the assembler expects a byte offset and encodes 
this into the instruction by dividing by four.
Operation
vAddr ← AR[s] + (026||imm4||02)
(mem32, error) ← Load32(vAddr)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
AR[t] ← mem32
else
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'L32R', 'Load 32-bit PC-Relative
imm16
Assembler Syntax
L32R at, label
Description
L32R is a PC-relative 32-bit load from memory. It is typically used to load constant 
values into a register when the constant cannot be encoded in a MOVI instruction.
L32R forms a virtual address by adding the 16-bit one-extended constant value encoded 
in the instruction word shifted left by two to the address of the L32R plus three with the 
two least significant bits cleared. Therefore, the offset can always specify 32-bit aligned 
addresses from -262141 to -4 bytes from the address of the L32R instruction. 32 bits 
(four bytes) are read from the physical address. This data is then written to address 
register at.
In the presence of the Extended L32R Option (Section 4.3.3 on page 56) when LIT-
BASE[0] is clear, the instruction has the identical operation. When LITBASE[0] is set, 
L32R forms a virtual address by adding the 16-bit one extended constant value encoded 
in the instruction word shifted left by two to the literal base address indicated by the up-
per 20 bits of LITBASE. The offset can specify 32-bit aligned addresses from -262144 to 
-4 bytes from the literal base address.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
It is not possible to specify an unaligned address.
L32R is one of only a few memory reference instructions that can access instruction 
RAM/ROM.
Assembler Note
In the assembler syntax, the immediate operand is specified as the address of the loca-
tion to load from, rather than the offset from the current instruction address. The linker 
and the assembler both assume that the location loaded by the L32R instruction has not 
been and will not be accessed by any other type of load or store instruction and optimiz-
es according to that assumption.
Operation
vAddr ← (LITBASE31..12||012) + (114||imm16||02)
vAddr ← ((PC + 3)31..2||02) + (114||imm16||02)
if Extended L32R Option and LITBASE0 then
else
endif
(mem32, error) ← Load32(vAddr)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
AR[t] ← mem32
else
endif
Exceptions
(cid:132) Memory Group (see page 244)
(cid:132) GenExcep(LoadProhibitedCause) if Region Protection Option or MMU Option
(cid:132) DebugExcep(DBREAK) if Debug Option
');
INSERT INTO "instructions" VALUES('xtensa', 'LDCT', 'Load Data Cache Tag
Data Cache Test Option (See Section 4.5.6 on page 121)
Assembler Syntax
LDCT at, as
Description
LDCT is not part of the Xtensa Instruction Set Architecture, but is instead specific to an 
implementation. That is, it may not exist in all implementations of the Xtensa ISA.
LDCT is intended for reading the RAM array that implements the data cache tags as part 
of manufacturing test.
LDCT uses the contents of address register as to select a line in the data cache, reads 
the tag associated with this line, and writes the result to address register at. The value 
written to at is described under Cache Tag Format in Section 4.5.1.2 on page 112.
LDCT is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
index ← AR[s]dih..dil
AR[t] ← DataCacheTag[index]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
(cid:132) MemoryErrorException if Memory ECC/Parity Option
Implementation Notes
x ← ceil(log2(DataCacheBytes))
y ← log2(DataCacheBytes ÷ DataCacheWayCount)
z ← log2(DataCacheLineBytes)
LDCT
The cache line specified by index AR[s]x-1..z in a direct-mapped cache or way 
AR[s]x-1..y and index AR[s]y-1..z in a set-associative cache is the chosen line. If the 
specified cache way is not valid (the fourth way of a three way cache), the instruction 
loads an undefined value.
');
INSERT INTO "instructions" VALUES('xtensa', 'LDDEC', 'Load with Autodecrement
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
LDDEC mw, as
Description
LDDEC loads MAC16 register mw from memory using auto-decrement addressing. It 
forms a virtual address by subtracting 4 from the contents of address register as. 32 bits 
(four bytes) are read from the physical address. This data is then written to MAC16 
register mw, and the virtual address is written back to address register as.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
Operation
vAddr ← AR[s] − 4
(mem32, error) ← Load32(vAddr)
if error then
else
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
MR[w] ← mem32
AR[s] ← vAddr
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'LDINC', 'Load with Autoincrement
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
LDINC mw, as
Description
LDINC loads MAC16 register mw from memory using auto-increment addressing. It 
forms a virtual address by adding 4 to the contents of address register as. 32 bits (four 
bytes) are read from the physical address. This data is then written to MAC16 register 
mw, and the virtual address is written back to address register as.
If the Region Translation Option (page 156) or the MMU Option (page 158)is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
Operation
vAddr ← AR[s] + 4
(mem32, error) ← Load32(vAddr)
if error then
else
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
MR[w] ← mem32
AR[s] ← vAddr
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'LICT', 'Load Instruction Cache Tag
Instruction Cache Test Option (See Section 4.5.3 on page 116)
Assembler Syntax
LICT at, as
Description
LICT is not part of the Xtensa Instruction Set Architecture, but is instead specific to an 
implementation. That is, it may not exist in all implementations of the Xtensa ISA.
LICT is intended for reading the RAM array that implements the instruction cache tags 
as part of manufacturing test.
LICT uses the contents of address register as to select a line in the instruction cache, 
reads the tag associated with this line, and writes the result to address register at. The 
value written to at is described under Cache Tag Format in Section 4.5.1.2 on 
page 112.
LICT is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
index ← AR[s]iih..iil
AR[t] ← InstCacheTag[index]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
Implementation Notes
x ← ceil(log2(InstCacheBytes))
y ← log2(InstCacheBytes ÷ InstCacheWayCount)
z ← log2(InstCacheLineBytes)
The cache line specified by index AR[s]x-1..z in a direct-mapped cache or way 
AR[s]x-1..y and index AR[s]y-1..z in a set-associative cache is the chosen line. If the 
specified cache way is not valid (the fourth way of a three way cache), the instruction 
loads an undefined value.
');
INSERT INTO "instructions" VALUES('xtensa', 'LICW', 'Load Instruction Cache Word
Instruction Cache Test Option (See Section 4.5.3 on page 116)
Assembler Syntax
LICW at, as
Description
LICW is not part of the Xtensa Instruction Set Architecture, but is instead specific to an 
implementation. That is, it may not exist in all implementations of the Xtensa ISA.
LICW is intended for reading the RAM array that implements the instruction cache as 
part of manufacturing test.
LICW uses the contents of address register as to select a line in the instruction cache 
and one 32-bit quantity within that line, reads that data, and writes the result to address 
register at.
LICW is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
index ← AR[s]iih..2
AR[t] ← InstCacheData [index]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
(cid:132) MemoryErrorException if Memory ECC/Parity Option
Implementation Notes
x ← ceil(log2(InstCacheBytes))
y ← log2(InstCacheBytes ÷ InstCacheWayCount)
z ← log2(InstCacheLineBytes)
The cache line specified by index AR[s]x-1..z in a direct-mapped cache or way 
AR[s]x-1..y and index AR[s]y-1..z in a set-associative cache is the chosen line. If the 
specified cache way is not valid (the fourth way of a three way cache), the instruction 
loads an undefined value. Within the cache line, AR[s]z-1..2 is used to determine 
which 32-bit quantity within the line is loaded.
');
INSERT INTO "instructions" VALUES('xtensa', 'LOOP', 'Loop
Loop Option (See Section 4.3.2 on page 54)
Assembler Syntax
LOOP as, label
Description
LOOP sets up a zero-overhead loop by setting the LCOUNT, LBEG, and LEND special 
registers, which control instruction fetch. The loop will iterate the number of times speci-
fied by address register as, with 0 causing the loop to iterate 232 times. LCOUNT, the 
current loop iteration counter, is loaded from the contents of address register as minus 
one. LEND is the loop end address and is loaded with the address of the LOOP instruc-
tion plus four, plus the zero-extended 8-bit offset encoded in the instruction (therefore, 
the loop code may be up to 256 bytes in length). LBEG, the loop begin address, is loaded 
with the address of the following instruction (the address of the LOOP instruction plus 
three).
After the processor fetches an instruction that increments the PC to the value contained 
in LEND, and LCOUNT is not zero, it loads the PC with the contents of LBEG and decre-
ments LCOUNT. LOOP is intended to be implemented with help from the instruction fetch 
engine of the processor, and therefore should not incur a mispredict or taken branch 
penalty. Branches and jumps to the address contained in LEND do not cause a loop 
back, and therefore may be used to exit the loop prematurely. Likewise, a return from a 
call instruction as the last instruction of the loop would not trigger loop back; this case 
should be avoided.
There is no mechanism to proceed to the next iteration of the loop from the middle of the 
loop. The compiler may insert a branch to a NOP placed as the last instruction of the loop 
to implement this function if required.
Because LCOUNT, LBEG, and LEND are single registers, zero-overhead loops may not be 
nested. Using conditional branch instructions to implement outer level loops is typically 
not a performance issue. Because loops cannot be nested, it is usually inappropriate to 
include a procedure call inside a loop (the callee might itself use a zero-overhead loop).
To simplify the implementation of zero-overhead loops, the LBEG address, which is the 
LOOP instruction address plus three, must be such that the first instruction must entirely 
fit within a naturally aligned four byte region or, if the instruction is larger than four bytes, 
a naturally aligned region which is the next power of two equal to or larger than the 
instruction. When the LOOP instruction would not naturally be placed at such an 
address, the insertion of NOP instructions or adjustment of which instructions are 16-bit 
density instructions is sufficient to give it the required alignment.
The automatic loop-back when the PC increments to match LEND is disabled when 
PS.EXCM is set. This prevents non-privileged code from affecting the operation of the 
privileged exception vector code.
Assembler Note
The assembler automatically aligns the LOOP instruction as required.
When the label is out of range, the assembler may insert a number of instructions to 
extend the size of the loop. Prefixing the instruction mnemonic with an underscore 
(_LOOP) disables this feature and forces the assembler to generate an error in this case.
Operation
LCOUNT ← AR[s] − 1
LBEG ← PC + 3
LEND ← PC + (024||imm8) + 4
Exceptions
(cid:132)
EveryInstR Group (see page 244)
Implementation Notes
In some implementations, LOOP takes an extra clock for the first loop back of certain 
loops. In addition, certain instructions (such as ISYNC or a write to LEND) may cause an 
additional cycle on the following loop back.
');
INSERT INTO "instructions" VALUES('xtensa', 'LOOPGTZ', 'Loop if Greater Than Zero
Loop Option (See Section 4.3.2 on page 54)
Assembler Syntax
LOOPGTZ as, label
Description
LOOPGTZ sets up a zero-overhead loop by setting the LCOUNT, LBEG, and LEND special 
registers, which control instruction fetch. The loop will iterate the number of times speci-
fied by address register as with values ≤ 0 causing the loop to be skipped altogether by 
branching directly to the loop end address. LCOUNT, the current loop iteration counter, is 
loaded from the contents of address register as minus one. LEND is the loop end 
address and is loaded with the address of the LOOPGTZ instruction plus four, plus the 
zero-extended 8-bit offset encoded in the instruction (therefore, the loop code may be 
up to 256 bytes in length). LBEG, the loop begin address, is loaded with the address of 
the following instruction (the address of the LOOPGTZ instruction plus three). LCOUNT, 
LEND, and LBEG are still loaded even when the loop is skipped.
After the processor fetches an instruction that increments the PC to the value contained 
in LEND, and LCOUNT is not zero, it loads the PC with the contents of LBEG and decre-
ments LCOUNT. LOOPGTZ is intended to be implemented with help from the instruction 
fetch engine of the processor, and therefore should not incur a mispredict or taken 
branch penalty. Branches and jumps to the address contained in LEND do not cause a 
loop back, and therefore may be used to exit the loop prematurely. Similarly, a return 
from a call instruction as the last instruction of the loop would not trigger loop back; this 
case should be avoided.
There is no mechanism to proceed to the next iteration of the loop from the middle of the 
loop. The compiler may insert a branch to a NOP placed as the last instruction of the loop 
to implement this function if required.
Because LCOUNT, LBEG, and LEND are single registers, zero-overhead loops may not be 
nested. Using conditional branch instructions to implement outer level loops is typically 
not a performance issue. Because loops cannot be nested, it is usually inappropriate to 
include a procedure call inside a loop (the callee might itself use a zero-overhead loop).
To simplify the implementation of zero-overhead loops, the LBEG address, which is the 
LOOP instruction address plus three, must be such that the first instruction must entirely 
fit within a naturally aligned four byte region or, if the instruction is larger than four bytes, 
a naturally aligned region which is the next power of two equal to or larger than the 
instruction. When the LOOP instruction would not naturally be placed at such an 
address, the insertion of NOP instructions or adjustment of which instructions are 16-bit 
density instructions is sufficient to give it the required alignment.
The automatic loop-back when the PC increments to match LEND is disabled when 
PS.EXCM is set. This prevents non-privileged code from affecting the operation of the 
privileged exception vector code.
Assembler Note
The assembler automatically aligns the LOOPGTZ instruction as required.
When the label is out of range, the assembler may insert a number of instructions to 
extend the size of the loop. Prefixing the instruction mnemonic with an underscore 
(_LOOPGTZ) disables this feature and forces the assembler to generate an error in this 
case.
Operation
LCOUNT ← AR[s] − 1
LBEG ← PC + 3
LEND ← PC + (024||imm8) + 4
if AR[s] ≤ 032 then
endif
Exceptions
nextPC ← PC + (024||imm8) + 4
(cid:132)
EveryInstR Group (see page 244)
Implementation Notes
In some implementations, LOOPGTZ takes an extra clock for the first loop back of certain 
loops. In addition, certain instructions (such as ISYNC or a write to LEND) may cause an 
additional cycle on the following loop back.
');
INSERT INTO "instructions" VALUES('xtensa', 'LOOPNEZ', 'Loop if Not-Equal Zero
Loop Option (See Section 4.3.2 on page 54)
Assembler Syntax
LOOPNEZ as, label
Description
LOOPNEZ sets up a zero-overhead loop by setting the LCOUNT, LBEG, and LEND special 
registers, which control instruction fetch. The loop will iterate the number of times speci-
fied by address register as with the zero value causing the loop to be skipped altogether 
by branching directly to the loop end address. LCOUNT, the current loop iteration 
counter, is loaded from the contents of address register as minus 1. LEND is the loop 
end address and is loaded with the address of the LOOPNEZ instruction plus four plus 
the zero-extended 8-bit offset encoded in the instruction (therefore, the loop code may 
be up to 256 bytes in length). LBEG is loaded with the address of the following instruc-
tion (the address of the LOOPNEZ instruction plus three). LCOUNT, LEND, and LBEG are 
still loaded even when the loop is skipped.
After the processor fetches an instruction that increments the PC to the value contained 
in LEND, and LCOUNT is not zero, it loads the PC with the contents of LBEG and decre-
ments LCOUNT. LOOPNEZ is intended to be implemented with help from the instruction 
fetch engine of the processor, and therefore should not incur a mispredict or taken 
branch penalty. Branches and jumps to the address contained in LEND do not cause a 
loop back, and therefore may be used to exit the loop prematurely. Similarly a return 
from a call instruction as the last instruction of the loop would not trigger loop back; this 
case should be avoided.
There is no mechanism to proceed to the next iteration of the loop from the middle of the 
loop. The compiler may insert a branch to a NOP placed as the last instruction of the loop 
to implement this function if required.
Because LCOUNT, LBEG, and LEND are single registers, zero-overhead loops may not be 
nested. Using conditional branch instructions to implement outer level loops is typically 
not a performance issue. Because loops cannot be nested, it is usually inappropriate to 
include a procedure call inside a loop (the callee might itself use a zero-overhead loop).
To simplify the implementation of zero-overhead loops, the LBEG address, which is the 
LOOP instruction address plus three, must be such that the first instruction must entirely 
fit within a naturally aligned four byte region or, if the instruction is larger than four bytes, 
a naturally aligned region which is the next power of two equal to or larger than the 
instruction. When the LOOP instruction would not naturally be placed at such an ad-
dress, the insertion of NOP instructions or adjustment of which instructions are 16-bit 
density instructions is sufficient to give it the required alignment.
The automatic loop-back when the PC increments to match LEND is disabled when 
PS.EXCM is set. This prevents non-privileged code from affecting the operation of the 
privileged exception vector code.
Assembler Note
The assembler automatically aligns the LOOPNEZ instruction as required.
When the label is out of range, the assembler may insert a number of instructions to 
extend the size of the loop. Prefixing the instruction mnemonic with an underscore 
(_LOOPNEZ) disables this feature and forces the assembler to generate an error in this 
case.
Operation
LCOUNT ← AR[s] − 1
LBEG ← PC + 3
LEND ← PC + (024||imm8) + 4)
if AR[s] = 032 then
endif
Exceptions
nextPC ← PC + (024||imm8) + 4
(cid:132)
EveryInstR Group (see page 244)
Implementation Notes
In some implementations, LOOPNEZ takes an extra clock for the first loop back of certain 
loops. In addition, certain instructions (such as ISYNC or a write to LEND) may cause an 
additional cycle on the following loop back.
');
INSERT INTO "instructions" VALUES('xtensa', 'LSI', 'Load Single Immediate
Assembler Syntax
LSI ft, as, 0..1020
Description
LSI is a 32-bit load from memory to the floating-point register file. It forms a virtual ad-
dress by adding the contents of address register as and an 8-bit zero-extended constant 
value encoded in the instruction word shifted left by two. Therefore, the offset can spec-
ify multiples of four from zero to 1020. Thirty-two bits (four bytes) are read from the 
physical address. This data is then written to floating-point register ft.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
Assembler Note
To form a virtual address, LSI calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
(mem32, error) ← Load32(vAddr)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
FR[t] ← mem32
else
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'LSIU', 'Load Single Immediate with Update
Assembler Syntax
LSIU ft, as, 0..1020
Description
LSIU is a 32-bit load from memory to the floating-point register file with base address 
register update. It forms a virtual address by adding the contents of address register as 
and an 8-bit zero-extended constant value encoded in the instruction word shifted left by 
two. Therefore, the offset can specify multiples of four from zero to 1020. Thirty-two bits 
(four bytes) are read from the physical address. This data is then written to floating-point 
register ft and the virtual address is written back to address register as.
If the Region Translation Option (page 156) or the MMU Option (page 158)is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
Assembler Note
To form a virtual address, LSIU calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
(mem32, error) ← Load32(vAddr)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
FR[t] ← mem32
AS[s] ← vAddr
else
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'LSX', 'Load Single Indexed
Assembler Syntax
LSX fr, as, at
Description
LSX is a 32-bit load from memory to the floating-point register file. It forms a virtual 
address by adding the contents of address register as and the contents of address 
register at. 32 bits (four bytes) are read from the physical address. This data is then 
written to floating-point register fr.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
Operation
vAddr ← AR[s] + (AR[t])
(mem32, error) ← Load32(vAddr)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
FR[r] ← mem32
else
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'LSXU', 'Load Single Indexed with Update
Assembler Syntax
LSXU fr, as, at
Description
LSXU is a 32-bit load from memory to the floating-point register file with base address 
register update. It forms a virtual address by adding the contents of address register as 
and the contents of address register at. 32 bits (four bytes) are read from the physical 
address. This data is then written to floating-point register fr and the virtual address is 
written back to address register as.
If the Region Translation Option (page 156) or the MMU Option (page 158)is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
Operation
vAddr ← AR[s] + (AR[t])
(mem32, error) ← Load32(vAddr)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
FR[r] ← mem32
AR[s] ← vAddr
else
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'MADD.S', 'Multiply and Add Single
Assembler Syntax
MADD.S fr, fs, ft
Description
Using IEEE754 single-precision arithmetic, MADD.S multiplies the contents of floating-
point registers fs and ft, adds the product to the contents of floating-point register fr, 
and then writes the sum back to floating-point register fr. The computation is performed 
with no intermediate round.
Operation
FR[r] ← FR[r] +s (FR[s] ×s FR[t]) (×s does not round)
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'MAX', 'Maximum Value
Miscellaneous Operations Option (See Section 4.3.8 on page 62)
Assembler Syntax
MAX ar, as, at
Description
MAX computes the maximum of the twos complement contents of address registers as 
and at and writes the result to address register ar.
Operation
AR[r] ← if AR[s] < AR[t] then AR[t] else AR[s]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MAXU', 'Maximum Value Unsigned
Miscellaneous Operations Option (See Section 4.3.8 on page 62)
Assembler Syntax
MAXU ar, as, at
Description
MAXU computes the maximum of the unsigned contents of address registers as and at 
and writes the result to address register ar.
Operation
AR[r] ← if (0||AR[s]) < (0||AR[t]) then AR[t] else AR[s]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MEMW', 'Memory Wait
Assembler Syntax
MEMW
Description
MEMW ensures that all previous load, store, acquire, release, prefetch, and cache instruc-
tions perform before performing any subsequent load, store, acquire, release, prefetch, 
or cache instructions. MEMW is intended to implement the volatile attribute of lan-
guages such as C and C++. The compiler should separate all volatile loads and 
stores with a MEMW instruction. ISYNC should be used to cause instruction fetches to 
wait as MEMW will have no effect on them.
On processor/system implementations that always reference memory in program order, 
MEMW may be a no-op. Implementations that reorder load, store, or cache instructions, or 
which perform merging of stores (for example, in a write buffer) must order such memo-
ry references so that all memory references executed before MEMW are performed before 
any memory references that are executed after MEMW.
Because the instruction execution pipeline is implementation-specific, the operation 
section below specifies only a call to the implementation’s memw function.
Operation
memw()
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MIN', 'Minimum Value
Miscellaneous Operations Option (See Section 4.3.8 on page 62)
Assembler Syntax
MIN ar, as, at
Description
MIN computes the minimum of the twos complement contents of address registers as 
and at and writes the result to address register ar.
Operation
AR[r] ← if AR[s] < AR[t] then AR[s] else AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MINU', 'Minimum Value Unsigned
Miscellaneous Operations Option (See Section 4.3.8 on page 62)
Assembler Syntax
MINU ar, as, at
Description
MINU computes the minimum of the unsigned contents of address registers as and at, 
and writes the result to address register ar.
Operation
AR[r] ← if (0||AR[s]) < (0||AR[t]) then AR[s] else AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MOV', 'Move
Assembler Macro
Assembler Syntax
MOV ar, as
Description
MOV is an assembler macro that uses the OR instruction (page 466) to move the contents 
of address register as to address register ar. The assembler input
MOV ar, as
expands into
OR  ar, as, as
ar and as should not specify the same register due to the MOV.N restriction.
Assembler Note
The assembler may convert MOV instructions to MOV.N when the Code Density Option is 
enabled. Prefixing the MOV instruction with an underscore (_MOV) disables this optimiza-
tion and forces the assembler to generate the OR form of the instruction.
Operation
AR[r] ← AR[s]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MOV.N', 'Narrow Move
Assembler Syntax
MOV.N at, as
Description
MOV.N is similar in function to the assembler macro MOV, but has a 16-bit encoding. 
MOV.N moves the contents of address register as to address register at.
The operation of the processor when at and as specify the same register is undefined 
and reserved for future use.
Assembler Note
The assembler may convert MOV.N instructions to MOV. Prefixing the MOV.N instruction 
with an underscore (_MOV.N) disables this optimization and forces the assembler to 
generate the narrow form of the instruction.
Operation
AR[t] ← AR[s]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MOV.S', 'Move Single
Assembler Syntax
MOV.S fr, fs
Description
MOV.S moves the contents of floating-point register fs to floating-point register fr. The 
move is non-arithmetic; no floating-point exceptions are raised.
Operation
FR[r] ← FR[s]
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVEQZ', 'Move if Equal to Zero
Assembler Syntax
MOVEQZ ar, as, at
Description
MOVEQZ performs a conditional move if equal to zero. If the contents of address register 
at are zero, then the processor sets address register ar to the contents of address reg-
ister as. Otherwise, MOVEQZ performs no operation and leaves address register ar 
unchanged.
The inverse of MOVEQZ is MOVNEZ.
Operation
if AR[t] = 032 then
AR[r] ← AR[s]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVEQZ.S', 'Move Single if Equal to Zero
Assembler Syntax
MOVEQZ.S fr, fs, at
Description
MOVEQZ.S is a conditional move between floating-point registers based on the value in 
an address register. If address register at contains zero, the contents of floating-point 
register fs are written to floating-point register fr. MOVEQZ.S is non-arithmetic; no 
floating-point exceptions are raised.
The inverse of MOVEQZ.S is MOVNEZ.S.
Operation
if AR[t] = 032 then
FR[r] ← FR[s]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVF', 'Move if False
Assembler Syntax
MOVF ar, as, bt
Description
MOVF moves the contents of address register as to address register ar if Boolean regis-
ter bt is false. Address register ar is left unchanged if Boolean register bt is true.
The inverse of MOVF is MOVT.
Operation
AR[r] ← AR[s]
if not BRt then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVF.S', 'Move Single if False
Assembler Syntax
MOVF.S fr, fs, bt
Description
MOVF.S is a conditional move between floating-point registers based on the value in a 
Boolean register. If Boolean register bt contains zero, the contents of floating-point reg-
ister fs are written to floating-point register fr. MOVF.S is non-arithmetic; no floating-
point exceptions are raised.
The inverse of MOVF.S is MOVT.S.
Operation
FR[r] ← FR[s]
if not BRt then
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVGEZ', 'Move if Greater Than or Equal to Zero
Assembler Syntax
MOVGEZ ar, as, at
Description
MOVGEZ performs a conditional move if greater than or equal to zero. If the contents of 
address register at are greater than or equal to zero (that is, the most significant bit is 
clear), then the processor sets address register ar to the contents of address register 
as. Otherwise, MOVGEZ performs no operation and leaves address register ar 
unchanged.
The inverse of MOVGEZ is MOVLTZ.
Operation
if AR[t]31 = 0 then
AR[r] ← AR[s]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'Move Single if Greater Than or Eq Zero', 'MOVGEZ.S 
Assembler Syntax
MOVGEZ.S fr, fs, at
Description
MOVGEZ.S is a conditional move between floating-point registers based on the value in 
an address register. If the contents of address register at is greater than or equal to 
zero (that is, the most significant bit is clear), the contents of floating-point register fs 
are written to floating-point register fr. MOVGEZ.S is non-arithmetic; no floating-point 
exceptions are raised.
The inverse of MOVGEZ.S is MOVLTZ.S.
Operation
if AR[t]31 = 0 then
FR[r] ← FR[s]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVI', 'Move Immediate
imm12b7..0
imm12b11..8
Assembler Syntax
MOVI at, -2048..2047
Description
MOVI sets address register at to a constant in the range -2048..2047 encoded in the 
instruction word. The constant is stored in two non-contiguous fields of the instruction 
word. The processor decodes the constant specification by concatenating the two fields 
and sign-extending the 12-bit value.
Assembler Note
The assembler will convert MOVI instructions into a literal load when given an immediate 
operand that evaluates to a value outside the range -2048..2047. The assembler will 
convert MOVI instructions to MOVI.N when the Code Density Option is enabled and the 
immediate operand falls within the available range. Prefixing the MOVI instruction with 
an underscore (_MOVI) disables these features and forces the assembler to generate 
an error for the first case and the wide form of the instruction for the second case.
Operation
AR[t] ← imm121120||imm12
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVI.N', 'Narrow Move Immediate
imm73..0
imm76..4
Assembler Syntax
MOVI.N as, -32..95
Description
MOVI.N is similar to MOVI, but has a 16-bit encoding and supports a smaller range of 
constant values encoded in the instruction word.
MOVI.N sets address register as to a constant in the range -32..95 encoded in the 
instruction word. The constant is stored in two non-contiguous fields of the instruction 
word. The range is asymmetric around zero because positive constants are more fre-
quent than negative constants. The processor decodes the constant specification by 
concatenating the two fields and sign-extending the 7-bit value with the logical and of its 
two most significant bits.
Assembler Note
The assembler may convert MOVI.N instructions to MOVI. Prefixing the MOVI.N instruc-
tion with an underscore (_MOVI.N) disables this optimization and forces the assembler 
to generate the narrow form of the instruction.
Operation
AR[s] ← (imm76 and imm75)25||imm7
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVLTZ', 'Move if Less Than Zero
Assembler Syntax
MOVLTZ ar, as, at
Description
MOVLTZ performs a conditional move if less than zero. If the contents of address register 
at are less than zero (that is, the most significant bit is set), then the processor sets ad-
dress register ar to the contents of address register as. Otherwise, MOVLTZ performs 
no operation and leaves address register ar unchanged.
The inverse of MOVLTZ is MOVGEZ.
Operation
if AR[t]31 ≠ 0 then
AR[r] ← AR[s]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVLTZ.S', 'Move Single if Less Than Zero
Assembler Syntax
MOVLTZ.S fr, fs, at
Description
MOVLTZ.S is a conditional move between floating-point registers based on the value in 
an address register. If the contents of address register at is less than zero (that is, the 
most significant bit is set), the contents of floating-point register fs are written to float-
ing-point register fr. MOVLTZ.S is non-arithmetic; no floating-point exceptions are 
raised.
The inverse of MOVLTZ.S is MOVGEZ.S.
Operation
if AR[t]31 ≠ 0 then
FR[r] ← FR[s]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVNEZ', 'Move if Not-Equal to Zero
Assembler Syntax
MOVNEZ ar, as, at
Description
MOVNEZ performs a conditional move if not equal to zero. If the contents of address reg-
ister at are non-zero, then the processor sets address register ar to the contents of ad-
dress register as. Otherwise, MOVNEZ performs no operation and leaves address 
register ar unchanged.
The inverse of MOVNEZ is MOVEQZ.
Operation
if AR[t] ≠ 032 then
AR[r] ← AR[s]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVNEZ.S', 'Move Single if Not Equal to Zero
Assembler Syntax
MOVNEZ.S fr, fs, at
Description
MOVNEZ.S is a conditional move between floating-point registers based on the value in 
an address register. If the contents of address register at is non-zero, the contents of 
floating-point register fs are written to floating-point register fr. MOVNEZ.S is non-arith-
metic; no floating-point exceptions are raised.
The inverse of MOVNEZ.S is MOVEQZ.S.
Operation
if AR[t] ≠ 032 then
FR[r] ← FR[s]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVSP', 'Move to Stack Pointer
Assembler Syntax
MOVSP at, as
Description
MOVSP provides an atomic window check and register-to-register move. If the caller’s 
registers are present in the register file, this instruction simply moves the contents of 
address register as to address register at. If the caller’s registers are not present, 
MOVSP raises an Alloca exception.
MOVSP is typically used to perform variable-size stack frame allocation. The Xtensa ABI 
specifies that the caller’s a0-a3 may be stored just below the callee’s stack pointer. 
When the stack frame is extended, these values may need to be moved. They can only 
be moved with interrupts and exceptions disabled. This instruction provides a mecha-
nism to test if they must be moved, and if so, to raise an exception to move the data with 
interrupts and exceptions disabled. The Xtensa ABI also requires that the caller’s return 
address be in a0 when MOVSP is executed.
Operation
Exception (AllocaCause)
AR[t] ← AR[s]
if WindowStartWindowBase-0011..WindowBase-0001 = 03 then
else
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(AllocaCause) if Windowed Register Option
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVT', 'Move if True
Assembler Syntax
MOVT ar, as, bt
Description
MOVT moves the contents of address register as to address register ar if Boolean regis-
ter bt is true. Address register ar is left unchanged if Boolean register bt is false.
The inverse of MOVT is MOVF.
Operation
AR[r] ← AR[s]
if BRt then
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MOVT.S', 'Move Single if True
Assembler Syntax
MOVT.S fr, fs, bt
Description
MOVT.S is a conditional move between floating-point registers based on the value in a 
Boolean register. If Boolean register bt is set, the contents of floating-point register fs 
are written to floating-point register fr. MOVT.S is non-arithmetic; no floating-point 
exceptions are raised.
The inverse of MOVT.S is MOVF.S.
Operation
FR[r] ← FR[s]
if BRt then
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'MSUB.S', 'Multiply and Subtract Single
Assembler Syntax
MSUB.S fr, fs, ft
Description
MSUB.S multiplies the contents of floating-point registers fs and ft, subtracts the prod-
uct from the contents of floating-point register fr, and then writes the difference back to 
floating-point register fr. The computation is performed with no intermediate round.
Operation
FR[r] ← FR[r] −s (FR[s] ×s FR[t]) (×s does not round)
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'MUL.AA.*', 'Signed Multiply
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MUL.AA.* as, at
Where * expands as follows:
MUL.AA.LL - for (half=0)
MUL.AA.HL - for (half=1)
MUL.AA.LH - for (half=2)
MUL.AA.HH - for (half=3)
Description
MUL.AA.* performs a two’s complement multiply of half of each of the address registers 
as and at, producing a 32-bit result. The result is sign-extended to 40 bits and written to 
the MAC16 accumulator.
Operation
m1 ← if half0 then AR[s]31..16 else AR[s]15..0
m2 ← if half1 then AR[t]31..16 else AR[t]15..0
ACC ← (m11524||m1) × (m21524||m2)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MUL.AD.*', 'Signed Multiply
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MUL.AD.* as, my
Where * expands as follows:
MUL.AD.LL - for (half=0)
MUL.AD.HL - for (half=1)
MUL.AD.LH - for (half=2)
MUL.AD.HH - for (half=3)
Description
MUL.AD.* performs a two’s complement multiply of half of address register as and half 
of MAC16 register my, producing a 32-bit result. The result is sign-extended to 40 bits 
and written to the MAC16 accumulator. The my operand can designate either MAC16 
register m2 or m3.
Operation
m1 ← if half0 then AR[s]31..16 else AR[s]15..0
m2 ← if half1 then MR[1||y]31..16 else MR[1||y]15..0
ACC ← (m11524||m1) × (m21524||m2)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MUL.DA.*', 'Signed Multiply
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MUL.DA.* mx, at
Where * expands as follows:
MUL.DA.LL - for (half=0)
MUL.DA.HL - for (half=1)
MUL.DA.LH - for (half=2)
MUL.DA.HH - for (half=3)
Description
MUL.DA.* performs a two’s complement multiply of half of MAC16 register mx and half 
of address register at, producing a 32-bit result. The result is sign-extended to 40 bits 
and written to the MAC16 accumulator. The mx operand can designate either MAC16 
register m0 or m1.
Operation
m1 ← if half0 then MR[0||x]31..16 else MR[0||x]15..0
m2 ← if half1 then AR[t]31..16 else AR[t]15..0
ACC ← (m11524||m1) × (m21524||m2)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MUL.DD.*', 'Signed Multiply
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MUL.DD.* mx, my
Where * expands as follows:
MUL.DD.LL - for (half=0)
MUL.DD.HL - for (half=1) 
MUL.DD.LH - for (half=2) 
MUL.DD.HH - for (half=3) 
Description
MUL.DD.* performs a two’s complement multiply of half of the MAC16 registers mx and 
my, producing a 32-bit result. The result is sign-extended to 40 bits and written to the 
MAC16 accumulator. The mx operand can designate either MAC16 register m0 or m1. 
The my operand can designate either MAC16 register m2 or m3.
Operation
m1 ← if half0 then MR[0||x]31..16 else MR[0||x]15..0
m2 ← if half1 then MR[1||y]31..16 else MR[1||y]15..0
ACC ← (m11524||m1) × (m21524||m2)
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MUL.S', 'Multiply Single
Assembler Syntax
MUL.S fr, fs, ft
Description
MUL.S computes the IEEE754 single-precision product of the contents of floating-point 
registers fs and ft and writes the result to floating-point register fr.
Operation
FR[r] ← FR[s] ×s FR[t]
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'MUL16S', 'Multiply 16-bit Signed
Assembler Syntax
MUL16S ar, as, at
Description
MUL16S performs a two’s complement multiplication of the least-significant 16 bits of the 
contents of address registers as and at and writes the 32-bit product to address regis-
ter ar.
Operation
AR[r] ← (AR[s]1516||AR[s]15..0) × (AR[t]1516||AR[t]15..0)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MUL16U', 'Multiply 16-bit Unsigned
Assembler Syntax
MUL16U ar, as, at
Description
MUL16U performs an unsigned multiplication of the least-significant 16 bits of the con-
tents of address registers as and at and writes the 32-bit product to address register 
ar.
Operation
AR[r] ← (016||AR[s]15..0) × (016||AR[t]15..0)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULA.AA.*', 'Signed Multiply/Accumulate
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MULA.AA.* as, at
Where * expands as follows:
MULA.AA.LL - for (half=0) 
MULA.AA.HL - for (half=1) 
MULA.AA.LH - for (half=2) 
MULA.AA.HH - for (half=3) 
Description
MULA.AA.* performs a two’s complement multiply of half of each of the address regis-
ters as and at, producing a 32-bit result. The result is sign-extended to 40 bits and add-
ed to the contents of the MAC16 accumulator.
Operation
m1 ← if half0 then AR[s]31..16 else AR[s]15..0
m2 ← if half1 then AR[t]31..16 else AR[t]15..0
ACC ← ACC + (m11524||m1) × (m21524||m2)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULA.AD.*', 'Signed Multiply/Accumulate
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MULA.AD.* as, my
Where * expands as follows:
MULA.AD.LL - for (half=0) 
MULA.AD.HL - for (half=1) 
MULA.AD.LH - for (half=2) 
MULA.AD.HH - for (half=3) 
Description
MULA.AD.* performs a two’s complement multiply of half of address register as and 
half of MAC16 register my, producing a 32-bit result. The result is sign-extended to 40 
bits and added to the contents of the MAC16 accumulator. The my operand can desig-
nate either MAC16 register m2 or m3.
Operation
m1 ← if half0 then AR[s]31..16 else AR[s]15..0
m2 ← if half1 then MR[1||y]31..16 else MR[1||y]15..0
ACC ← ACC + (m11524||m1) × (m21524||m2)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULA.DA.*', 'Signed Multiply/Accumulate
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MULA.DA.* mx, at
Where * expands as follows:
MULA.DA.LL - for (half=0) 
MULA.DA.HL - for (half=1) 
MULA.DA.LH - for (half=2) 
MULA.DA.HH - for (half=3) 
Description
MULA.DA.* performs a two’s complement multiply of half of MAC16 register mx and half 
of address register at, producing a 32-bit result. The result is sign-extended to 40 bits 
and added to the contents of the MAC16 accumulator. The mx operand can designate 
either MAC16 register m0 or m1.
Operation
m1 ← if half0 then MR[0||x]31..16 else MR[0||x]15..0
m2 ← if half1 then AR[t]31..16 else AR[t]15..0
ACC ← ACC + (m11524||m1) × (m21524||m2)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULA.DA.*.LDDEC', 'Signed Mult/Accum, Ld/Autodec 
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MULA.DA.*.LDDEC mw, as, mx, at
Where * expands as follows:
MULA.DA.LL.LDDEC - for (half=0) 
MULA.DA.HL.LDDEC - for (half=1) 
MULA.DA.LH.LDDEC - for (half=2) 
MULA.DA.HH.LDDEC - for (half=3) 
Description
MULA.DA.*.LDDEC performs a parallel load and multiply/accumulate.
First, it performs a two’s complement multiply of half of MAC16 register mx and half of 
address register at, producing a 32-bit result. The result is sign-extended to 40 bits and 
added to the contents of the MAC16 accumulator. The mx operand can designate either 
MAC16 register m0 or m1.
Next, it loads MAC16 register mw from memory using auto-decrement addressing. It 
forms a virtual address by subtracting 4 from the contents of address register as. Thirty-
two bits (four bytes) are read from the physical address. This data is then written to 
MAC16 register mw, and the virtual address is written back to address register as. The 
mw operand can designate any of the four MAC16 registers.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
The MAC16 register source mx and the MAC16 register destination mw may be the 
same. In this case, the instruction uses the contents of mx as the source operand prior to 
loading mx with the load data.
Operation
vAddr ← AR[s] − 4
(mem32, error) ← Load32(vAddr)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
m1 ← if half0 then MR[0||x]31..16 else MR[0||x]15..0
m2 ← if half1 then AR[t]31..16 else AR[t]15..0
ACC ← ACC + (m11524||m1) × (m21524||m2)
AR[s] ← vAddr
MR[w] ← mem32
else
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULA.DA.*.LDINC', 'Signed Mult/Accum, Ld/Autoinc 
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MULA.DA.*.LDINC mw, as, mx, at
Where * expands as follows:
MULA.DA.LL.LDINC - for (half=0) 
MULA.DA.HL.LDINC - for (half=1) 
MULA.DA.LH.LDINC - for (half=2) 
MULA.DA.HH.LDINC - for (half=3) 
Description
MULA.DA.*.LDINC performs a parallel load and multiply/accumulate.
First, it performs a two’s complement multiply of half of MAC16 register mx and half of 
address register at, producing a 32-bit result. The result is sign-extended to 40 bits and 
added to the contents of the MAC16 accumulator. The mx operand can designate either 
MAC16 register m0 or m1.
Next, it loads MAC16 register mw from memory using auto-increment addressing. It 
forms a virtual address by adding 4 to the contents of address register as. 32 bits (four 
bytes) are read from the physical address. This data is then written to MAC16 register 
mw, and the virtual address is written back to address register as. The mw operand can 
designate any of the four MAC16 registers.
If the Region Translation Option (page 156) or the MMU Option (page 158)is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
The MAC16 register source mx and the MAC16 register destination mw may be the 
same. In this case, the instruction uses the contents of mx as the source operand prior to 
loading mx with the load data.
Operation
vAddr ← AR[s] + 4
(mem32, error) ← Load32(vAddr)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
m1 ← if half0 then MR[0||x]31..16 else MR[0||x]15..0
m2 ← if half1 then AR[t]31..16 else AR[t]15..0
ACC ← ACC + (m11524||m1) × (m21524||m2)
AR[s] ← vAddr
MR[w] ← mem32
else
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULA.DD.*', 'Signed Multiply/Accumulate
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MULA.DD.* mx, my
Where * expands as follows:
MULA.DD.LL - for (half=0) 
MULA.DD.HL - for (half=1) 
MULA.DD.LH - for (half=2) 
MULA.DD.HH - for (half=3) 
Description
MULA.DD.* performs a two’s complement multiply of half of each of the MAC16 regis-
ters mx and my, producing a 32-bit result. The result is sign-extended to 40 bits and add-
ed to the contents of the MAC16 accumulator. The mx operand can designate either 
MAC16 register m0 or m1. The my operand can designate either MAC16 register m2 or 
m3.
Operation
m1 ← if half0 then MR[0||x]31..16 else MR[0||x]15..0
m2 ← if half1 then MR[1||y]31..16 else MR[1||y]15..0
ACC ← ACC + (m11524||m1) × (m21524||m2)
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULA.DD.*.LDDEC', 'Signed Mult/Accum, Ld/Autodec
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MULA.DD.*.LDDEC mw, as, mx, my
Where * expands as follows:
MULA.DD.LL.LDDEC - for (half=0) 
MULA.DD.HL.LDDEC - for (half=1) 
MULA.DD.LH.LDDEC - for (half=2) 
MULA.DD.HH.LDDEC - for (half=3) 
Description
MULA.DD.*.LDDEC performs a parallel load and multiply/accumulate.
First, it performs a two’s complement multiply of half of the MAC16 registers mx and my, 
producing a 32-bit result. The result is sign-extended to 40 bits and added to the con-
tents of the MAC16 accumulator. The mx operand can designate either MAC16 register 
m0 or m1. The my operand can designate either MAC16 register m2 or m3.
Next, it loads MAC16 register mw from memory using auto-decrement addressing. It 
forms a virtual address by subtracting 4 from the contents of address register as. Thirty-
two bits (four bytes) are read from the physical address. This data is then written to 
MAC16 register mw, and the virtual address is written back to address register as. The 
mw operand can designate any of the four MAC16 registers.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
The MAC16 register destination mw may be the same as either MAC16 register source 
mx or my. In this case, the instruction uses the contents of mx and my as the source oper-
ands prior to loading mw with the load data.
Operation
vAddr ← AR[s] − 4
(mem32, error) ← Load32(vAddr)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
m1 ← if half0 then MR[0||x]31..16 else MR[0||x]15..0
m2 ← if half1 then MR[1||y]31..16 else MR[1||y]15..0
ACC ← ACC + (m11524||m1) × (m21524||m2)
AR[s] ← vAddr
MR[w] ← mem32
else
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULA.DD.*.LDINC', 'Signed Mult/Accum, Ld/Autoinc
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MULA.DD.*.LDINC mw, as, mx, my
Where * expands as follows:
MULA.DD.LL.LDINC - for (half=0) 
MULA.DD.HL.LDINC - for (half=1) 
MULA.DD.LH.LDINC - for (half=2) 
MULA.DD.HH.LDINC - for (half=3) 
Description
MULA.DD.*.LDINC performs a parallel load and multiply/accumulate.
First, it performs a two’s complement multiply of half of each of the MAC16 registers mx 
and my, producing a 32-bit result. The result is sign-extended to 40 bits and added to the 
contents of the MAC16 accumulator. The mx operand can designate either MAC16 reg-
ister m0 or m1. The my operand can designate either MAC16 register m2 or m3.
Next, it loads MAC16 register mw from memory using auto-increment addressing. It 
forms a virtual address by adding 4 to the contents of address register as. Thirty-two 
bits (four bytes) are read from the physical address. This data is then written to MAC16 
register mw, and the virtual address is written back to address register as. The mw oper-
and can designate any of the four MAC16 registers.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
The MAC16 register destination mw may be the same as either MAC16 register source 
mx or my. In this case, the instruction uses the contents of mx and my as the source 
operands prior to loading mw with the load data.
Operation
vAddr ← AR[s] + 4
(mem32, error) ← Load32(vAddr)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreErrorCause)
m1 ← if half0 then MR[0||x]31..16 else MR[0||x]15..0
m2 ← if half1 then MR[1||y]31..16 else MR[1||y]15..0
ACC ← ACC + (m11524||m1) × (m21524||m2)
AR[s] ← vAddr
MR[w] ← mem32
else
endif
Exceptions
(cid:132) Memory Load Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULL', 'Multiply Low
Assembler Syntax
MULL ar, as, at
Description
MULL performs a 32-bit multiplication of the contents of address registers as and at, 
and writes the least significant 32 bits of the product to address register ar. Because the 
least significant product bits are unaffected by the multiplicand and multiplier sign, MULL 
is useful for both signed and unsigned multiplication.
Operation
AR[r] ← AR[s] × AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULS.AA.*', 'Signed Multiply/Subtract
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MULS.AA.* as, at
Where * expands as follows:
MULS.AA.LL - for (half=0) 
MULS.AA.HL - for (half=1) 
MULS.AA.LH - for (half=2) 
MULS.AA.HH - for (half=3) 
Description
MULS.AA.* performs a two’s complement multiply of half of each of the address regis-
ters as and at, producing a 32-bit result. The result is sign-extended to 40 bits and 
subtracted from the contents of the MAC16 accumulator.
Operation
m1 ← if half0 then AR[s]31..16 else AR[s]15..0
m2 ← if half1 then AR[t]31..16 else AR[t]15..0
ACC ← ACC − (m11524||m1) × (m21524||m2)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULS.AD.*', 'Signed Multiply/Subtract
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MULS.AD.* as, my
Where * expands as follows:
MULS.AD.LL - for (half=0) 
MULS.AD.HL - for (half=1) 
MULS.AD.LH - for (half=2) 
MULS.AD.HH - for (half=3) 
Description
MULS.AD.* performs a two’s complement multiply of half of address register as and 
half of MAC16 register my, producing a 32-bit result. The result is sign-extended to 40 
bits and subtracted from the contents of the MAC16 accumulator. The my operand can 
designate either MAC16 register m2 or m3.
Operation
m1 ← if half0 then AR[s]31..16 else AR[s]15..0
m2 ← if half1 then MR[1||y]31..16 else MR[1||y]15..0
ACC ← ACC − (m11524||m1) × (m21524||m2)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULS.DA.*', 'Signed Multiply/Subtract
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MULS.DA.* mx, at
Where * expands as follows:
MULS.DA.LL - for (half=0) 
MULS.DA.HL - for (half=1) 
MULS.DA.LH - for (half=2) 
MULS.DA.HH - for (half=3) 
Description
MULS.DA.* performs a two’s complement multiply of half of MAC16 register mx and half 
of address register at, producing a 32-bit result. The result is sign-extended to 40 bits 
and subtracted from the contents of the MAC16 accumulator. The mx operand can 
designate either MAC16 register m0 or m1.
Operation
m1 ← if half0 then MR[0||x]31..16 else MR[0||x]15..0
m2 ← if half1 then AR[t]31..16 else AR[t]15..0
ACC ← ACC − (m11524||m1) × (m21524||m2)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULS.DD.*', 'Signed Multiply/Subtract
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
MULS.DD.* mx, my
Where * expands as follows:
MULS.DD.LL - for (half=0) 
MULS.DD.HL - for (half=1) 
MULS.DD.LH - for (half=2) 
MULS.DD.HH - for (half=3) 
Description
MULS.DD.* performs a two’s complement multiply of half of each of MAC16 registers 
mx and my, producing a 32-bit result. The result is sign-extended to 40 bits and subtract-
ed from the contents of the MAC16 accumulator. The mx operand can designate either 
MAC16 register m0 or m1. The my operand can designate either MAC16 register m2 or 
m3.
Operation
m1 ← if half0 then MR[0||x]31..16 else MR[0||x]15..0
m2 ← if half1 then MR[1||y]31..16 else MR[1||y]15..0
ACC ← ACC − (m11524||m1) × (m21524||m2)
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULSH', 'Multiply Signed High
Assembler Syntax
MULSH ar, as, at
Description
MULSH performs a 32-bit two’s complement multiplication of the contents of address reg-
isters as and at and writes the most significant 32 bits of the product to address register 
ar.
Operation
tp ← (AR[s]3132||AR[s]) × (AR[t]3132||AR[t])
AR[r] ← tp63..32
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'MULUH', 'Multiply Unsigned High
Assembler Syntax
MULUH ar, as, at
Description
MULUH performs an unsigned multiplication of the contents of address registers as and 
at, and writes the most significant 32 bits of the product to address register ar.
Operation
tp ← (032||AR[s]) × (032||AR[t])
AR[r] ← tp63..32
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'NEG', 'Negate
Assembler Syntax
NEG ar, at
Description
NEG calculates the two’s complement negation of the contents of address register at 
and writes it to address register ar. Arithmetic overflow is not detected.
Operation
AR[r] ← 0 − AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'NEG.S', 'Negate Single
Assembler Syntax
NEG.S fr, fs
Description
NEG.S negates the single-precision value of the contents of floating-point register fs 
and writes the result to floating-point register fr.
Operation
FR[r] ← −s FR[s]
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'NOP', 'No-Operation
Assembler Syntax
NOP
Description
This instruction performs no operation. It is typically used for instruction alignment. NOP 
is a 24-bit instruction. For a 16-bit version, see NOP.N. 
Assembler Note
The assembler may convert NOP instructions to NOP.N when the Code Density Option is 
enabled. Prefixing the NOP instruction with an underscore (_NOP) disables this optimiza-
tion and forces the assembler to generate the wide form of the instruction.
Operation
none
Exceptions
(cid:132)
EveryInst Group (see page 244)
Implementation Notes
In some implementations NOP is not an instruction but only an assembler macro that 
uses the instruction “OR An, An, An” (with An a convenient register).
');
INSERT INTO "instructions" VALUES('xtensa', 'NOP.N', 'Narrow No-Operation
Assembler Syntax
NOP.N
Description
This instruction performs no operation. It is typically used for instruction alignment. 
NOP.N is a 16-bit instruction. For a 24-bit version, see NOP.
Assembler Note
The assembler may convert NOP.N instructions to NOP. Prefixing the NOP.N instruction 
with an underscore (_NOP.N) disables this optimization and forces the assembler to 
generate the narrow form of the instruction.
Operation
none
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'NSA', 'Normalization Shift Amount
Miscellaneous Operations Option (See Section 4.3.8 on page 62)
Assembler Syntax
NSA at, as
Description
NSA calculates the left shift amount that will normalize the twos complement contents of 
address register as and writes this amount (in the range 0 to 31) to address register at. 
If as contains 0 or -1, NSA returns 31. Using SSL and SLL to shift as left by the NSA 
result yields the smallest value for which bits 31 and 30 differ unless as contains 0.
Operation
sign ← AR[s]31
if AR[s]30..0 = sign31 then
AR[t] ← 31
else
b4 ← AR[s]30..16 = sign15
t3 ← if b4 then AR[s]15..0 else AR[s]31..16
b3 ← t315..8 = sign8
t2 ← if b3 then t37..0 else t315..8
b2 ← t37..4 = sign4
t1 ← if b2 then t23..0 else t27..4
b1 ← t33..2 = sign2
b0 ← if b1 then t11 = sign else t13 = sign
AR[t] ← 027||((b4||b3||b2||b1||b0) − 1)
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'NSAU', 'Normalization Shift Amount Unsigned
Miscellaneous Operations Option (See Section 4.3.8 on page 62)
Assembler Syntax
NSAU at, as
Description
NSAU calculates the left shift amount that will normalize the unsigned contents of 
address register as and writes this amount (in the range 0 to 32) to address register at. 
If as contains 0, NSAU returns 32. Using SSL and SLL to shift as left by the NSAU result 
yields the smallest value for which bit 31 is set, unless as contains 0.
Operation
if AR[s] = 032 then
else
AR[t] ← 32
b4 ← AR[s]31..16 = 016
t3 ← if b4 then AR[s]15..0 else AR[s]31..16
b3 ← t315..8 = 08
t2 ← if b3 then t37..0 else t315..8
b2 ← t27..4 = 04
t1 ← if b2 then t23..0 else t27..4
b1 ← t13..2 = 02
b0 ← if b1 then t11 = 0 else t13 = 0
AR[t] ← 027||b4||b3||b2||b1||b0
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'OEQ.S', 'Compare Single Equal
Assembler Syntax
OEQ.S br, fs, ft
Description
OEQ.S compares the contents of floating-point registers fs and ft for IEEE754 equality. 
If the values are ordered and equal then Boolean register br is set to 1, otherwise br is 
set to 0. IEEE754 specifies that +0 and −0 compare as equal. IEEE754 floating-point 
values are ordered if neither is a NaN.
Operation
BRr ← not isNaN(FR[s]) and not isNaN(FR[t])
and (FR[s] =s FR[t])
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'OLE.S', 'Compare Single Ord & Less Than or Equal
Assembler Syntax
OLE.S br, fs, ft
Description
OLE.S compares the contents of floating-point registers fs and ft. If the contents of fs 
are ordered with, and less than or equal to the contents of ft, then Boolean register br 
is set to 1, otherwise br is set to 0. According to IEEE754, +0 and −0 compare as equal. 
IEEE754 floating-point values are ordered if neither is a NaN.
Operation
BRr ← not isNaN(FR[s]) and not isNaN(FR[t])
and (FR[s] ≤s FR[t])
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'OLT.S', 'Compare Single Ordered and Less Than
Assembler Syntax
OLT.S br, fs, ft
Description
OLT.S compares the contents of floating-point registers fs and ft. If the contents of fs 
are ordered with and less than the contents of ft then Boolean register br is set to 1, 
otherwise br is set to 0. According to IEEE754, +0 and −0 compare as equal. IEEE754 
floating-point values are ordered if neither is a NaN.
Operation
BRr ← not isNaN(FR[s]) and not isNaN(FR[t])
and (FR[s] <s FR[t])
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'OR', 'Bitwise Logical Or
Assembler Syntax
OR ar, as, at
Description
OR calculates the bitwise logical or of address registers as and at. The result is written 
to address register ar.
Operation
AR[r] ← AR[s] or AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ORB', 'Boolean Or
Assembler Syntax
ORB br, bs, bt
Description
ORB performs the logical or of Boolean registers bs and bt, and writes the result to 
Boolean register br.
When the sense of one of the source Booleans is inverted (0 → true, 1 → false), use 
ORBC. When the sense of both of the source Booleans is inverted, use ANDB and an 
inverted test of the result.
Operation
BRr ← BRs or BRt
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'ORBC', 'Boolean Or with Complement
Assembler Syntax
ORBC br, bs, bt
Description
ORBC performs the logical or of Boolean register bs with the logical complement of 
Boolean register bt and writes the result to Boolean register br.
Operation
BRr ← BRs or not BRt
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'PDTLB', 'Probe Data TLB
Region Translation Option (page 156) or the MMU Option (page 158)
Assembler Syntax
PDTLB at, as
Description
PDTLB searches the data TLB for an entry that translates the virtual address in address 
register as and writes the way and index of that entry to address register at. If no entry 
matches, zero is written to the hit bit of at. The value written to at is implementation-
specific, but in all implementations a value with the hit bit set is suitable as an input to 
the IDTLB or WDTLB instructions. See Section 4.6 on page 138 for information on the re-
sult register formats for specific memory protection and translation options.
PDTLB is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
(match, vpn, ei, wi) ← ProbeDataTLB(AR[s])
if match > 1 then
EXCVADDR ← AR[s]
Exception (LoadStoreTLBMultiHit)
AR[t] ← PackDataTLBEntrySpec(match, vpn, ei, wi)
else
endif
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(LoadStoreTLBMultiHitCause) if Region Protection Option or MMU Option
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'PITLB', 'Probe Instruction TLB
Region Translation Option (page 156) or the MMU Option (page 158)
Assembler Syntax
PITLB at, as
Description
PITLB searches the Instruction TLB for an entry that translates the virtual address in 
address register as and writes the way and index of that entry to address register at. If 
no entry matches, zero is written to the hit bit of at. The value written to at is implemen-
tation-specific, but in all implementations a value with the hit bit set is suitable as an in-
put to the IITLB or WITLB instructions. See Section 4.6 on page 138 for information on 
the result register formats for specific memory protection and translation options.
PITLB is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
(match, vpn, ei, wi) ← ProbeInstTLB(AR[s])
if match > 1 then
EXCVADDR ← AR[s]
Exception (InstructionFetchTLBMultiHit)
AR[t] ← PackInstTLBEntrySpec(match, vpn, ei, wi)
else
endif
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'QUOS', 'Quotient Signed
Assembler Syntax
QUOS ar, as, at
Description
QUOS performs a 32-bit two’s complement division of the contents of address register as 
by the contents of address register at and writes the quotient to address register ar. 
The ambiguity which exists when either address register as or address register at is 
negative is resolved by requiring the product of the quotient and address register at to 
be smaller in absolute value than the address register as. If the contents of address reg-
ister at are zero, QUOS raises an Integer Divide by Zero exception instead of writing a 
result. Overflow (-2147483648 divided by -1) is not detected.
Operation
Exception (IntegerDivideByZero)
AR[r] ← AR[s] quo AR[t]
if AR[t] = 032 then
else
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(IntegerDivideByZeroCause) if 32-bit Integer Divide Option
');
INSERT INTO "instructions" VALUES('xtensa', 'QUOU', 'Quotient Unsigned
Assembler Syntax
QUOU ar, as, at
Description
QUOU performs a 32-bit unsigned division of the contents of address register as by the 
contents of address register at and writes the quotient to address register ar. If the con-
tents of address register at are zero, QUOU raises an Integer Divide by Zero exception 
instead of writing a result.
Operation
if AR[t] = 032 then
else
Exception (IntegerDivideByZero)
tq ← (0||AR[s]) quo (0||AR[t])
AR[r] ← tq31..0
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(IntegerDivideByZeroCause) if 32-bit Integer Divide Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RDTLB0', 'Read Data TLB Entry Virtual
Region Translation Option (page 156) or the MMU Option (page 158)
Assembler Syntax
RDTLB0 at, as
Description
RDTLB0 reads the data TLB entry specified by the contents of address register as and 
writes the Virtual Page Number (VPN) and address space ID (ASID) to address register 
at. See Section 4.6 on page 138 for information on the address and result register for-
mats for specific memory protection and translation options.
RDTLB0 is a privileged instruction.
Operation
AR[t] ← RDTLB0(AR[s])
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RDTLB1', 'Read Data TLB Entry Translation
Region Translation Option (page 156) or the MMU Option (page 158)
Assembler Syntax
RDTLB1 at, as
Description
RDTLB1 reads the data TLB entry specified by the contents of address register as and 
writes the Physical Page Number (PPN) and cache attribute (CA) to address register 
at. See Section 4.6 on page 138 for information on the address and result register for-
mats for specific memory protection and translation options.
RDTLB1 is a privileged instruction.
Operation
AR[t] ← RDTLB1(AR[s])
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'REMS', 'Remainder Signed
Assembler Syntax
REMS ar, as, at
Description
REMS performs a 32-bit two’s complement division of the contents of address register as 
by the contents of address register at and writes the remainder to address register ar. 
The ambiguity which exists when either address register as or address register at is 
negative is resolved by requiring the remainder to have the same sign as address regis-
ter as. If the contents of address register at are zero, REMS raises an Integer Divide by 
Zero exception instead of writing a result.
Operation
Exception (IntegerDivideByZero)
AR[r] ← AR[s] rem AR[t]
if AR[t] = 032 then
else
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(IntegerDivideByZeroCause) if 32-bit Integer Divide Option 
');
INSERT INTO "instructions" VALUES('xtensa', 'REMU', 'Remainder Unsigned
Assembler Syntax
REMU ar, as, at
Description
REMU performs a 32-bit unsigned division of the contents of address register as by the 
contents of address register at and writes the remainder to address register ar. If the 
contents of address register at are zero, REMU raises an Integer Divide by Zero excep-
tion instead of writing a result.
Operation
if AR[t] = 032 then
else
Exception (IntegerDivideByZero)
tr ← (0||AR[s]) rem (0||AR[t])
AR[r] ← tr31..0
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
GenExcep(IntegerDivideByZeroCause) if 32-bit Integer Divide Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RER', 'Read External Register
Assembler Syntax
RER at, as
Description
RER reads one of a set of "External Registers". It is in some ways similar to the RSR.* 
instruction except that the registers being read are not defined by the Xtensa ISA and 
are conceptually outside the processor core. They are read through processor ports.
Address register as is used to determine which register is to be read and the result is 
placed in address register at. When no External Register is addressed by the value in 
address register as, the result in address register at is undefined. The entire address 
space is reserved for use by Tensilica. RER and WER are managed by the processor core 
so that the requests appear on the processor ports in program order. External logic is re-
sponsible for extending that order to the registers themselves.
RER is a privileged instruction.
Operation
Exception (PrivilegedInstructionCause)
Read External Register as defined outside the processor.
if CRING ≠ 0 then
else
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RET', 'Non-Windowed Return
Assembler Syntax
RET
Description
RET returns from a routine called by CALL0 or CALLX0. It is equivalent to the instruction
JX
A0
RET exists as a separate instruction because some Xtensa ISA implementations may 
realize performance advantages from treating this operation as a special case.
Assembler Note
The assembler may convert RET instructions to RET.N when the Code Density Option is 
enabled. Prefixing the RET instruction with an underscore (_RET) disables this optimiza-
tion and forces the assembler to generate the wide form of the instruction.
Operation
nextPC ← AR[0]
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'RET.N', 'Narrow Non-Windowed Return
Assembler Syntax
RET.N
Description
RET.N is the same as RET in a 16-bit encoding. RET returns from a routine called by 
CALL0 or CALLX0.
Assembler Note
The assembler may convert RET.N instructions to RET. Prefixing the RET.N instruction 
with an underscore (_RET.N) disables this optimization and forces the assembler to 
generate the narrow form of the instruction.
Operation
nextPC ← AR[0]
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'RETW', 'Windowed Return
Assembler Syntax
RETW
Description
RETW returns from a subroutine called by CALL4, CALL8, CALL12, CALLX4, CALLX8, or 
CALLX12, and that had ENTRY as its first instruction.
RETW uses bits 29..0 of address register a0 as the low 30 bits of the return address 
and bits 31..30 of the address of the RETW as the high two bits of the return address. 
Bits 31..30 of a0 are used as the caller’s window increment.
RETW subtracts the window increment from WindowBase to return to the caller’s regis-
ters. It then checks the WindowStart bit for this WindowBase. If it is set, then the 
caller’s registers still reside in the register file, and RETW completes by clearing its own 
WindowStart bit and jumping to the return address. If the WindowStart bit is clear, 
then the caller’s registers have been stored into the stack, so RETW signals one of win-
dow underflow’s 4, 8, or 12, based on the size of the caller’s window increment. The un-
derflow handler is invoked with WindowBase decremented, a minor exception to the 
rule that instructions aborted by an exception have no side effects to the operating state 
of the processor. The processor stores the previous value of WindowBase in PS.OWB so 
that it can be restored by RFWU.
The window underflow handler is expected to restore the caller’s registers, set the 
caller’s WindowStart bit, and then return (see RFWU) to re-execute the RETW, which 
will then complete.
The operation of this instruction is undefined if AR[0]31..30 is 02, if PS.WOE is 0, if 
PS.EXCM is 1, or if the first set bit among [WindowStartWindowBase-1, 
WindowStartWindowBase-2, WindowStartWindowBase-3] is anything other than 
WindowStartWindowBase-n, where n is AR[0]31..30. (If none of the three bits is set, an 
underflow exception will be raised as described above, but if the wrong first one is set, 
the state is not legal.) Some implementations raise an illegal instruction exception in 
these cases as a debugging aid.
Assembler Note
The assembler may convert RETW instructions to RETW.N when the Code Density 
Option is enabled. Prefixing the RETW instruction with an underscore (_RETW) disables 
this optimization and forces the assembler to generate the wide form of the instruction.
Operation
n ← AR[0]31..30
nextPC ← PC31..30||AR[0]29..0
owb ← WindowBase
m ← if WindowStartWindowBase-4’b0001 then 2’b01
 elsif WindowStartWindowBase-4’b0010 then 2’b10
 elsif WindowStartWindowBase-4’b0011 then 2’b11
 else 2’b00
if n=2’b00 | (m≠2’b00 & m≠n) | PS.WOE=0 | PS.EXCM=1 then
-- undefined operation
-- may raise illegal instruction exception
WindowBase ← WindowBase − (02||n)
if WindowStartWindowBase ≠ 0 then
else
WindowStartowb ← 0
-- Underflow exception
PS.EXCM ← 1
EPC[1] ← PC
PS.OWB ← owb
nextPC ← if n = 2''b01 then WindowUnderflow4
else if n = 2''b10 then WindowUnderflow8
else WindowUnderflow12
else
endif
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) WindowUnderExcep if Windowed Register Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RETW.N', 'Narrow Windowed Return
(See Section 4.7.1 on page 180)
Assembler Syntax
RETW.N
Description
RETW.N is the same as RETW in a 16-bit encoding.
Assembler Note
The assembler may convert RETW.N instructions to RETW. Prefixing the RETW.N instruc-
tion with an underscore (_RETW.N) disables this optimization and forces the assembler 
to generate the narrow form of the instruction.
Operation
n ← AR[0]31..30
nextPC ← PC31..30||AR[0]29..0
owb ← WindowBase
m ← if WindowStartWindowBase-4’b0001 then 2’b01
 elsif WindowStartWindowBase-4’b0010 then 2’b10
 elsif WindowStartWindowBase-4’b0011 then 2’b11
 else 2’b00
if n=2’b00 | (m≠2’b00 & m≠n) | PS.WOE=0 | PS.EXCM=1 then
else
-- undefined operation
-- may raise illegal instruction exception
WindowBase ← WindowBase − (02||n)
if WindowStartWindowBase ≠ 0 then
else
WindowStartowb ← 0
-- Underflow exception
PS.EXCM ← 1
EPC[1] ← PC
PS.OWB ← owb
nextPC ← if n = 2''b01 then WindowUnderflow4
else if n = 2''b10 then WindowUnderflow8
else WindowUnderflow12
endif
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) WindowUnderExcep if Windowed Register Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RFDD', 'Return from Debug and Dispatch
s0
Assembler Syntax
RFDD
Description
This instruction is used only in On-Chip Debug Mode and exists only in some implemen-
tations. It is an illegal instruction when the processor is not in On-Chip Debug Mode. 
See the Tensilica On-Chip Debugging Guide for a description of its operation.
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(IllegalInstructionCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RFDE', 'Return from Double Exception
Exception Option (See Section 4.4.1 on page 82)
Assembler Syntax
RFDE
Description
RFDE returns from an exception that went to the double exception vector (that is, an ex-
ception raised while the processor was executing with PS.EXCM set). It is similar to RFE, 
but PS.EXCM is not cleared, and DEPC, if it exists, is used instead of EPC[1]. RFDE sim-
ply jumps to the exception PC. PS.UM and PS.WOE are left unchanged.
RFDE is a privileged instruction.
Operation
if CRING ≠ 0 then
Exception (PrivilegedInstructionCause)
elsif NDEPC=1 then
nextPC ¨ DEPC
else
nextPC ← EPC[1]
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RFDO', 'Return from Debug Operation
Assembler Syntax
RFDO
Description
This instruction is used only in On-Chip Debug Mode and exists only in some implemen-
tations. It is an illegal instruction when the processor is not in On-Chip Debug Mode. 
See the Tensilica On-Chip Debugging Guide for a description of its operation.
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(IllegalInstructionCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RFE', 'Return from Exception
Exception Option (See Section 4.4.1 on page 82)
Assembler Syntax
RFE
Description
RFE returns from either the UserExceptionVector or the KernelExceptionVector. RFE 
sets PS.EXCM back to 0, and then jumps to the address in EPC[1]. PS.UM and PS.WOE 
are left unchanged.
RFE is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
PS.EXCM ← 0
nextPC ← EPC[1]
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RFI', 'Return from High-Priority Interrupt
level
High-Priority Interrupt Option (See Section 4.4.5 on page 106)
Assembler Syntax
RFI 0..15
Description
RFI returns from a high-priority interrupt. It restores the PS from EPS[level] and 
jumps to the address in EPC[level]. Level is given as a constant 2..(NLEVEL+NNMI) 
in the instruction word. The operation of this opcode when level is 0 or 1 or greater than 
(NLEVEL+NNMI) is undefined.
RFI is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
nextPC ← EPC[level]
PS ← EPS[level]
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RFME', 'Return from Memory Error
Memory ECC/Parity Option (See Section 4.5.14 on page 128)
Assembler Syntax
RFME
Description
RFME returns from a memory error exception. It restores the PS from MEPS and jumps to 
the address in MEPC. In addition, the MEME bit of the MESR register is cleared.
RFME is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
nextPC ← MEPC
PS ← MEPS
MESR.MEME ← 0
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RFR', 'Move FR to AR
Assembler Syntax
RFR ar, fs
Description
RFR moves the contents of floating-point register fs to address register ar. The move is 
non-arithmetic; no floating-point exceptions are raised.
Operation
AR[r] ← FR[s]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RFUE', 'Return from User-Mode Exception
Exception Option (Xtensa Exception Architecture 1 Only)
Assembler Syntax
RFUE
Description
RFUE exists only in Xtensa Exception Architecture 1 (see Section A.2 “Xtensa Exception 
Architecture 1” on page 611). It is an illegal instruction in current Xtensa implementa-
tions.
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(IllegalInstructionCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RFWO', 'Return from Window Overflow
Assembler Syntax
RFWO
Description
RFWO returns from an exception that went to one of the three window overflow vectors. It 
sets PS.EXCM back to 0, clears the WindowStart bit of the registers that were spilled, 
restores WindowBase from PS.OWB, and then jumps to the address in EPC[1]. PS.UM 
is left unchanged.
RFWO is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
PS.EXCM ← 0
nextPC ← EPC[1]
WindowStartWindowBase ← 0
WindowBase ← PS.OWB
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RFWU', 'Return From Window Underflow
Assembler Syntax
RFWU
Description
RFWU returns from an exception that went to one of the three window underflow vectors. 
It sets PS.EXCM back to 0, sets the WindowStart bit of the registers that were reload-
ed, restores WindowBase from PS.OWB, and then jumps to the address in EPC[1]. 
PS.UM is left unchanged.
RFWU is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
PS.EXCM ← 0
nextPC ← EPC[1]
WindowStartWindowBase ← 1
WindowBase ← PS.OWB
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RITLB0', 'Read Instruction TLB Entry Virtual
Region Translation Option (page 156) or the MMU Option (page 158)
Assembler Syntax
RITLB0 at, as
Description
RITLB0 reads the instruction TLB entry specified by the contents of address register as 
and writes the Virtual Page Number (VPN) and address space ID (ASID) to address reg-
ister at. See Section 4.6 on page 138 for information on the address and result register 
formats for specific memory protection and translation options.
RITLB0 is a privileged instruction.
Operation
AR[t] ← RITLB0(AR[s])
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RITLB1', 'Read Instruction TLB Entry Translation
Region Translation Option (page 156) or the MMU Option (page 158)
Assembler Syntax
RITLB1 at, as
Description
RITLB1 reads the instruction TLB entry specified by the contents of address register as 
and writes the Physical Page Number (PPN) and cache attribute (CA) to address regis-
ter at. See Section 4.6 on page 138 for information on the address and result register 
formats for specific memory protection and translation options.
RITLB1 is a privileged instruction.
Operation
AR[t] ← RITLB1(AR[s])
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'ROTW', 'Rotate Window
imm4
Assembler Syntax
ROTW -8..7
Description
ROTW adds a constant to WindowBase, thereby moving the current window into the 
register file. ROTW is intended for use in exception handlers and context switch code.
ROTW is a privileged instruction.
Operation
Exception (PrivilegedInstructionCause)
WindowBase ← WindowBase + imm4
if CRING ≠ 0 then
else
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'ROUND.S', 'Round Single to Fixed
Assembler Syntax
ROUND.S ar, fs, 0..15
Description
ROUND.S converts the contents of floating-point register fs from single-precision to 
signed integer format, rounding toward the nearest. The single-precision value is first 
scaled by a power of two constant value encoded in the t field, with 0..15 representing 
point is at the right end of the integer for t=0 and moves to the left as t increases until 
for t=15 there are 15 fractional bits represented in the fixed point number. For positive 
overflow (value ≥ 32''h7fffffff), positive infinity, or NaN, 32''h7fffffff is 
returned; for negative overflow (value ≤ 32''h80000000) or negative infinity, 
Operation
AR[r] ← rounds(FR[s] ×s pows(2.0,t))
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RSIL', 'Read and Set Interrupt Level
imm4
Interrupt Option (See Section 4.4.4 on page 100)
Assembler Syntax
RSIL at, 0..15
Description
RSIL first reads the PS Special Register (described in Table 4–63 on page 87, PS Reg-
ister Fields), writes this value to address register at, and then sets PS.INTLEVEL to a 
constant in the range 0..15 encoded in the instruction word. Interrupts at and below the 
PS.INTLEVEL level are disabled.
A WSR.PS or XSR.PS followed by an RSIL should be separated with an ESYNC to guar-
antee the value written is read back.
On some Xtensa ISA implementations the latency of RSIL is greater than one cycle, 
and so it is advantageous to schedule uses of the RSIL result later.
RSIL is typically used as follows:
RSIL a2, newlevel
code to be executed at newlevel
WSR.PS
a2
The instruction following the RSIL is guaranteed to be executed at the new interrupt 
level specified in PS.INTLEVEL, therefore it is not necessary to insert one of the SYNC 
instructions to force the interrupt level change to take effect. 
RSIL is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
AR[t] ← PS
PS.INTLEVEL ← s
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RSR.*', 'Read Special Register
sr
Assembler Syntax
RSR.* at
RSR at, *
RSR at, 0..255
Description
RSR.* reads the Special Registers that are described in Section 3.8.10 “Processor Con-
trol Instructions” on page 45. See Section 5.3 on page 208 for more detailed information 
on the operation of this instruction for each Special Register.
The contents of the Special Register designated by the 8-bit sr field of the instruction 
word are written to address register at. The name of the Special Register is used in 
place of the ‘*’ in the assembler syntax above and the translation is made to the 8-bit sr 
field by the assembler.
RSR is an assembler macro for RSR.* that provides compatibility with the older versions 
of the instruction containing either the name or the number of the Special Register.
A WSR.* followed by an RSR.* to the same register should be separated with ESYNC to 
guarantee the value written is read back. On some Xtensa ISA implementations, the la-
tency of RSR.* is greater than one cycle, and so it is advantageous to schedule other 
instructions before instructions that use the RSR.* result.
RSR.* with Special Register numbers ≥ 64 is privileged. An RSR.* for an unconfigured 
register generally will raise an illegal instruction exception.
Operation
sr ← if msbFirst then s||r else r||s
if sr ≥ 64 and CRING ≠ 0 then
Exception (PrivilegedInstructionCause)
see the Tables in Section 5.3 on page 208
else
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(IllegalInstructionCause) if Exception Option
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'RSYNC', 'Register Read Synchronize
Assembler Syntax
RSYNC
Description
RSYNC waits for all previously fetched WSR.* instructions to be performed before inter-
preting the register fields of the next instruction. This operation is also performed as part 
of ISYNC. ESYNC and DSYNC are performed as part of this instruction.
This instruction is appropriate after WSR.WindowBase, WSR.WindowStart, WSR.PS, 
WSR.CPENABLE, or WSR.EPS* instructions before using their results. See the Special 
Register Tables in Section 5.3 on page 208 for a complete description of the uses of the 
RSYNC instruction.
Because the instruction execution pipeline is implementation-specific, the operation sec-
tion below specifies only a call to the implementation’s rsync function.
Operation
rsync()
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'RUR.*', 'Read User Register
No Option - instructions created from the TIE language (See Section 4.3.9.2 “Coproces-
sor Context Switch” on page 64)
Assembler Syntax
RUR.* ar
RUR ar, *
Description
RUR.* reads TIE state that has been grouped into 32-bit quantities by the TIE 
user_register statement. The name in the user_register statement replaces the 
“*” in the instruction name and causes the correct register number to be placed in the st 
field of the encoded instruction. The contents of the TIE user_register designated by 
the 8-bit number 16*s+t are written to address register ar. Here s and t are the 
numbers corresponding to the respective fields of the instruction word.
RUR is an assembler macro for RUR.*, which provides compatibility with the older 
version of the instruction.
Operation
AR[r] ← user_register[st]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor*Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'S8I', 'Store 8-bit
Assembler Syntax
S8I at, as, 0..255
Description
S8I is an 8-bit store from address register at to memory. It forms a virtual address by 
adding the contents of address register as and an 8-bit zero-extended constant value 
encoded in the instruction word. Therefore, the offset has a range from 0 to 255. Eight 
bits (1 byte) from the least significant quarter of address register at are written to mem-
ory at the physical address.
If the Region Translation Option (page 156) or the MMU Option (page 158)is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Operation
vAddr ← AR[s] + (024||imm8)
Store8 (vAddr, AR[t]7..0)
Exceptions
(cid:132) Memory Group (see page 244)
(cid:132) GenExcep(StoreProhibitedCause) if Region Protection Option or MMU Option
(cid:132) DebugExcep(DBREAK) if Debug Option
');
INSERT INTO "instructions" VALUES('xtensa', 'S16I', 'Store 16-bit
Assembler Syntax
S16I at, as, 0..510
Description
S16I is a 16-bit store from address register at to memory. It forms a virtual address by 
adding the contents of address register as and an 8-bit zero-extended constant value 
encoded in the instruction word shifted left by one. Therefore, the offset can specify mul-
tiples of two from zero to 510. Sixteen bits (two bytes) from the least significant half of 
the register are written to memory at the physical address.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the least significant bit of the 
address is ignored. A reference to an odd address produces the same result as a refer-
ence to the address, minus one. With the Unaligned Exception Option, such an access 
raises an exception.
Assembler Note
To form a virtual address, S16I calculates the sum of address register as and the imm8 
field of the instruction word times two. Therefore, the machine-code offset is in terms of 
the instruction by dividing by two.
Operation
vAddr ← AR[s] + (023||imm8||0)
Store16 (vAddr, AR[t]15..0)
Exceptions
(cid:132) Memory Store Group (see page 245)
');
INSERT INTO "instructions" VALUES('xtensa', 'S32C1I', 'Store 32-bit Compare Conditional
Conditional Store Option (See Section 4.3.13 on page 77)
Assembler Syntax
S32C1I at, as, 0..1020
Description
S32C1I is a conditional store instruction intended for updating synchronization variables 
in memory shared between multiple processors. It may also be used to atomically up-
date variables shared between different interrupt levels or other pairs of processes on a 
single processor. S32C1I attempts to store the contents of address register at to the 
virtual address formed by adding the contents of address register as and an 8-bit zero-
extended constant value encoded in the instruction word shifted left by two. If the old 
contents of memory at the physical address equals the contents of the SCOMPARE1 Spe-
cial Register, the new data is written; otherwise the memory is left unchanged. In either 
case, the value read from the location is written to address register at. The memory 
read, compare, and write may take place in the processor or the memory system, de-
pending on the Xtensa ISA implementation, as long as these operations exclude other 
writes to this location. See Section 4.3.13 “Conditional Store Option” on page 77 for 
more information on where the atomic operation takes place.
From a memory ordering point of view, the atomic pair of accesses has the characteris-
tics of both an acquire and a release. That is, the atomic pair of accesses does not begin 
until all previous loads, stores, acquires, and releases have performed. The atomic pair 
must perform before any following load, store, acquire, or release may begin.
If the Region Translation Option (page 156) or the MMU Option (page 158)is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
');
INSERT INTO "instructions" VALUES('xtensa', 'S32C1I', 'Store 32-bit Compare Conditional
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
S32C1I does both a load and a store when the store is successful. However, memory 
protection tests check for store capability and the instruction may raise a 
StoreProhibitedCause exception, but will never raise a LoadProhibited Cause exception.
Assembler Note
To form a virtual address, S32C1I calculates the sum of address register as and the 
imm8 field of the instruction word times four. Therefore, the machine-code offset is in 
terms of 32-bit (4 byte) units. However, the assembler expects a byte offset and encodes 
this into the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
(mem32, error) ← Store32C1 (vAddr, AR[t], SCOMPARE1)
if error then
EXCVADDR ← vAddr
Exception (LoadStoreError)
AR[t] ← mem32
else
endif
Exceptions
(cid:132) Memory Store Group (see page 245)
');
INSERT INTO "instructions" VALUES('xtensa', 'S32E', 'Store 32-bit for Window Exceptions
Assembler Syntax
S32E at, as, -64..-4
Description
S32E is a 32-bit store instruction similar to S32I, but with semantics required by window 
overflow and window underflow exception handlers. In particular, memory access check-
ing is done with PS.RING instead of CRING, and the offset used to form the virtual ad-
dress is a 4-bit one-extended immediate. Therefore, the offset can specify multiples of 
four from -64 to -4. In configurations without the MMU Option, there is no PS.RING and 
S32E is similar to S32I with a negative offset.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
S32E is a privileged instruction.
Assembler Note
To form a virtual address, S32E calculates the sum of address register as and the r field 
of the instruction word times four (and one extended). Therefore, the machine-code 
offset is in terms of 32-bit (4 byte) units. However, the assembler expects a byte offset 
and encodes this into the instruction by dividing by four.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
vAddr ← AR[s] + (126||r||02)
ring ← if MMU Option then PS.RING else 0
Store32Ring (vAddr, AR[t], ring)
endif
Exceptions
(cid:132) Memory Store Group (see page 245)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'S32I', 'Store 32-bit
Assembler Syntax
S32I at, as, 0..1020
Description
S32I is a 32-bit store from address register at to memory. It forms a virtual address by 
adding the contents of address register as and an 8-bit zero-extended constant value 
encoded in the instruction word shifted left by two. Therefore, the offset can specify mul-
tiples of four from zero to 1020. The data to be stored is taken from the contents of ad-
dress register at and written to memory at the physical address.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an er-
ror (for example, protection violation or non-existent memory), the processor raises one 
of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
S32I is one of only a few memory reference instructions that can access instruction 
RAM.
Assembler Note
The assembler may convert S32I instructions to S32I.N when the Code Density 
Option is enabled and the imm8 operand falls within the available range. Prefixing the 
S32I instruction with an underscore (_S32I) disables this optimization and forces the 
assembler to generate the wide form of the instruction.
To form a virtual address, S32I calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
Store32 (vAddr, AR[t])
Exceptions
(cid:132) Memory Store Group (see page 245)
');
INSERT INTO "instructions" VALUES('xtensa', 'S32I.N', 'Narrow Store 32-bit
imm4
Assembler Syntax
S32I.N at, as, 0..60
Description
S32I.N is similar to S32I, but has a 16-bit encoding and supports a smaller range of 
offset values encoded in the instruction word.
S32I.N is a 32-bit store to memory. It forms a virtual address by adding the contents of 
address register as and an 4-bit zero-extended constant value encoded in the instruc-
tion word shifted left by two. Therefore, the offset can specify multiples of four from zero 
to 60. The data to be stored is taken from the contents of address register at and written 
to memory at the physical address.
S32I.N is one of only a few memory reference instructions that can access instruction 
RAM.
If the Region Translation Option (page 156) or the MMU Option (page 158)is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Options, such an access raises an exception.
Assembler Note
The assembler may convert S32I.N instructions to S32I. Prefixing the S32I.N instruc-
tion with an underscore (_S32I.N) disables this optimization and forces the assembler 
to generate the narrow form of the instruction.
To form a virtual address, S32I.N calculates the sum of address register as and the 
imm4 field of the instruction word times four. Therefore, the machine-code offset is in 
terms of 32-bit (4 byte) units. However, the assembler expects a byte offset and encodes 
this into the instruction by dividing by four.
Operation
vAddr ← AR[s] + (026||imm4||02)
Store32 (vAddr, AR[t])
Exceptions
(cid:132) Memory Store Group (see page 245)
');
INSERT INTO "instructions" VALUES('xtensa', 'S32RI', 'Store 32-bit Release
Multiprocessor Synchronization Option (See Section 4.3.12 on page 74)
Assembler Syntax
S32RI at, as, 0..1020
Description
S32RI is a store barrier and 32-bit store from address register at to memory. S32RI 
stores to synchronization variables, which signals that previously written data is 
“released” for consumption by readers of the synchronization variable. This store will not 
perform until all previous loads, stores, acquires, and releases have performed. This 
ensures that any loads of the synchronization variable that see the new value will also 
find all previously written data available as well.
S32RI forms a virtual address by adding the contents of address register as and an  
Therefore, the offset can specify multiples of four from zero to 1020. S32RI waits for 
previous loads, stores, acquires, and releases to be performed, and then the data to be 
stored is taken from the contents of address register at and written to memory at the 
physical address. Because the method of waiting is implementation dependent, this is 
indicated in the operation section below by the implementation function release.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without theUnaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
Assembler Note
To form a virtual address, S32RI calculates the sum of address register as and the 
imm8 field of the instruction word times four. Therefore, the machine-code offset is in 
terms of 32-bit (4 byte) units. However, the assembler expects a byte offset and encodes 
this into the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
release()
Store32 (vAddr, AR[t])
Exceptions
(cid:132) Memory Store Group (see page 245)
');
INSERT INTO "instructions" VALUES('xtensa', 'SDCT', 'Store Data Cache Tag
Data Cache Test Option (See Section 4.5.6 on page 121)
Assembler Syntax
SDCT at, as
Description
SDCT is not part of the Xtensa Instruction Set Architecture, but is instead specific to an 
implementation. That is, it may not exist in all implementations of the Xtensa ISA.
SDCT is intended for writing the RAM array that implements the data cache tags as part 
of manufacturing test.
SDCT uses the contents of address register as to select a line in the data cache and 
writes the contents of address register at to the tag associated with that line. The value 
written from at is described under Cache Tag Format in Section 4.5.1.2 on page 112.
SDCT is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
index ← AR[s]dih..dil
DataCacheTag[index] ← AR[t]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
(cid:132) MemoryErrorException if Memory ECC/Parity Option
Implementation Notes
x ← ceil(log2(DataCacheBytes))
y ← log2(DataCacheBytes ÷ DataCacheWayCount)
z ← log2(DataCacheLineBytes)
The cache line specified by index AR[s]x-1..z in a direct-mapped cache or way 
AR[s]x-1..y and index AR[s]y-1..z in a set-associative cache is the chosen line. If the 
specified cache way is not valid (the fourth way of a three way cache), the instruction 
does nothing.
');
INSERT INTO "instructions" VALUES('xtensa', 'SEXT', 'Sign Extend
Miscellaneous Operations Option (See Section 4.3.8 on page 62)
Assembler Syntax
SEXT ar, as, 7..22
Description
SEXT takes the contents of address register as and replicates the bit specified by its 
immediate operand (in the range 7 to 22) to the high bits and writes the result to address 
register ar. The input can be thought of as an imm+1 bit value with the high bits irrele-
vant and this instruction produces the 32-bit sign-extension of this value.
Assembler Note
The immediate values accepted by the assembler are 7 to 22. The assembler encodes 
these in the t field of the instruction using 0 to 15.
Operation
b ← t+7
AR[r] ← AR[s]b31−b||AR[s]b..0
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SICT', 'Store Instruction Cache Tag
Instruction Cache Test Option (See Section 4.5.3 on page 116)
Assembler Syntax
SICT at, as
Description
SICT is not part of the Xtensa Instruction Set Architecture, but is instead specific to an 
implementation. That is, it may not exist in all implementations of the Xtensa ISA.
SICT is intended for writing the RAM array that implements the instruction cache tags as 
part of manufacturing test.
SICT uses the contents of address register as to select a line in the instruction cache, 
and writes the contents of address register at to the tag associated with that line. The 
value written from at is described under Cache Tag Format in Section 4.5.1.2 on 
page 112.
SICT is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
index ← AR[s]iih..iil
InstCacheTag[index] ← AR[t]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
(cid:132) MemoryErrorException if Memory ECC/Parity Option
');
INSERT INTO "instructions" VALUES('xtensa', 'SICT', 'Implementation Notes
Store Instruction Cache Tag
x ← ceil(log2(InstCacheBytes))
y ← log2(InstCacheBytes ÷ InstCacheWayCount)
z ← log2(InstCacheLineBytes)
The cache line specified by index AR[s]x-1..z in a direct-mapped cache or way 
AR[s]x-1..y and index AR[s]y-1..z in a set-associative cache is the chosen line. If the 
specified cache way is not valid (the fourth way of a three way cache), the instruction 
does nothing.
');
INSERT INTO "instructions" VALUES('xtensa', 'SICW', 'Store Instruction Cache Word
Instruction Cache Test Option (See Section 4.5.3 on page 116)
Assembler Syntax
SICW at, as
Description
SICW is not part of the Xtensa Instruction Set Architecture, but is instead specific to an 
implementation. That is, it may not exist in all implementations of the Xtensa ISA.
SICW is intended for writing the RAM array that implements the instruction cache as part 
of manufacturing tests.
SICW uses the contents of address register as to select a line in the instruction cache, 
and writes the contents of address register at to the data associated with that line.
SICW is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
index ← AR[s]iih..iiw
InstCacheData [index] ← AR[t]
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
(cid:132) MemoryErrorException if Memory ECC/Parity Option
Implementation Notes
x ← ceil(log2(InstCacheBytes))
y ← log2(InstCacheBytes ÷ InstCacheWayCount)
z ← log2(InstCacheLineBytes)
The cache line specified by index AR[s]x-1..z in a direct-mapped cache or way 
AR[s]x-1..y and index AR[s]y-1..z in a set-associative cache is the chosen line. If the 
specified cache way is not valid (the fourth way of a three way cache), the instruction 
does nothing. Within the cache line, AR[s]z-1..2 is used to determine which 32-bit 
quantity within the line is written.
The width of the instruction cache RAM may be more than 32 bits depending on the con-
figuration. In that case, some implementations may write the same data replicated 
enough times to fill the entire width of the RAM.
');
INSERT INTO "instructions" VALUES('xtensa', 'SIMCALL', 'Simulator Call
Xtensa Instruction Set Simulator only — illegal in hardware
Assembler Syntax
SIMCALL
Description
SIMCALL is not implemented by any Xtensa processor. Processors raise an illegal 
instruction exception for this opcode. It is implemented by the Xtensa Instruction Set 
Simulator only to allow simulated programs to request services of the simulator host 
processor. See the Xtensa Instruction Set Simulator (ISS) User’s Guide.
The value in address register a2 is the request code. Most codes request host system 
call services while others are used for special purposes such as debugging. Arguments 
needed by host system calls will be found in a3, a4, and a5 and a return code will be 
stored to a2 and an error number to a3.
Operation
See the Xtensa Instruction Set Simulator (ISS) User’s Guide. 
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(IllegalInstructionCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'SLL', 'Shift Left Logical
Assembler Syntax
SLL ar, as
Description
SLL shifts the contents of address register as left by the number of bit positions speci-
fied (as 32 minus number of bit positions) in the SAR (shift amount register) and writes 
the result to address register ar. Typically the SSL or SSA8L instructions are used to 
specify the left shift amount by loading SAR with 32-shift. This transformation allows 
SLL to be implemented in the SRC funnel shifter (which only shifts right), using the SLL 
data as the most significant 32 bits and zero as the least significant 32 bits. Note the 
result of SLL is undefined if SAR > 32.
Operation
sa ← SAR5..0
AR[r] ← (AR[s]||032)31+sa..sa
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SLLI', 'Shift Left Logical Immediate
sa4
sa3..0
Assembler Syntax
SLLI ar, as, 1..31
Description
SLLI shifts the contents of address register as left by a constant amount in the range 
encoded as 32−shift. When the sa field is 0, the result of this instruction is undefined.
Assembler Note
The shift amount is specified in the assembly language as the number of bit positions to 
shift left. The assembler performs the 32-shift calculation when it assembles the in-
struction word. When the immediate operand evaluates to zero, the assembler converts 
this instruction to an OR instruction to effect a register-to-register move. To disable this 
transformation, prefix the mnemonic with an underscore (_SLLI). If imm evaluates to 
zero when the mnemonic has the underscore prefix, the assembler will emit an error.
Operation
AR[r] ← (AR[s]||032)31+sa..sa
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SRA', 'Shift Right Arithmetic
Assembler Syntax
SRA ar, at
Description
SRA arithmetically shifts the contents of address register at right, inserting the sign of 
at on the left, by the number of bit positions specified by SAR (shift amount register) and 
writes the result to address register ar. Typically the SSR or SSA8B instructions are used 
to load SAR with the shift amount from an address register. Note the result of SRA is un-
defined if SAR > 32.
Operation
sa ← SAR5..0
AR[r] ← ((AR[t]31)32||AR[t])31+sa..sa
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SRAI', 'Shift Right Arithmetic Immediate
sa4
sa3..0
Assembler Syntax
SRAI ar, at, 0..31
Description
SRAI arithmetically shifts the contents of address register at right, inserting the sign of 
at on the left, by a constant amount encoded in the instruction word in the range 0..31. 
The shift amount sa field is split, with bits 3..0 in bits 11..8 of the instruction word, 
and bit 4 in bit 20 of the instruction word.
Operation
AR[r] ← ((AR[t]31)32||AR[t])31+sa..sa
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SRC', 'Shift Right Combined
Assembler Syntax
SRC ar, as, at
Description
SRC performs a right shift of the concatenation of address registers as and at by the 
shift amount in SAR. The least significant 32 bits of the shift result are written to address 
register ar. A shift with a wider input than output is called a funnel shift. SRC directly per-
forms right funnel shifts. Left funnel shifts are done by swapping the high and low oper-
ands to SRC and setting SAR to 32 minus the shift amount. The SSL and SSA8B instruc-
tions directly implement such SAR settings. Note the result of SRC is undefined if SAR > 
Operation
sa ← SAR5..0
AR[r] ← (AR[s]||AR[t])31+sa..sa
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SRL', 'Shift Right Logical
Assembler Syntax
SRL ar, at
Description
SRL shifts the contents of address register at right, inserting zeros on the left, by the 
number of bits specified by SAR (shift amount register) and writes the result to address 
register ar. Typically the SSR or SSA8B instructions are used to load SAR with the shift 
amount from an address register. Note the result of SRL is undefined if SAR > 32.
Operation
sa ← SAR5..0
AR[r] ← (032||AR[t])31+sa..sa
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SRLI', 'Shift Right Logical Immediate
sa
Assembler Syntax
SRLI ar, at, 0..15
Description
SRLI shifts the contents of address register at right, inserting zeros on the left, by a 
constant amount encoded in the instruction word in the range 0..15. There is no SRLI 
for shifts ≥ 16. EXTUI replaces these shifts.
Assembler Note
The assembler converts SRLI instructions with a shift amount ≥ 16 into EXTUI. Prefixing 
the SRLI instruction with an underscore (_SRLI) disables this replacement and forces 
the assembler to generate an error.
Operation
AR[r] ← (032||AR[t])31+sa..sa
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SSA8B', 'Set Shift Amount for BE Byte Shift
Assembler Syntax
SSA8B as
Description
SSA8B sets the shift amount register (SAR) for a left shift by multiples of eight (for exam-
ple, for big-endian (BE) byte alignment). The left shift amount is the two least significant 
bits of address register as multiplied by eight. Thirty-two minus this amount is written to 
SAR. Using 32 minus the left shift amount causes a funnel right shift and swapped high 
and low input operands to perform a left shift. SSA8B is similar to SSL, except the shift 
amount is multiplied by eight.
SSA8B is typically used to set up for an SRC instruction to shift bytes. It may be used with 
big-endian byte ordering to extract a 32-bit value from a non-aligned byte address.
Operation
SAR ← 32 − (0||AR[s]1..0||03)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SSA8L', 'Set Shift Amount for LE Byte Shift
Assembler Syntax
SSA8L as
Description
SSA8L sets the shift amount register (SAR) for a right shift by multiples of eight (for ex-
ample, for little-endian (LE) byte alignment). The right shift amount is the two least sig-
nificant bits of address register as multiplied by eight, and is written to SAR. SSA8L is 
similar to SSR, except the shift amount is multiplied by eight.
SSA8L is typically used to set up for an SRC instruction to shift bytes. It may be used with 
little-endian byte ordering to extract a 32-bit value from a non-aligned byte address.
Operation
SAR ← 0||AR[s]1..0||03
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SSAI', 'Set Shift Amount Immediate
sa3..0
sa4
Assembler Syntax
SSAI 0..31
Description
SSAI sets the shift amount register (SAR) to a constant. The shift amount sa field is split, 
with bits 3..0 in bits 11..8 of the instruction word, and bit 4 in bit 4 of the instruction 
word. Because immediate forms exist of most shifts (SLLI, SRLI, SRAI), this is primari-
ly useful to set the shift amount for SRC.
Operation
SAR ← 0||sa
Exceptions
(cid:132)
EveryInst Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SSI', 'Store Single Immediate
Assembler Syntax
SSI ft, as, 0..1020
Description
SSI is a 32-bit store from floating-point register ft to memory. It forms a virtual address 
by adding the contents of address register as and an 8-bit zero-extended constant value 
encoded in the instruction word shifted left by two. Therefore, the offset can specify mul-
tiples of four from zero to 1020. The data to be stored is taken from the contents of float-
ing-point register ft and written to memory at the physical address.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
Assembler Note
To form a virtual address, SSI calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
Store32 (vAddr, FR[t])
Exceptions
(cid:132) Memory Store Group (see page 245)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'SSIU', 'Store Single Immediate with Update
Assembler Syntax
SSIU ft, as, 0..1020
Description
SSIU is a 32-bit store from floating-point register ft to memory with base address regis-
ter update. It forms a virtual address by adding the contents of address register as and 
an 8-bit zero-extended constant value encoded in the instruction word shifted left by two. 
Therefore, the offset can specify multiples of four from zero to 1020. The data to be 
stored is taken from the contents of floating-point register ft and written to memory at 
the physical address. The virtual address is written back to address register as.
If the Region Translation Option (page 156) or the MMU Option (page 158) is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
Assembler Note
To form a virtual address, SSIU calculates the sum of address register as and the imm8 
field of the instruction word times four. Therefore, the machine-code offset is in terms of 
the instruction by dividing by four.
Operation
vAddr ← AR[s] + (022||imm8||02)
Store32 (vAddr, FR[t])
AR[s] ← vAddr
Exceptions
(cid:132) Memory Store Group (see page 245)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'SSL', 'Set Shift Amount for Left Shift
Assembler Syntax
SSL as
Description
SSL sets the shift amount register (SAR) for a left shift (for example, SLL). The left shift 
amount is the 5 least significant bits of address register as. 32 minus this amount is writ-
ten to SAR. Using 32 minus the left shift amount causes a right funnel shift, and swapped 
high and low input operands to perform a left shift.
Operation
sa ← AR[s]4..0
SAR ← 32 − (0||sa)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SSR', 'Set Shift Amount for Right Shift
Assembler Syntax
SSR as
Description
SSR sets the shift amount register (SAR) for a right shift (for example, SRL, SRA, or SRC). 
The least significant five bits of address register as are written to SAR. The most signifi-
cant bit of SAR is cleared. This instruction is similar to a WSR.SAR, but differs in that only 
AR[s]4..0 is used, instead of AR[s]5..0.
Operation
sa ← AR[s]4..0
SAR ← 0||sa
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SSX', 'Store Single Indexed
Assembler Syntax
SSX fr, as, at
Description
SSX is a 32-bit store from floating-point register ft to memory. It forms a virtual address 
by adding the contents of address register as and the contents of address register at. 
The data to be stored is taken from the contents of floating-point register fr and written 
to memory at the physical address.
If the Region Translation Option (page 156) or the MMU Option (page 158)is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
Operation
vAddr ← AR[s] + (AR[t])
Store32 (vAddr, FR[r])
Exceptions
(cid:132) Memory Store Group (see page 245)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'SSXU', 'Store Single Indexed with Update
Assembler Syntax
SSXU fr, as, at
Description
SSXU is a 32-bit store from floating-point register ft to memory with base address regis-
ter update. It forms a virtual address by adding the contents of address register as and 
the contents of address register at. The data to be stored is taken from the contents of 
floating-point register fr and written to memory at the physical address. The virtual ad-
dress is written back to address register as.
If the Region Translation Option (page 156) or the MMU Option (page 158)is enabled, 
the virtual address is translated to the physical address. If not, the physical address is 
identical to the virtual address. If the translation or memory reference encounters an 
error (for example, protection violation or non-existent memory), the processor raises 
one of several exceptions (see Section 4.4.1.5 on page 89).
Without the Unaligned Exception Option (page 99), the two least significant bits of the 
address are ignored. A reference to an address that is not 0 mod 4 produces the same 
result as a reference to the address with the least significant bits cleared. With the Un-
aligned Exception Option, such an access raises an exception.
Operation
vAddr ← AR[s] + (AR[t])
Store32 (vAddr, FR[r])
AR[s] ← vAddr
Exceptions
(cid:132) Memory Store Group (see page 245)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'SUB', 'Subtract
Assembler Syntax
SUB ar, as, at
Description
SUB calculates the two’s complement 32-bit difference of address registers as and at. 
The low 32 bits of the difference are written to address register ar. Arithmetic overflow is 
not detected.
Operation
AR[r] ← AR[s] − AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SUB.S', 'Subtract Single
Assembler Syntax
SUB.S fr, fs, ft
Description
SUB.S computes the IEEE754 single-precision difference of the contents of floating-
point registers fs and ft and writes the result to floating-point register fr.
Operation
FR[r] ← FR[s] −s FR[t]
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'SUBX2', 'Subtract with Shift by 1
Assembler Syntax
SUBX2 ar, as, at
Description
SUBX2 calculates the two’s complement 32-bit difference of address register as shifted 
left by 1 bit and address register at. The low 32 bits of the difference are written to 
address register ar. Arithmetic overflow is not detected.
SUBX2 is frequently used as part of sequences to multiply by small constants.
Operation
AR[r] ← (AR[s]30..0||0) − AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SUBX4', 'Subtract with Shift by 2
Assembler Syntax
SUBX4 ar, as, at
Description
SUBX4 calculates the two’s complement 32-bit difference of address register as shifted 
left by two bits and address register at. The low 32 bits of the difference are written to 
address register ar. Arithmetic overflow is not detected.
SUBX4 is frequently used as part of sequences to multiply by small constants.
Operation
AR[r] ← (AR[s]29..0||02) − AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SUBX8', 'Subtract with Shift by 3
Assembler Syntax
SUBX8 ar, as, at
Description
SUBX8 calculates the two’s complement 32-bit difference of address register as shifted 
left by three bits and address register at. The low 32 bits of the difference are written to 
address register ar. Arithmetic overflow is not detected.
SUBX8 is frequently used as part of sequences to multiply by small constants.
Operation
AR[r] ← (AR[s]28..0||03) − AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'SYSCALL', 'System Call
Exception Option (See Section 4.4.1 on page 82)
Assembler Syntax
SYSCALL
Description
When executed, the SYSCALL instruction raises a system-call exception, redirecting ex-
ecution to an exception vector (see Section 4.4.1 on page 82). Therefore, SYSCALL in-
structions never complete. EPC[1] contains the address of the SYSCALL and ICOUNT is 
not incremented. The system call handler should add 3 to EPC[1] before returning from 
the exception to continue execution.
The program may pass parameters to the system-call handler in the registers. There are 
no bits in SYSCALL instruction reserved for this purpose. See Section 8.2.2 “System 
Calls” on page 597 for a description of software conventions for system call parameters.
Operation
Exception (SyscallCause)
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(SyscallCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'TRUNC.S', 'Truncate Single to Fixed
Assembler Syntax
TRUNC.S ar, fs, 0..15
Description
TRUNC.S converts the contents of floating-point register fs from single-precision to 
signed integer format, rounding toward 0. The single-precision value is first scaled by a 
power of two constant value encoded in the t field, with 0..15 representing 1.0, 2.0, 4.0, 
…, 32768.0. The scaling allows for a fixed point notation where the binary point is at the 
right end of the integer for t=0, and moves to the left as t increases until for t=15 there 
are 15 fractional bits represented in the fixed point number. For positive overflow (value 
≥ 32''h7fffffff), positive infinity, or NaN, 32''h7fffffff is returned; for negative 
overflow (value ≤ 32''h80000000) or negative infinity, 32''h80000000 is returned. The 
result is written to address register ar.
Operation
AR[r] ← truncs(FR[s] ×s pows(2.0,t))
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'UEQ.S', 'Compare Single Unordered or Equal
Assembler Syntax
UEQ.S br, fs, ft
Description
UEQ.S compares the contents of floating-point registers fs and ft. If the values are 
equal or unordered then Boolean register br is set to 1, otherwise br is set to 0. Accord-
ing to IEEE754, +0 and −0 compare as equal. IEEE754 floating-point values are 
unordered if either of them is a NaN.
Operation
BRr ← isNaN(FR[s]) or isNaN(FR[t]) or (FR[s] =s FR[t])
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'UFLOAT.S', 'Convert Unsigned Fixed to Single
Assembler Syntax
UFLOAT.S fr, as, 0..15
Description
UFLOAT.S converts the contents of address register as from unsigned integer to single-
precision format, rounding according to the current rounding mode. The converted inte-
ger value is then scaled by a power of two constant value encoded in the t field, with 
notation where the binary point is at the right end of the integer for t=0, and moves to 
the left as t increases until for t=15 there are 15 fractional bits represented in the fixed 
point number. The result is written to floating-point register fr.
Operation
FR[r] ← ufloats(AR[s]) ×s pows(2.0,-t))
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'ULE.S', 'Compare Single Unord or Less Than or Equal 
Assembler Syntax
ULE.S br, fs, ft
Description
ULE.S compares the contents of floating-point registers fs and ft. If the contents of fs 
are less than or equal to or unordered with the contents of ft, then Boolean register br 
is set to 1, otherwise br is set to 0. IEEE754 specifies that +0 and −0 compare as equal. 
IEEE754 floating-point values are unordered if either of them is a NaN.
Operation
BRr ← isNaN(FR[s]) or isNaN(FR[t]) or (FR[s] ≤s FR[t])
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'ULT.S', 'Compare Single Unordered or Less Than
Assembler Syntax
ULT.S br, fs, ft
Description
ULT.S compares the contents of floating-point registers fs and ft. If the contents of fs 
are less than or unordered with the contents of ft, then Boolean register br is set to 1, 
otherwise br is set to 0. IEEE754 specifies that +0 and −0 compare as equal. IEEE754 
floating-point values are unordered if either of them is a NaN.
Operation
BRr ← isNaN(FR[s]) or isNaN(FR[t]) or (FR[s] <s FR[t])
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'UMUL.AA.*', 'Unsigned Multiply
half
MAC16 Option (See Section 4.3.7 on page 60)
Assembler Syntax
UMUL.AA.* as, at
Where * expands as follows:
UMUL.AA.LL - for (half=0) 
UMUL.AA.HL - for (half=1) 
UMUL.AA.LH - for (half=2) 
UMUL.AA.HH - for (half=3) 
Description
UMUL.AA.* performs an unsigned multiply of half of each of the address registers as 
and at, producing a 32-bit result. The result is zero-extended to 40 bits and written to 
the MAC16 accumulator.
Operation
m1 ← if half0 then AR[s]31..16 else AR[s]15..0
m2 ← if half1 then AR[t]31..16 else AR[t]15..0
ACC ← (024||m1) × (024||m2)
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'UN.S', 'Compare Single Unordered
Assembler Syntax
UN.S br, fs, ft
Description
UN.S sets Boolean register br to 1 if the contents of either floating-point register fs or 
ft is a IEEE754 NaN; otherwise br is set to 0.
Operation
BRr ← isNaN(FR[s]) or isNaN(FR[t])
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'UTRUNC.S', 'Truncate Single to Fixed Unsigned
Assembler Syntax
UTRUNC.S ar, fs, 0..15
Description
UTRUNC.S converts the contents of floating-point register fs from single-precision to 
unsigned integer format, rounding toward 0. The single-precision value is first scaled by 
a power of two constant value encoded in the t field, with 0..15 representing 1.0, 2.0, 
the right end of the integer for t=0, and moves to the left as t increases until for t=15 
there are 15 fractional bits represented in the fixed point number. For positive overflow 
(value ≥ 32''hffffffff), positive infinity, or NaN, 32''hffffffff is returned; for neg-
ative numbers or negative infinity, 32''h80000000 is returned. The result is written to 
address register ar.
Operation
AR[r] ← utruncs(FR[s] ×s pows(2.0,t))
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'WAITI', 'Wait for Interrupt
imm4
Interrupt Option (See Section 4.4.4 on page 100)
Assembler Syntax
WAITI 0..15
Description
WAITI sets the interrupt level in PS.INTLEVEL to imm4 and then, on some Xtensa ISA 
implementations, suspends processor operation until an interrupt occurs. WAITI is typi-
cally used in an idle loop to reduce power consumption. CCOUNT continues to increment 
during suspended operation, and a CCOMPARE interrupt will wake the processor.
When an interrupt is taken during suspended operation, EPC[i] will have the address 
of the instruction following WAITI. An implementation is not required to enter suspended 
operation and may leave suspended operation and continue execution at the following 
instruction at any time. Usually, therefore, the WAITI instruction should be within a loop.
The combination of setting the interrupt level and suspending operation avoids a race 
condition where an interrupt between the interrupt level setting and the suspension of 
operation would be ignored until a second interrupt occurred.
WAITI is a privileged instruction.
Operation
Exception (PrivilegedInstructionCause)
PS.INTLEVEL ← imm4
if CRING ≠ 0 then
else
endif
Exceptions
(cid:132)
EveryInst Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'WDTLB', 'Write Data TLB Entry
Region Translation Option (page 156) or the MMU Option (page 158)
Assembler Syntax
WDTLB at, as
Description
WDTLB uses the contents of address register as to specify a data TLB entry and writes 
the contents of address register at into that entry. See Section 4.6 on page 138 for in-
formation on the address and result register formats for specific memory protection and 
translation options. The point at which the data TLB write is effected is implementation-
specific. Any translation that would be affected by this write before the execution of a 
DSYNC instruction is therefore undefined.
WDTLB is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
(vpn, ei, wi) ← SplitDataTLBEntrySpec(AR[s])
(ppn, sr, ring, ca) ← SplitDataEntry(wi, AR[t])
DataTLB[wi][ei].ASID ← ASID(ring)
DataTLB[wi][ei].VPN ← vpn
DataTLB[wi][ei].PPN ← ppn
DataTLB[wi][ei].SR ← sr
DataTLB[wi][ei].CA ← ca
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'WER', 'Write External Register
Assembler Syntax
WER at, as
Description
WER writes one of a set of "External Registers". It is in some ways similar to the WSR.* 
instruction except that the registers being written are not defined by the Xtensa ISA and 
are conceptually outside the processor core. They are written through processor ports.
Address register as is used to determine which register is to be written and address reg-
ister at provides the write data. When no External Register is addressed by the value in 
address register as, no write occurs. The entire address space is reserved for use by 
Tensilica. RER and WER are managed by the processor core so that the requests appear 
on the processor ports in program order. External logic is responsible for extending that 
order to the registers themselves.
WER is a privileged instruction.
Operation
Exception (PrivilegedInstructionCause)
Write External Register as defined outside the processor.
if CRING ≠ 0 then
else
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'WFR', 'Move AR to FR
Assembler Syntax
WFR fr, as
Description
WFR moves the contents of address register as to floating-point register fr. The move is 
non-arithmetic; no floating-point exceptions are raised.
Operation
FR[r] ← AR[s]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor0Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'WITLB', 'Write Instruction TLB Entry
Region Translation Option (page 156) or the MMU Option (page 158)
Assembler Syntax
WITLB at, as
Description
WITLB uses the contents of address register as to specify an instruction TLB entry and 
writes the contents of address register at into that entry. See Section 4.6 on page 138 
for information on the address and result register formats for specific memory protection 
and translation options. The point at which the instruction TLB write is effected is imple-
mentation-specific. Any translation that would be affected by this write before the execu-
tion of an ISYNC instruction is therefore undefined.
WITLB is a privileged instruction.
Operation
if CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
(vpn, ei, wi) ← SplitInstTLBEntrySpec(AR[s])
(ppn, sr, ring, ca) ← SplitInstEntry(wi, AR[t])
InstTLB[wi][ei].ASID ← ASID(ring)
InstTLB[wi][ei].VPN ← vpn
InstTLB[wi][ei].PPN ← ppn
InstTLB[wi][ei].SR ← sr
InstTLB[wi][ei].CA ← ca
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'WSR.*', 'Write Special Register
sr
Assembler Syntax
WSR.* at
WSR at, *
WSR at, 0..255
Description
WSR.* writes the special registers that are described in Section 3.8.10 “Processor Con-
trol Instructions” on page 45. See Section 5.3 on page 208 for more detailed information 
on the operation of this instruction for each Special Register.
The contents of address register at are written to the special register designated by the 
of the ‘*’ in the assembler syntax above and the translation is made to the 8-bit sr field 
by the assembler.
WSR is an assembler macro for WSR.* that provides compatibility with the older versions 
of the instruction containing either the name or the number of the Special Register.
The point at which WSR.* to certain registers affects subsequent instructions is not al-
ways defined (SAR and ACC are exceptions). In these cases, the Special Register Tables 
in Section 5.3 on page 208 explain how to ensure the effects are seen by a particular 
point in the instruction stream (typically involving the use of one of the ISYNC, RSYNC, 
ESYNC, or DSYNC instructions). A WSR.* followed by an RSR.* to the same register 
should be separated with an ESYNC to guarantee the value written is read back. A 
WSR.PS followed by RSIL also requires an ESYNC.
WSR.* with Special Register numbers ≥ 64 is privileged. A WSR.* for an unconfigured 
register generally will raise an illegal instruction exception.
Operation
Write Special Register
Exception (PrivilegedInstructionCause)
sr ← if msbFirst then s||r else r||s
if sr ≥ 64 and CRING ≠ 0 then
else
endif
Exceptions
see the Special Register Tables in Section 5.3 on page 208
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(IllegalInstructionCause) if Exception Option
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
INSERT INTO "instructions" VALUES('xtensa', 'WUR.*', 'Write User Register
sr
No Option - instructions created from the TIE language (See Section 4.3.9.2 “Coproces-
sor Context Switch” on page 64)
Assembler Syntax
WUR.* at
WUR at,*
Description
WUR.* writes TIE state that has been grouped into 32-bit quantities by the TIE 
user_register statement. The name in the user_register statement replaces the 
“*” in the instruction name and causes the correct register number to be placed in the st 
field of the encoded instruction. The contents of address register at are written to the 
TIE user_register designated by the 8-bit sr field of the instruction word.
WUR is an assembler macro for WUR.* that provides compatibility with the older version 
of the instruction.
Operation
user_register[sr] ← AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(Coprocessor*Disabled) if Coprocessor Option
');
INSERT INTO "instructions" VALUES('xtensa', 'XOR', 'Bitwise Logical Exclusive Or
Assembler Syntax
XOR ar, as, at
Description
XOR calculates the bitwise logical exclusive or of address registers as and at. The 
result is written to address register ar.
Operation
AR[r] ← AR[s] xor AR[t]
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'XORB', 'Boolean Exclusive Or
Assembler Syntax
XORB br, bs, bt
Description
XORB performs the logical exclusive or of Boolean registers bs and bt and writes the 
result to Boolean register br.
When the sense of one of the source Booleans is inverted (0 → true, 1 → false), use an 
inverted test of the result. When the sense of both of the source Booleans is inverted, 
use a non-inverted test of the result.
Operation
BRr ← BRs xor BRt
Exceptions
(cid:132)
EveryInstR Group (see page 244)
');
INSERT INTO "instructions" VALUES('xtensa', 'XSR.*', 'Exchange Special Register
sr
Assembler Syntax
XSR.* at
XSR at, *
XSR at, 0..255
Description
XSR.* simultaneously reads and writes the special registers that are described in 
Section 3.8.10 “Processor Control Instructions” on page 45. See Section 5.3 on 
page 208 for more detailed information on the operation of this instruction for each 
Special Register.
The contents of address register at and the Special Register designated by the immedi-
ate in the 8-bit sr field of the instruction word are both read. The read address register 
value is then written to the Special Register, and the read Special Register value is writ-
ten to at. The name of the Special Register is used in place of the ‘*’ in the assembler 
syntax above and the translation is made to the 8-bit sr field by the assembler.
XSR is an assembler macro for XSR.*, which provides compatibility with the older ver-
sions of the instruction containing either the name or the number of the Special Register.
The point at which XSR.* to certain registers affects subsequent instructions is not al-
ways defined (SAR and ACC are exceptions). In these cases, the Special Register Tables 
in Section 5.3 on page 208 explain how to ensure the effects are seen by a particular 
point in the instruction stream (typically involving the use of one of the ISYNC, RSYNC, 
ESYNC, or DSYNC instructions). An XSR.* followed by an RSR.* to the same register 
should be separated with an ESYNC to guarantee the value written is read back. An 
XSR.PS followed by RSIL also requires an ESYNC. In general, the restrictions on XSR.* 
include the union of the restrictions of the corresponding RSR.* and WSR.*.
XSR.* with Special Register numbers ≥ 64 is privileged. An XSR.* for an unconfigured 
register generally will raise an illegal instruction exception.
Operation
sr ← if msbFirst then s||r else r||s
if sr ≥ 64 and CRING ≠ 0 then
else
Exception (PrivilegedInstructionCause)
t0 ← AR[t]
t1 ← see RSR frame of the Tables in Section 5.3 on page 208
see WSR frame of the Tables in Section 5.3 on page 208 ← t0
AR[t] ← t1
endif
Exceptions
(cid:132)
EveryInstR Group (see page 244)
(cid:132) GenExcep(IllegalInstructionCause) if Exception Option
(cid:132) GenExcep(PrivilegedCause) if Exception Option
');
COMMIT;
