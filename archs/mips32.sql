PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE instructions (platform TEXT, mnem TEXT, description TEXT);
INSERT INTO "instructions" VALUES('MIPS32','ABS.fmt','


Floating Point Absolute Value                                                                                   ABS.fmt


      31             26 25                21 20              16 15              11 10           6   5                  0

           COP1                                      0                                                      ABS
                                 fmt                                    fs              fd
           010001                                  00000                                                  000101

              6                   5                  5                   5              5                    6


        Format: ABS.S fd, fs                                                                         MIPS32 (MIPS I)
                  ABS.D fd, fs                                                                       MIPS32 (MIPS I)

        Purpose:

        To compute the absolute value of an FP value

        Description: fd <- abs(fs)

        The absolute value of the value in FPR fs is placed in FPR fd. The operand and result are values in format fmt. Cause
        bits are ORed into the Flag bits if no exception is taken.

        This operation is arithmetic; a NaN operand signals invalid operation.

        Restrictions:

        The fields fs and fd must specify FPRs valid for operands of type fmt. If they are not valid, the result is UNPRE-
        DICTABLE.

        The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value of the operand
        FPR becomes UNPREDICTABLE.

        Operation:

             StoreFPR(fd, fmt, AbsoluteValue(ValueFPR(fs, fmt)))

        Exceptions:

        Coprocessor Unusable, Reserved Instruction

        Floating Point Exceptions:

        Unimplemented Operation, Invalid Operation');
INSERT INTO "instructions" VALUES('MIPS32','ADD','


Add Word                                                                                                          ADD


  31              26 25                21 20            16 15               11 10            6   5                 0

      SPECIAL                                                                        0                    ADD
                               rs                 rt                rd
       000000                                                                      00000                 100000

         6                     5                  5                 5                5                     6


    Format: ADD rd, rs, rt                                                                        MIPS32 (MIPS I)

    Purpose:

    To add 32-bit integers. If an overflow occurs, then trap.

    Description: rd <- rs + rt

    The 32-bit word value in GPR rt is added to the 32-bit value in GPR rs to produce a 32-bit result.

       . If the addition results in 32-bit 2''s complement arithmetic overflow, the destination register is not modified and
         an Integer Overflow exception occurs.

       . If the addition does not overflow, the 32-bit result is placed into GPR rd.

    Restrictions:

    None

    Operation:

        temp <- (GPR[rs]31||GPR[rs]31..0) + (GPR[rt]31||GPR[rt]31..0)
        if temp
                  32  != temp31 then
             SignalException(IntegerOverflow)
        else
             GPR[rd] <- temp
        endif

    Exceptions:

    Integer Overflow

    Programming Notes:

    ADDU performs the same arithmetic operation but does not trap on overflow.');
INSERT INTO "instructions" VALUES('MIPS32','ADD.fmt','


Floating Point Add                                                                                                  ADD.fmt


      31              26 25               21 20              16 15            11 10               6   5                  0

           COP1                                                                                               ADD
                                 fmt                 ft               fs                  fd
           010001                                                                                            000000

              6                   5                  5                 5                   5                    6


        Format: ADD.S fd, fs, ft                                                                       MIPS32 (MIPS I)
                   ADD.D fd, fs, ft                                                                    MIPS32 (MIPS I)

        Purpose:

        To add floating point values

        Description: fd <- fs + ft

        The value in FPR ft is added to the value in FPR fs. The result is calculated to infinite precision, rounded by using to
        the current rounding mode in FCSR, and placed into FPR fd. The operands and result are values in format fmt. Cause
        bits are ORed into the Flag bits if no exception is taken.

        Restrictions:

        The fields fs, ft, and fd must specify FPRs valid for operands of type fmt. If they are not valid, the result is UNPRE-
        DICTABLE.

        The operands must be values in format fmt; if they are not, the result is UNPREDICTABLE and the value of the
        operand FPRs becomes UNPREDICTABLE.

        Operation:

             StoreFPR (fd, fmt, ValueFPR(fs, fmt) +fmt ValueFPR(ft, fmt))

        Exceptions:

        Coprocessor Unusable, Reserved Instruction

        Floating Point Exceptions:

        Unimplemented Operation, Invalid Operation, Inexact, Overflow, Underflow');
INSERT INTO "instructions" VALUES('MIPS32','ADDI','


Add Immediate Word                                                                                                ADDI


   31             26 25                21 20             16 15                                                     0

        ADDI
                               rs                  rt                              immediate
        001000

          6                    5                   5                                   16


     Format: ADDI rt, rs, immediate                                                               MIPS32 (MIPS I)

     Purpose:

     To add a constant to a 32-bit integer. If overflow occurs, then trap.

     Description: rt <- rs + immediate

     The 16-bit signed immediate is added to the 32-bit value in GPR rs to produce a 32-bit result.

       . If the addition results in 32-bit 2''s complement arithmetic overflow, the destination register is not modified and
         an Integer Overflow exception occurs.

       . If the addition does not overflow, the 32-bit result is placed into GPR rt.

     Restrictions:

     None

     Operation:

         temp <- (GPR[rs]31||GPR[rs]31..0) + sign_extend(immediate)
         if temp
                  32  != temp31 then
             SignalException(IntegerOverflow)
         else
             GPR[rt] <- temp
         endif

     Exceptions:

     Integer Overflow

     Programming Notes:

     ADDIU performs the same arithmetic operation but does not trap on overflow.');
INSERT INTO "instructions" VALUES('MIPS32','ADDIU','


Add Immediate Unsigned Word                                                                                      ADDIU


    31             26 25                21 20            16 15                                                       0

        ADDIU
                               rs                rt                                 immediate
         001001

           6                    5                5                                      16


      Format: ADDIU rt, rs, immediate                                                              MIPS32 (MIPS I)

      Purpose:

      To add a constant to a 32-bit integer

      Description: rt <- rs + immediate

      The 16-bit signed immediate is added to the 32-bit value in GPR rs and the 32-bit arithmetic result is placed into
      GPR rt.

      No Integer Overflow exception occurs under any circumstances.

      Restrictions:

      None

      Operation:

          temp <- GPR[rs] + sign_extend(immediate)
          GPR[rt]<- temp

      Exceptions:

      None

      Programming Notes:

      The term !=unsigned!= in the instruction name is a misnomer; this operation is 32-bit modulo arithmetic that does not
      trap on overflow. This instruction is appropriate for unsigned arithmetic, such as address arithmetic, or integer arith-
      metic environments that ignore overflow, such as C language arithmetic.');
INSERT INTO "instructions" VALUES('MIPS32','ADDU','


Add Unsigned Word                                                                                                ADDU


   31             26 25              21 20              16 15            11 10               6   5                  0

       SPECIAL                                                                       0                 ADDU
                              rs                rt                 rd
        000000                                                                     00000               100001

          6                   5                 5                  5                 5                    6


     Format: ADDU rd, rs, rt                                                                      MIPS32 (MIPS I)

     Purpose:

     To add 32-bit integers

     Description: rd <- rs + rt

     The 32-bit word value in GPR rt is added to the 32-bit value in GPR rs and the 32-bit arithmetic result is placed into
     GPR rd.

     No Integer Overflow exception occurs under any circumstances.

     Restrictions:

     None

     Operation:

         temp <- GPR[rs] + GPR[rt]
         GPR[rd] <- temp

     Exceptions:

     None

     Programming Notes:

     The term !=unsigned!= in the instruction name is a misnomer; this operation is 32-bit modulo arithmetic that does not
     trap on overflow. This instruction is appropriate for unsigned arithmetic, such as address arithmetic, or integer arith-
     metic environments that ignore overflow, such as C language arithmetic.');
INSERT INTO "instructions" VALUES('MIPS32','AND','


And                                                                                                        AND


  31              26 25            21 20            16 15            11 10               6 5                 0

     SPECIAL                                                                      0              AND
                             rs               rt              rd
      000000                                                                   00000             100100

         6                   5                5               5                   5                 6


    Format:     AND rd, rs, rt                                                              MIPS32 (MIPS I)

    Purpose:

    To do a bitwise logical AND

    Description: rd <- rs AND rt

    The contents of GPR rs are combined with the contents of GPR rt in a bitwise logical AND operation. The result is
    placed into GPR rd.

    Restrictions:

    None

    Operation:

        GPR[rd] <- GPR[rs] and GPR[rt]

    Exceptions:

    None');
INSERT INTO "instructions" VALUES('MIPS32','ANDI','


And Immediate                                                                                                    ANDI


   31             26 25                21 20            16 15                                                     0

        ANDI
                               rs                rt                               immediate
       001100

          6                     5                5                                    16


     Format: ANDI rt, rs, immediate                                                              MIPS32 (MIPS I)

     Purpose:

     To do a bitwise logical AND with a constant

     Description: rt <- rs AND immediate

     The 16-bit immediate is zero-extended to the left and combined with the contents of GPR rs in a bitwise logical AND
     operation. The result is placed into GPR rt.

     Restrictions:

     None

     Operation:

         GPR[rt] <- GPR[rs] and zero_extend(immediate)

     Exceptions:

     None');
INSERT INTO "instructions" VALUES('MIPS32','B','


Unconditional Branch                                                                                                     B


  31               26 25               21 20              16 15                                                         0

         BEQ                    0                  0
                                                                                         offset
        000100               00000               00000

           6                    5                  5                                      16


     Format: B offset                                                                                  Assembly Idiom

     Purpose:

     To do an unconditional branch

     Description: branch

     B offset is the assembly idiom used to denote an unconditional branch. The actual instruction is interpreted by the
     hardware as BEQ r0, r0, offset.

     An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
     the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

     Restrictions:

     Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
     delay slot of a branch or jump.

     Operation:

         I:        target_offset <- sign_extend(offset || 0 )               2

         I+1:      PC <- PC + target_offset

     Exceptions:

     None

     Programming Notes:

     With the 18-bit signed instruction offset, the conditional branch range is +- 128 Kbytes. Use jump (J) or jump register
     (JR) instructions to branch to addresses outside this range.');
INSERT INTO "instructions" VALUES('MIPS32','BAL','


Branch and Link                                                                                                      BAL


   31               26 25               21 20              16 15                                                         0

       REGIMM                    0               BGEZAL
                                                                                          offset
        000001                00000               10001

           6                     5                  5                                      16


      Format: BAL rs, offset                                                                            Assembly Idiom

      Purpose:

      To do an unconditional PC-relative procedure call

      Description: procedure_call

      BAL offset is the assembly idiom used to denote an unconditional branch. The actual instruction is iterpreted by the
      hardware as BGEZAL r0, offset.

      Place the return address link in GPR 31. The return link is the address of the second instruction following the branch,
      where execution continues after a procedure call.

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      GPR 31 must not be used for the source register rs, because such an instruction does not have the same effect when
      reexecuted. The result of executing such an instruction is UNPREDICTABLE. This restriction permits an exception
      handler to resume execution by reexecuting the branch when an exception occurs in the branch delay slot.

      Operation:

          I:        target_offset <- sign_extend(offset || 0 )               2

                    GPR[31] <- PC + 8
          I+1:      PC <- PC + target_offset

      Exceptions:

      None

      Programming Notes:

      With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump and link (JAL) or
      jump and link register (JALR) instructions for procedure calls to addresses outside this range.');
INSERT INTO "instructions" VALUES('MIPS32','BC1F','


Branch on FP False                                                                                                       BC1F


   31               26 25               21 20      18 17 16 15                                                           0

          COP1                  BC                     nd tf
                                                cc                                        offset
         010001               01000                    0   0

            6                    5              3      1   1                               16


      Format: BC1F         offset (cc = 0 implied)                                                     MIPS32 (MIPS I)
                  BC1F     cc, offset                                                                MIPS32 (MIPS IV)

      Purpose:

      To test an FP condition code and do a PC-relative conditional branch

      Description: if cc = 0 then branch

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself) in the branch delay slot to form a PC-relative effective target address. If the FP con-
      dition code bit CC is false (0), the program branches to the effective target address after the instruction in the delay
      slot is executed. An FP condition code is set by the FP compare instruction, C.cond.fmt.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

      This operation specification is for the general Branch On Condition operation with the tf (true/false) and nd (nullify
      delay slot) fields as variables. The individual instructions BC1F, BC1FL, BC1T, and BC1TL have specific values for
      tf and nd.
           I:       condition <- FPConditionCode(cc) = 0
                    target_offset <- (offset15)          GPRLEN-(16+2)    || offset || 0     2

           I+1:     if condition then
                         PC <- PC + target_offset
                    endif

    Exceptions:

    Coprocessor Unusable, Reserved Instruction

    Floating Point Exceptions:

    Unimplemented Operation

    Programming Notes:

    With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
    (JR) instructions to branch to addresses outside this range

    Historical Information:

    The MIPS I architecture defines a single floating point condition code, implemented as the coprocessor 1 condition
    signal (Cp1Cond) and the C bit in the FP Control/Status register. MIPS I, II, and III architectures must have the CC
    field set to 0, which is implied by the first format in the !=Format!= section.

    The MIPS IV and MIPS32 architectures add seven more Condition Code bits to the original condition code 0. FP
    compare and conditional branch instructions specify the Condition Code bit to set or test. Both assembler formats are
    valid for MIPS IV and MIPS32.

    In the MIPS I, II, and III architectures there must be at least one instruction between the compare instruction that sets
    the condition code and the branch instruction that tests it. Hardware does not detect a violation of this restriction.');
INSERT INTO "instructions" VALUES('MIPS32','BC1FL','


Branch on FP False Likely                                                                                               BC1FL


    31               26 25               21 20       18 17 16 15                                                          0

          COP1                   BC                      nd tf
                                                 cc                                          offset
          010001               01000                     1   0

            6                     5              3       1   1                                16


      Format: BC1FL           offset (cc = 0 implied)                                                 MIPS32 (MIPS II)
              BC1FL           cc, offset                                                              MIPS32 (MIPS IV)

      Purpose:

      To test an FP condition code and make a PC-relative conditional branch; execute the instruction in the delay slot only
      if the branch is taken.

      Description: if cc = 0 then branch_likely

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself) in the branch delay slot to form a PC-relative effective target address. If the FP Con-
      dition Code bit CC is false (0), the program branches to the effective target address after the instruction in the delay
      slot is executed. If the branch is not taken, the instruction in the delay slot is not executed.

      An FP condition code is set by the FP compare instruction, C.cond.fmt.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

      This operation specification is for the general Branch On Condition operation with the tf (true/false) and nd (nullify
      delay slot) fields as variables. The individual instructions BC1F, BC1FL, BC1T, and BC1TL have specific values for
      tf and nd.
           I:       condition <- FPConditionCode(cc) = 0
                    target_offset <- (offset15)            GPRLEN-(16+2)    || offset || 0      2

           I+1:     if condition then
                         PC <- PC + target_offset
                    else
                         NullifyCurrentInstruction()
                    endif
    Exceptions:

    Coprocessor Unusable, Reserved Instruction

    Floating Point Exceptions:

    Unimplemented Operation

    Programming Notes:

    With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
    (JR) instructions to branch to addresses outside this range.

    Software is strongly encouraged to avoid the use of the Branch Likely instructions, as they will be removed from a
    future revision of the MIPS Architecture.

    Some implementations always predict the branch will be taken, so there is a significant penalty if the branch is not
    taken. Software should only use this instruction when there is a very high probability (98% or more) that the branch
    will be taken. If the branch is not likely to be taken or if the probability of a taken branch is unknown, software is
    encouraged to use the BC1F instruction instead.

    Historical Information:

    The MIPS I architecture defines a single floating point condition code, implemented as the coprocessor 1 condition
    signal (Cp1Cond) and the C bit in the FP Control/Status register. MIPS I, II, and III architectures must have the CC
    field set to 0, which is implied by the first format in the !=Format!= section.

    The MIPS IV and MIPS32 architectures add seven more Condition Code bits to the original condition code 0. FP
    compare and conditional branch instructions specify the Condition Code bit to set or test. Both assembler formats are
    valid for MIPS IV and MIPS32.

    In the MIPS II andIII architectionrs there must be at least one instruction between the compare instruction that
    sets a condition code and the branch instruction that tests it. Hardware does not detect a violation of this restriction.');
INSERT INTO "instructions" VALUES('MIPS32','BC1T','


Branch on FP True                                                                                                        BC1T


   31               26 25                21 20     18 17 16 15                                                            0

          COP1                  BC                     nd tf
                                                cc                                        offset
         010001               01000                    0   1

            6                    5              3      1   1                               16


      Format: BC1T offset (cc = 0 implied)                                                              MIPS32 (MIPS I)
              BC1T cc, offset                                                                           MIPS32 (MIPS IV)

      Purpose:

      To test an FP condition code and do a PC-relative conditional branch

      Description: if cc = 1 then branch

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself) in the branch delay slot to form a PC-relative effective target address. If the FP con-
      dition code bit CC is true (1), the program branches to the effective target address after the instruction in the delay slot
      is executed. An FP condition code is set by the FP compare instruction, C.cond.fmt.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

      This operation specification is for the general Branch On Condition operation with the tf (true/false) and nd (nullify
      delay slot) fields as variables. The individual instructions BC1F, BC1FL, BC1T, and BC1TL have specific values for
      tf and nd.
           I:       condition <- FPConditionCode(cc) = 1
                    target_offset <- (offset15)          GPRLEN-(16+2)    || offset || 0      2

           I+1:     if condition then
                         PC <- PC + target_offset
                    endif
    Exceptions:

    Coprocessor Unusable, Reserved Instruction

    Floating Point Exceptions:

    Unimplemented Operation

    Programming Notes:

    With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
    (JR) instructions to branch to addresses outside this range.

    Historical Information:

    The MIPS I architecture defines a single floating point condition code, implemented as the coprocessor 1 condition
    signal (Cp1Cond) and the C bit in the FP Control/Status register. MIPS I, II, and III architectures must have the CC
    field set to 0, which is implied by the first format in the !=Format!= section.

    The MIPS IV and MIPS32 architectures add seven more Condition Code bits to the original condition code 0. FP
    compare and conditional branch instructions specify the Condition Code bit to set or test. Both assembler formats are
    valid for MIPS IV and MIPS32.

    In the MIPS I, II, and III architectures there must be at least one instruction between the compare instruction that sets
    the condition code and the branch instruction that tests it. Hardware does not detect a violation of this restriction.');
INSERT INTO "instructions" VALUES('MIPS32','BC1TL','


Branch on FP True Likely                                                                                               BC1TL


    31              26 25                21 20       18 17 16 15                                                          0

          COP1                   BC                      nd tf
                                                 cc                                          offset
         010001                01000                     1   1

            6                     5              3       1   1                                16


      Format: BC1TL           offset (cc = 0 implied)                                                 MIPS32 (MIPS II)
                  BC1TL       cc, offset                                                              MIPS32 (MIPS IV)

      Purpose:

      To test an FP condition code and do a PC-relative conditional branch; execute the instruction in the delay slot only if
      the branch is taken.

      Description: if cc = 1 then branch_likely

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself) in the branch delay slot to form a PC-relative effective target address. If the FP Con-
      dition Code bit CC is true (1), the program branches to the effective target address after the instruction in the delay
      slot is executed. If the branch is not taken, the instruction in the delay slot is not executed.

      An FP condition code is set by the FP compare instruction, C.cond.fmt.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

      This operation specification is for the general Branch On Condition operation with the tf (true/false) and nd (nullify
      delay slot) fields as variables. The individual instructions BC1F, BC1FL, BC1T, and BC1TL have specific values for
      tf and nd.
           I:       condition <- FPConditionCode(cc) = 1
                    target_offset <- (offset15)            GPRLEN-(16+2)    || offset || 0      2

           I+1:     if condition then
                         PC <- PC + target_offset
                    else
                         NullifyCurrentInstruction()
                    endif
    Exceptions:

    Coprocessor Unusable, Reserved Instruction

    Floating Point Exceptions:

    Unimplemented Operation

    Programming Notes:

    With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
    (JR) instructions to branch to addresses outside this range.

    Software is strongly encouraged to avoid the use of the Branch Likely instructions, as they will be removed from a
    future revision of the MIPS Architecture.

    Some implementations always predict the branch will be taken, so there is a significant penalty if the branch is not
    taken. Software should only use this instruction when there is a very high probability (98% or more) that the branch
    will be taken. If the branch is not likely to be taken or if the probability of a taken branch is unknown, software is
    encouraged to use the BC1T instruction instead.

    Historical Information:

    The MIPS I architecture defines a single floating point condition code, implemented as the coprocessor 1 condition
    signal (Cp1Cond) and the C bit in the FP Control/Status register. MIPS I, II, and III architectures must have the CC
    field set to 0, which is implied by the first format in the !=Format!= section.

    The MIPS IV and MIPS32 architectures add seven more Condition Code bits to the original condition code 0. FP
    compare and conditional branch instructions specify the Condition Code bit to set or test. Both assembler formats are
    valid for MIPS IV and MIPS32.

    In the MIPS II andIII architectionrs there must be at least one instruction between the compare instruction that
    sets a condition code and the branch instruction that tests it. Hardware does not detect a violation of this restriction.');
INSERT INTO "instructions" VALUES('MIPS32','BC2F','


Branch on COP2 False                                                                                                   BC2F


   31                26 25             21 20       18 17 16 15                                                           0

          COP2                  BC                     nd tf
                                                cc                                        offset
         010010               01000                    0   0

            6                    5              3      1   1                               16


      Format: BC2F         offset (cc = 0 implied)                                                     MIPS32 (MIPS I)
                  BC2F     cc, offset                                                               MIPS32 (MIPS IV)

      Purpose:

      To test a COP2 condition code and do a PC-relative conditional branch

      Description: if cc = 0 then branch

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself) in the branch delay slot to form a PC-relative effective target address. If the COP2
      condition specified by CC is false (0), the program branches to the effective target address after the instruction in the
      delay slot is executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

      This operation specification is for the general Branch On Condition operation with the tf (true/false) and nd (nullify
      delay slot) fields as variables. The individual instructions BC2F, BC2FL, BC2T, and BC2TL have specific values for
      tf and nd.
           I:       condition <- COP2Condition(cc) = 0
                    target_offset <- (offset15)          GPRLEN-(16+2)    || offset || 0     2

           I+1:     if condition then
                         PC <- PC + target_offset
                    endif

      Exceptions:

      Coprocessor Unusable, Reserved Instruction

      Programming Notes:

      With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
      (JR) instructions to branch to addresses outside this range.');
INSERT INTO "instructions" VALUES('MIPS32','BC2FL','


Branch on COP2 False Likely                                                                                            BC2FL


    31               26 25              21 20       18 17 16 15                                                          0

          COP2                  BC                     nd tf
                                                cc                                          offset
         010010                01000                    1   0

            6                     5             3       1   1                                 16


      Format: BC2FL          offset (cc = 0 implied)                                                   MIPS32 (MIPS II)
                  BC2FL      cc, offset                                                                MIPS32 (MIPS IV)

      Purpose:

      To test a COP2 condition code and make a PC-relative conditional branch; execute the instruction in the delay slot
      only if the branch is taken.

      Description: if cc = 0 then branch_likely

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself) in the branch delay slot to form a PC-relative effective target address. If the COP2
      condition specified by CC is false (0), the program branches to the effective target address after the instruction in the
      delay slot is executed. If the branch is not taken, the instruction in the delay slot is not executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

      This operation specification is for the general Branch On Condition operation with the tf (true/false) and nd (nullify
      delay slot) fields as variables. The individual instructions BC2F, BC2FL, BC2T, and BC2TL have specific values for
      tf and nd.
           I:       condition <- COP2Condition(cc) = 0
                    target_offset <- (offset15)            GPRLEN-(16+2)   || offset || 0       2

           I+1:     if condition then
                         PC <- PC + target_offset
                    else
                         NullifyCurrentInstruction()
                    endif
    Exceptions:

    Coprocessor Unusable, Reserved Instruction

    Programming Notes:

    With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
    (JR) instructions to branch to addresses outside this range.

    Software is strongly encouraged to avoid the use of the Branch Likely instructions, as they will be removed from a
    future revision of the MIPS Architecture.

    Some implementations always predict the branch will be taken, so there is a significant penalty if the branch is not
    taken. Software should only use this instruction when there is a very high probability (98% or more) that the branch
    will be taken. If the branch is not likely to be taken or if the probability of a taken branch is unknown, software is
    encouraged to use the BC2F instruction instead.');
INSERT INTO "instructions" VALUES('MIPS32','BC2T','


Branch on COP2 True                                                                                                    BC2T


   31                26 25             21 20       18 17 16 15                                                           0

          COP2                  BC                     nd tf
                                                cc                                        offset
         010010               01000                    0   1

            6                    5              3      1   1                               16


      Format: BC2T offset (cc = 0 implied)                                                             MIPS32 (MIPS I)
                  BC2T cc, offset                                                                   MIPS32 (MIPS IV)

      Purpose:

      To test a COP2 condition code and do a PC-relative conditional branch

      Description: if cc = 1 then branch

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself) in the branch delay slot to form a PC-relative effective target address. If the COP2
      condition specified by CC is true (1), the program branches to the effective target address after the instruction in the
      delay slot is executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

      This operation specification is for the general Branch On Condition operation with the tf (true/false) and nd (nullify
      delay slot) fields as variables. The individual instructions BC2F, BC2FL, BC2T, and BC2TL have specific values for
      tf and nd.
           I:       condition <- COP2Condition(cc) = 1
                    target_offset <- (offset15)          GPRLEN-(16+2)    || offset || 0     2

           I+1:     if condition then
                         PC <- PC + target_offset
                    endif

      Exceptions:

      Coprocessor Unusable, Reserved Instruction

      Programming Notes:

      With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
      (JR) instructions to branch to addresses outside this range.');
INSERT INTO "instructions" VALUES('MIPS32','BC2TL','


Branch on COP2 True Likely                                                                                           BC2TL


    31               26 25              21 20       18 17 16 15                                                          0

          COP2                  BC                     nd tf
                                                cc                                          offset
          010010               01000                    1   1

            6                     5             3       1   1                                 16


      Format: BC2TL          offset (cc = 0 implied)                                                   MIPS32 (MIPS II)
                  BC2TL      cc, offset                                                                MIPS32 (MIPS IV)

      Purpose:

      To test a COP2 condition code and do a PC-relative conditional branch; execute the instruction in the delay slot only
      if the branch is taken.

      Description: if cc = 1 then branch_likely

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself) in the branch delay slot to form a PC-relative effective target address. If the COP2
      condition specified by CC is true (1), the program branches to the effective target address after the instruction in the
      delay slot is executed. If the branch is not taken, the instruction in the delay slot is not executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

      This operation specification is for the general Branch On Condition operation with the tf (true/false) and nd (nullify
      delay slot) fields as variables. The individual instructions BC2F, BC2FL, BC2T, and BC2TL have specific values for
      tf and nd.
           I:       condition <- COP2Condition(cc) = 1
                    target_offset <- (offset15)            GPRLEN-(16+2)   || offset || 0       2

           I+1:     if condition then
                         PC <- PC + target_offset
                    else
                         NullifyCurrentInstruction()
                    endif
    Exceptions:

    Coprocessor Unusable, Reserved Instruction

    Programming Notes:

    With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
    (JR) instructions to branch to addresses outside this range.

    Software is strongly encouraged to avoid the use of the Branch Likely instructions, as they will be removed from a
    future revision of the MIPS Architecture.

    Some implementations always predict the branch will be taken, so there is a significant penalty if the branch is not
    taken. Software should only use this instruction when there is a very high probability (98% or more) that the branch
    will be taken. If the branch is not likely to be taken or if the probability of a taken branch is unknown, software is
    encouraged to use the BC2T instruction instead.');
INSERT INTO "instructions" VALUES('MIPS32','BEQ','


Branch on Equal                                                                                                         BEQ


   31               26 25               21 20              16 15                                                         0

          BEQ
                                 rs                 rt                                    offset
         000100

            6                    5                  5                                      16


      Format: BEQ rs, rt, offset                                                                       MIPS32 (MIPS I)

      Purpose:

      To compare GPRs then do a PC-relative conditional branch

      Description: if rs = rt then branch

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      If the contents of GPR rs and GPR rt are equal, branch to the effective target address after the instruction in the delay
      slot is executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

           I:       target_offset <- sign_extend(offset || 0 )               2

                         condition <- (GPR[rs] = GPR[rt])
           I+1:     if condition then
                         PC <- PC + target_offset
                    endif

      Exceptions:

      None

      Programming Notes:

      With the 18-bit signed instruction offset, the conditional branch range is +- 128 Kbytes. Use jump (J) or jump register
      (JR) instructions to branch to addresses outside this range.

      BEQ r0, r0 offset, expressed as B offset, is the assembly idiom used to denote an unconditional branch.');
INSERT INTO "instructions" VALUES('MIPS32','BEQL','


Branch on Equal Likely                                                                                                 BEQL


   31               26 25               21 20              16 15                                                         0

          BEQL
                                 rs                 rt                                     offset
         010100

            6                    5                  5                                       16


      Format: BEQL rs, rt, offset                                                                      MIPS32 (MIPS II)

      Purpose:

      To compare GPRs then do a PC-relative conditional branch; execute the delay slot only if the branch is taken.

      Description: if rs = rt then branch_likely

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      If the contents of GPR rs and GPR rt are equal, branch to the target address after the instruction in the delay slot is
      executed. If the branch is not taken, the instruction in the delay slot is not executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

           I:       target_offset <- sign_extend(offset || 0 )               2

                         condition <- (GPR[rs] = GPR[rt])
           I+1:     if condition then
                         PC <- PC + target_offset
                    else
                         NullifyCurrentInstruction()
                    endif

      Exceptions:

      None
    Programming Notes:

    With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
    (JR) instructions to branch to addresses outside this range.

    Software is strongly encouraged to avoid the use of the Branch Likely instructions, as they will be removed from a
    future revision of the MIPS Architecture.

    Some implementations always predict the branch will be taken, so there is a significant penalty if the branch is not
    taken. Software should only use this instruction when there is a very high probability (98% or more) that the branch
    will be taken. If the branch is not likely to be taken or if the probability of a taken branch is unknown, software is
    encouraged to use the BEQ instruction instead.

    Historical Information:

    In the MIPS I architecture, this instruction signaled a Reserved Instruction Exception.');
INSERT INTO "instructions" VALUES('MIPS32','BGEZ','


Branch on Greater Than or Equal to Zero                                                                                  BGEZ


    31               26 25               21 20             16 15                                                           0

        REGIMM                                    BGEZ
                                 rs                                                        offset
         000001                                   00001

            6                    5                  5                                        16


      Format: BGEZ rs, offset                                                                            MIPS32 (MIPS I)

      Purpose:

      To test a GPR then do a PC-relative conditional branch

      Description: if rs # 0 then branch

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      If the contents of GPR rs are greater than or equal to zero (sign bit is 0), branch to the effective target address after the
      instruction in the delay slot is executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

           I:       target_offset <- sign_extend(offset || 0 )                2

                    condition <- GPR[rs] # 0          GPRLEN

           I+1:     if condition then
                         PC <- PC + target_offset
                    endif

      Exceptions:

      None

      Programming Notes:

      With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
      (JR) instructions to branch to addresses outside this range.');
INSERT INTO "instructions" VALUES('MIPS32','BGEZAL','


Branch on Greater Than or Equal to Zero and Link                                                                      BGEZAL


     31               26 25               21 20             16 15                                                           0

        REGIMM                                    BGEZAL
                                  rs                                                        offset
          000001                                   10001

             6                    5                  5                                        16


       Format: BGEZAL rs, offset                                                                          MIPS32 (MIPS I)

       Purpose:

       To test a GPR then do a PC-relative conditional procedure call

       Description: if rs # 0 then procedure_call

       Place the return address link in GPR 31. The return link is the address of the second instruction following the branch,
       where execution continues after a procedure call.

       An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
       the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

       If the contents of GPR rs are greater than or equal to zero (sign bit is 0), branch to the effective target address after the
       instruction in the delay slot is executed.

       Restrictions:

       Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
       delay slot of a branch or jump.

       GPR 31 must not be used for the source register rs, because such an instruction does not have the same effect when
       reexecuted. The result of executing such an instruction is UNPREDICTABLE. This restriction permits an exception
       handler to resume execution by reexecuting the branch when an exception occurs in the branch delay slot.

       Operation:

            I:       target_offset <- sign_extend(offset || 0 )                2

                     condition <- GPR[rs] # 0          GPRLEN

                     GPR[31] <- PC + 8
            I+1:     if condition then
                          PC <- PC + target_offset
                     endif

       Exceptions:

       None

       Programming Notes:

       With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump and link (JAL) or
       jump and link register (JALR) instructions for procedure calls to addresses outside this range.

       BGEZAL r0, offset, expressed as BAL offset, is the assembly idiom used to denote a PC-relative branch and link.
       BAL is used in a manner similar to JAL, but provides PC-relative addressing and a more limited target PC range.');
INSERT INTO "instructions" VALUES('MIPS32','BGEZALL','


Branch on Greater Than or Equal to Zero and Link Likely                                                              BGEZALL


      31               26 25               21 20              16 15                                                          0

         REGIMM                                   BGEZALL
                                   rs                                                        offset
           000001                                    10011

              6                    5                   5                                        16


        Format: BGEZALL rs, offset                                                                        MIPS32 (MIPS II)

        Purpose:

        To test a GPR then do a PC-relative conditional procedure call; execute the delay slot only if the branch is taken.

        Description: if rs # 0 then procedure_call_likely

        Place the return address link in GPR 31. The return link is the address of the second instruction following the branch,
        where execution continues after a procedure call.

        An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
        the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

        If the contents of GPR rs are greater than or equal to zero (sign bit is 0), branch to the effective target address after the
        instruction in the delay slot is executed. If the branch is not taken, the instruction in the delay slot is not executed.

        Restrictions:

        GPR 31 must not be used for the source register rs, because such an instruction does not have the same effect when
        reexecuted. The result of executing such an instruction is UNPREDICTABLE. This restriction permits an exception
        handler to resume execution by reexecuting the branch when an exception occurs in the branch delay slot.

        Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
        delay slot of a branch or jump.

        Operation:

             I:       target_offset <- sign_extend(offset || 0 )                2

                      condition <- GPR[rs] # 0           GPRLEN

                      GPR[31] <- PC + 8
             I+1:     if condition then
                           PC <- PC + target_offset
                      else
                           NullifyCurrentInstruction()
                      endif

        Exceptions:

        None');
INSERT INTO "instructions" VALUES('MIPS32','Branch','

Programming Notes:

With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump and link (JAL) or
jump and link register (JALR) instructions for procedure calls to addresses outside this range.

Software is strongly encouraged to avoid the use of the Branch Likely instructions, as they will be removed from a
future revision of the MIPS Architecture.

Some implementations always predict the branch will be taken, so there is a significant penalty if the branch is not
taken. Software should only use this instruction when there is a very high probability (98% or more) that the branch
will be taken. If the branch is not likely to be taken or if the probability of a taken branch is unknown, software is
encouraged to use the BGEZAL instruction instead.

Historical Information:

In the MIPS I architecture, this instruction signaled a Reserved Instruction Exception.');
INSERT INTO "instructions" VALUES('MIPS32','BGEZL','


Branch on Greater Than or Equal to Zero Likely                                                                          BGEZL


    31               26 25               21 20              16 15                                                          0

       REGIMM                                     BGEZL
                                 rs                                                        offset
         000001                                    00011

            6                    5                   5                                        16


      Format: BGEZL rs, offset                                                                          MIPS32 (MIPS II)

      Purpose:

      To test a GPR then do a PC-relative conditional branch; execute the delay slot only if the branch is taken.

      Description: if rs # 0 then branch_likely

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      If the contents of GPR rs are greater than or equal to zero (sign bit is 0), branch to the effective target address after the
      instruction in the delay slot is executed. If the branch is not taken, the instruction in the delay slot is not executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

           I:       target_offset <- sign_extend(offset || 0 )                2

                    condition <- GPR[rs] # 0           GPRLEN

           I+1:     if condition then
                         PC <- PC + target_offset
                    else
                         NullifyCurrentInstruction()
                    endif

      Exceptions:

      None
    Programming Notes:

    With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
    (JR) instructions to branch to addresses outside this range.

    Software is strongly encouraged to avoid the use of the Branch Likely instructions, as they will be removed from a
    future revision of the MIPS Architecture.

    Some implementations always predict the branch will be taken, so there is a significant penalty if the branch is not
    taken. Software should only use this instruction when there is a very high probability (98% or more) that the branch
    will be taken. If the branch is not likely to be taken or if the probability of a taken branch is unknown, software is
    encouraged to use the BGEZ instruction instead.

    Historical Information:

    In the MIPS I architecture, this instruction signaled a Reserved Instruction Exception.');
INSERT INTO "instructions" VALUES('MIPS32','BGTZ','


Branch on Greater Than Zero                                                                                            BGTZ


   31                26 25               21 20             16 15                                                         0

          BGTZ                                       0
                                 rs                                                       offset
         000111                                    00000

            6                    5                   5                                     16


      Format: BGTZ rs, offset                                                                          MIPS32 (MIPS I)

      Purpose:

      To test a GPR then do a PC-relative conditional branch

      Description: if rs > 0 then branch

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      If the contents of GPR rs are greater than zero (sign bit is 0 but value not zero), branch to the effective target address
      after the instruction in the delay slot is executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

           I:        target_offset <- sign_extend(offset || 0 )              2

                     condition <- GPR[rs] > 0          GPRLEN

           I+1:      if condition then
                         PC <- PC + target_offset
                     endif

      Exceptions:

      None

      Programming Notes:

      With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
      (JR) instructions to branch to addresses outside this range.');
INSERT INTO "instructions" VALUES('MIPS32','BGTZL','


Branch on Greater Than Zero Likely                                                                                       BGTZL


    31               26 25               21 20               16 15                                                           0

         BGTZL                                       0
                                 rs                                                         offset
         010111                                    00000

            6                     5                  5                                        16


      Format: BGTZL rs, offset                                                                          MIPS32 (MIPS II)

      Purpose:

      To test a GPR then do a PC-relative conditional branch; execute the delay slot only if the branch is taken.

      Description: if rs > 0 then branch_likely

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      If the contents of GPR rs are greater than zero (sign bit is 0 but value not zero), branch to the effective target address
      after the instruction in the delay slot is executed. If the branch is not taken, the instruction in the delay slot is not exe-
      cuted.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

           I:        target_offset <- sign_extend(offset || 0 )               2

                     condition <- GPR[rs] > 0          GPRLEN

           I+1:      if condition then
                         PC <- PC + target_offset
                     else
                         NullifyCurrentInstruction()
                     endif

      Exceptions:

      None
    Programming Notes:

    With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
    (JR) instructions to branch to addresses outside this range.

    Software is strongly encouraged to avoid the use of the Branch Likely instructions, as they will be removed from a
    future revision of the MIPS Architecture.

    Some implementations always predict the branch will be taken, so there is a significant penalty if the branch is not
    taken. Software should only use this instruction when there is a very high probability (98% or more) that the branch
    will be taken. If the branch is not likely to be taken or if the probability of a taken branch is unknown, software is
    encouraged to use the BGTZ instruction instead.

    Historical Information:

    In the MIPS I architecture, this instruction signaled a Reserved Instruction Exception.');
INSERT INTO "instructions" VALUES('MIPS32','BLEZ','


Branch on Less Than or Equal to Zero                                                                                   BLEZ


   31               26 25                21 20              16 15                                                        0

          BLEZ                                       0
                                 rs                                                       offset
         000110                                   00000

            6                    5                   5                                     16


      Format: BLEZ rs, offset                                                                          MIPS32 (MIPS I)

      Purpose:

      To test a GPR then do a PC-relative conditional branch

      Description: if rs $ 0 then branch

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      If the contents of GPR rs are less than or equal to zero (sign bit is 1 or value is zero), branch to the effective target
      address after the instruction in the delay slot is executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

           I:       target_offset <- sign_extend(offset || 0 )               2

                    condition <- GPR[rs] $ 0           GPRLEN

           I+1:     if condition then
                         PC <- PC + target_offset
                    endif

      Exceptions:

      None

      Programming Notes:

      With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
      (JR) instructions to branch to addresses outside this range.');
INSERT INTO "instructions" VALUES('MIPS32','BLEZL','


Branch on Less Than or Equal to Zero Likely                                                                            BLEZL


    31              26 25               21 20               16 15                                                         0

         BLEZL                                      0
                                 rs                                                        offset
         010110                                   00000

            6                    5                  5                                       16


      Format: BLEZL rs, offset                                                                          MIPS32 (MIPS II)

      Purpose:

      To test a GPR then do a PC-relative conditional branch; execute the delay slot only if the branch is taken.

      Description: if rs $ 0 then branch_likely

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      If the contents of GPR rs are less than or equal to zero (sign bit is 1 or value is zero), branch to the effective target
      address after the instruction in the delay slot is executed. If the branch is not taken, the instruction in the delay slot is
      not executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

           I:       target_offset <- sign_extend(offset || 0 )                2

                    condition <- GPR[rs] $ 0           GPRLEN

           I+1:     if condition then
                         PC <- PC + target_offset
                    else
                         NullifyCurrentInstruction()
                    endif

      Exceptions:

      None
    Programming Notes:

    With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
    (JR) instructions to branch to addresses outside this range.

    Software is strongly encouraged to avoid the use of the Branch Likely instructions, as they will be removed from a
    future revision of the MIPS Architecture.

    Some implementations always predict the branch will be taken, so there is a significant penalty if the branch is not
    taken. Software should only use this instruction when there is a very high probability (98% or more) that the branch
    will be taken. If the branch is not likely to be taken or if the probability of a taken branch is unknown, software is
    encouraged to use the BLEZ instruction instead.

    Historical Information:

    In the MIPS I architecture, this instruction signaled a Reserved Instruction Exception.');
INSERT INTO "instructions" VALUES('MIPS32','BLTZ','


Branch on Less Than Zero                                                                                                 BLTZ


   31                26 25              21 20              16 15                                                           0

       REGIMM                                     BLTZ
                                 rs                                                         offset
         000001                                   00000

            6                    5                  5                                        16


      Format: BLTZ rs, offset                                                                           MIPS32 (MIPS I)

      Purpose:

      To test a GPR then do a PC-relative conditional branch

      Description: if rs < 0 then branch

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      If the contents of GPR rs are less than zero (sign bit is 1), branch to the effective target address after the instruction in
      the delay slot is executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:
           I:       target_offset <- sign_extend(offset || 0 )               2

                    condition <- GPR[rs] < 0          GPRLEN

           I+1:     if condition then
                         PC <- PC + target_offset
                    endif

      Exceptions:

      None

      Programming Notes:

      With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump and link (JAL) or
      jump and link register (JALR) instructions for procedure calls to addresses outside this range.');
INSERT INTO "instructions" VALUES('MIPS32','BLTZAL','


Branch on Less Than Zero and Link                                                                                      BLTZAL


    31               26 25              21 20              16 15                                                           0

       REGIMM                                    BLTZAL
                                 rs                                                         offset
         000001                                   10000

            6                    5                  5                                        16


      Format: BLTZAL rs, offset                                                                         MIPS32 (MIPS I)

      Purpose:

      To test a GPR then do a PC-relative conditional procedure call

      Description: if rs < 0 then procedure_call

      Place the return address link in GPR 31. The return link is the address of the second instruction following the branch,
      where execution continues after a procedure call.

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      If the contents of GPR rs are less than zero (sign bit is 1), branch to the effective target address after the instruction in
      the delay slot is executed.

      Restrictions:

      GPR 31 must not be used for the source register rs, because such an instruction does not have the same effect when
      reexecuted. The result of executing such an instruction is UNPREDICTABLE. This restriction permits an exception
      handler to resume execution by reexecuting the branch when an exception occurs in the branch delay slot.

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

           I:       target_offset <- sign_extend(offset || 0 )               2

                    condition <- GPR[rs] < 0          GPRLEN

                    GPR[31] <- PC + 8
           I+1:     if condition then
                         PC <- PC + target_offset
                    endif

      Exceptions:

      None

      Programming Notes:

      With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump and link (JAL) or
      jump and link register (JALR) instructions for procedure calls to addresses outside this range.');
INSERT INTO "instructions" VALUES('MIPS32','BLTZALL','


Branch on Less Than Zero and Link Likely                                                                              BLTZALL


     31               26 25               21 20             16 15                                                           0

        REGIMM                                   BLTZALL
                                  rs                                                         offset
          000001                                    10010

             6                    5                  5                                        16


       Format: BLTZALL rs, offset                                                                        MIPS32 (MIPS II)

       Purpose:

       To test a GPR then do a PC-relative conditional procedure call; execute the delay slot only if the branch is taken.

       Description: if rs < 0 then procedure_call_likely

       Place the return address link in GPR 31. The return link is the address of the second instruction following the branch,
       where execution continues after a procedure call.

       An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
       the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

       If the contents of GPR rs are less than zero (sign bit is 1), branch to the effective target address after the instruction in
       the delay slot is executed. If the branch is not taken, the instruction in the delay slot is not executed.

       Restrictions:

       GPR 31 must not be used for the source register rs, because such an instruction does not have the same effect when
       reexecuted. The result of executing such an instruction is UNPREDICTABLE. This restriction permits an exception
       handler to resume execution by reexecuting the branch when an exception occurs in the branch delay slot.

       Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
       delay slot of a branch or jump.

       Operation:

            I:       target_offset <- sign_extend(offset || 0 )                2

                     condition <- GPR[rs] < 0           GPRLEN

                     GPR[31] <- PC + 8
            I+1:     if condition then
                          PC <- PC + target_offset
                     else
                          NullifyCurrentInstruction()
                     endif

       Exceptions:

       None
   Programming Notes:

   With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump and link (JAL) or
   jump and link register (JALR) instructions for procedure calls to addresses outside this range.

   Software is strongly encouraged to avoid the use of the Branch Likely instructions, as they will be removed from a
   future revision of the MIPS Architecture.

   Some implementations always predict the branch will be taken, so there is a significant penalty if the branch is not
   taken. Software should only use this instruction when there is a very high probability (98% or more) that the branch
   will be taken. If the branch is not likely to be taken or if the probability of a taken branch is unknown, software is
   encouraged to use the BLTZAL instruction instead.

   Historical Information:

   In the MIPS I architecture, this instruction signaled a Reserved Instruction Exception.');
INSERT INTO "instructions" VALUES('MIPS32','BLTZL','


Branch on Less Than Zero Likely                                                                                          BLTZL


   31                26 25               21 20             16 15                                                           0

       REGIMM                                     BLTZL
                                 rs                                                         offset
         000001                                    00010

            6                    5                  5                                        16


      Format: BLTZL rs, offset                                                                          MIPS32 (MIPS II)

      Purpose:

      To test a GPR then do a PC-relative conditional branch; execute the delay slot only if the branch is taken.

      Description: if rs < 0 then branch_likely

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      If the contents of GPR rs are less than zero (sign bit is 1), branch to the effective target address after the instruction in
      the delay slot is executed. If the branch is not taken, the instruction in the delay slot is not executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

           I:       target_offset <- sign_extend(offset || 0 )                2

                    condition <- GPR[rs] < 0           GPRLEN

           I+1:     if condition then
                         PC <- PC + target_offset
                    else
                         NullifyCurrentInstruction()
                    endif

      Exceptions:

      None
   Programming Notes:

   With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
   (JR) instructions to branch to addresses outside this range.

   Software is strongly encouraged to avoid the use of the Branch Likely instructions, as they will be removed from a
   future revision of the MIPS Architecture.

   Some implementations always predict the branch will be taken, so there is a significant penalty if the branch is not
   taken. Software should only use this instruction when there is a very high probability (98% or more) that the branch
   will be taken. If the branch is not likely to be taken or if the probability of a taken branch is unknown, software is
   encouraged to use the BLTZ instruction instead.

   Historical Information:

   In the MIPS I architecture, this instruction signaled a Reserved Instruction Exception.');
INSERT INTO "instructions" VALUES('MIPS32','BNE','


Branch on Not Equal                                                                                                     BNE


   31                26 25              21 20              16 15                                                         0

          BNE
                                 rs                 rt                                    offset
         000101

            6                    5                  5                                      16


      Format: BNE rs, rt, offset                                                                       MIPS32 (MIPS I)

      Purpose:

      To compare GPRs then do a PC-relative conditional branch

      Description: if rs != rt then branch

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      If the contents of GPR rs and GPR rt are not equal, branch to the effective target address after the instruction in the
      delay slot is executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

           I:       target_offset <- sign_extend(offset || 0 )               2

                    condition <- (GPR[rs] != GPR[rt])
           I+1:     if condition then
                         PC <- PC + target_offset
                    endif

      Exceptions:

      None

      Programming Notes:

      With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
      (JR) instructions to branch to addresses outside this range.');
INSERT INTO "instructions" VALUES('MIPS32','BNEL','


Branch on Not Equal Likely                                                                                             BNEL


   31                26 25              21 20               16 15                                                        0

          BNEL
                                 rs                  rt                                     offset
         010101

            6                     5                  5                                        16


      Format: BNEL rs, rt, offset                                                                      MIPS32 (MIPS II)

      Purpose:

      To compare GPRs then do a PC-relative conditional branch; execute the delay slot only if the branch is taken.

      Description: if rs != rt then branch_likely

      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.

      If the contents of GPR rs and GPR rt are not equal, branch to the effective target address after the instruction in the
      delay slot is executed. If the branch is not taken, the instruction in the delay slot is not executed.

      Restrictions:

      Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
      delay slot of a branch or jump.

      Operation:

           I:       target_offset <- sign_extend(offset || 0 )                2

                    condition <- (GPR[rs] != GPR[rt])
           I+1:     if condition then
                         PC <- PC + target_offset
                    else
                         NullifyCurrentInstruction()
                    endif

      Exceptions:

      None
    Programming Notes:

    With the 18-bit signed instruction offset, the conditional branch range is +- 128 KBytes. Use jump (J) or jump register
    (JR) instructions to branch to addresses outside this range.

    Software is strongly encouraged to avoid the use of the Branch Likely instructions, as they will be removed from a
    future revision of the MIPS Architecture.

    Some implementations always predict the branch will be taken, so there is a significant penalty if the branch is not
    taken. Software should only use this instruction when there is a very high probability (98% or more) that the branch
    will be taken. If the branch is not likely to be taken or if the probability of a taken branch is unknown, software is
    encouraged to use the BNE instruction instead.

    Historical Information:

    In the MIPS I architecture, this instruction signaled a Reserved Instruction Exception.');
INSERT INTO "instructions" VALUES('MIPS32','BREAK','


Breakpoint                                                                                                      BREAK


    31              26 25                                                                 6   5                    0

       SPECIAL                                                                                       BREAK
                                                      code
        000000                                                                                        001101

           6                                           20                                               6


      Format: BREAK                                                                               MIPS32 (MIPS I)

      Purpose:

      To cause a Breakpoint exception

      Description:

      A breakpoint exception occurs, immediately and unconditionally transferring control to the exception handler. The
      code field is available for use as software parameters, but is retrieved by the exception handler only by loading the
      contents of the memory word containing the instruction.

      Restrictions:

      None

      Operation:

          SignalException(Breakpoint)

      Exceptions:

      Breakpoint');
INSERT INTO "instructions" VALUES('MIPS32','C.cond.fmt','


  Floating Point Compare                                                                                                 C.cond.fmt


          31               26 25               21 20               16 15             11 10         8   7   6   5  4   3            0

                COP1                                                                                       A   FC
                                       fmt                 ft                 fs             cc        0                   cond
               010001                                                                                      0    11

                  6                     5                   5                  5              3        1   1    2           4


            Format: C.cond.S fs, ft (cc = 0 implied)                                                            MIPS32 (MIPS I)
                         C.cond.D fs, ft (cc = 0 implied)                                                       MIPS32 (MIPS I)
                         C.cond.S cc, fs, ft                                                                  MIPS32 (MIPS IV)
                         C.cond.D cc, fs, ft                                                                  MIPS32 (MIPS IV)

            Purpose:

            To compare FP values and record the Boolean result in a condition code

            Description: cc <- fs compare_cond ft

            The value in FPR fs is compared to the value in FPR ft; the values are in format fmt. The comparison is exact and nei-
            ther overflows nor underflows.

            If the comparison specified by cond    2..1is true for the operand values, the result is true; otherwise, the result is false. If
            no exception is taken, the result is written into condition code CC; true is 1 and false is 0.

            If one of the values is an SNaN, or cond is set and at least one of the values is a QNaN, an Invalid Operation condi-
                                                       3
            tion is raised and the Invalid Operation flag is set in the FCSR. If the Invalid Operation Enable bit is set in the FCSR,
            no result is written and an Invalid Operation exception is taken immediately. Otherwise, the Boolean result is written
            into condition code CC.

            There are four mutually exclusive ordering relations for comparing floating point values; one relation is always true
            and the others are false. The familiar relations are greater than, less than, and equal. In addition, the IEEE floating
            point standard defines the relation unordered, which is true when at least one operand value is NaN; NaN compares
            unordered with everything, including itself. Comparisons ignore the sign of zero, so +0 equals -0.

            The comparison condition is a logical predicate, or equation, of the ordering relations such as less than or equal,
            equal, not less than, or unordered or equal. Compare distinguishes among the 16 comparison predicates. The Bool-
            ean result of the instruction is obtained by substituting the Boolean value of each ordering relation for the two FP val-
            ues in the equation. If the equal relation is true, for example, then all four example predicates above yield a true
            result. If the unordered relation is true then only the final predicate, unordered or equal, yields a true result.

            Logical negation of a compare result allows eight distinct comparisons to test for the 16 predicates as shown in . Each
            mnemonic tests for both a predicate and its logical negation. For each mnemonic, compare tests the truth of the first
            predicate. When the first predicate is true, the result is true as shown in the !=If Predicate Is True!= column, and the sec-
            ond predicate must be false, and vice versa. (Note that the False predicate is never true and False/True do not follow
            the normal pattern.)

            The truth of the second predicate is the logical negation of the instruction result. After a compare instruction, test for
            the truth of the first predicate can be made with the Branch on FP True (BC1T) instruction and the truth of the second
            can be made with Branch on FP False (BC1F).
                                                                                                3
      conditions. For these additional comparisons, if at least one of the operands is a NaN, including Quiet NaN, then an
      Invalid Operation condition is raised. If the Invalid Operation condition is enabled in the FCSR, an Invalid Operation
      exception occurs.



                        Table 3-24 FPU Comparisons Without Special Operand Exceptions

  Instruction                        Comparison Predicate                                  Comparison CC        Instruction
                                                                                                 Result

    Cond                     Name of Predicate and                      Relation              If         Inv Op Condition
  Mnemonic      Logically Negated Predicate (Abbreviation)               Values          Predicate        Excp.   Field
                                                                                          Is True          if
                                                                       > < = ?                           QNaN    3     2..0

                                                                                                           ?

                False [this predicate is always False]                 F   F    F  F
 F                                                                                            F                          0
                True (T)                                               T   T    T  T

                Unordered                                              F   F    F  T          T
 UN                                                                                                                      1
                Ordered (OR)                                           T   T    T  F          F

                Equal                                                  F   F    T  F          T
 EQ                                                                                                                      2
                Not Equal (NEQ)                                        T   T    F  T          F

                Unordered or Equal                                     F   F    T  T          T
 UEQ                                                                                                                     3
                Ordered or Greater Than or Less Than (OGL)             T   T    F  F          F
                                                                                                           No    0
                Ordered or Less Than                                   F   T    F  F          T
 OLT                                                                                                                     4
                Unordered or Greater Than or Equal (UGE)               T   F    T  T          F

                Unordered or Less Than                                 F   T    F  T          T
 ULT                                                                                                                     5
                Ordered or Greater Than or Equal (OGE)                 T   F    T  F          F

                Ordered or Less Than or Equal                          F   T    T  F          T
 OLE                                                                                                                     6
                Unordered or Greater Than (UGT)                        T   F    F  T          F

                Unordered or Less Than or Equal                        F   T    T  T          T
 ULE                                                                                                                     7
                Ordered or Greater Than (OGT)                          T   F    F  F          F

                       Key: ? = unordered, > = greater than, < = less than, = is equal, T = True, F = False

                  Table 3-25 FPU Comparisons With Special Operand Exceptions for QNaNs

Instruction                        Comparison Predicate                                    Comparison CC        Instructio
                                                                                                  Result            n

  Cond                    Name of Predicate and                        Relation              If         Inv Op  Condition
 Mnemonic     Logically Negated Predicate (Abbreviation)                 Values         Predicate       Excp If    Field
                                                                                          Is True       QNaN?
                                                                      > < = ?                                     3    2..0

             Signaling False [this predicate always False]            F   F    F   F
SF                                                                                           F                          0
             Signaling True (ST)                                      T   T    T   T

             Not Greater Than or Less Than or Equal                   F   F    F   T         T
NGLE                                                                                                                    1
             Greater Than or Less Than or Equal (GLE)                 T   T    T   F         F

             Signaling Equal                                          F   F    T   F         T
SEQ                                                                                                                     2
             Signaling Not Equal (SNE)                                T   T    F   T         F

             Not Greater Than or Less Than                            F   F    T   T         T
NGL                                                                                                                     3
             Greater Than or Less Than (GL)                           T   T    F   F         F
                                                                                                          Yes     1
             Less Than                                                F   T    F   F         T
LT                                                                                                                      4
             Not Less Than (NLT)                                      T   F    T   T         F

             Not Greater Than or Equal                                F   T    F   T         T
NGE                                                                                                                     5
             Greater Than or Equal (GE)                               T   F    T   F         F

             Less Than or Equal                                       F   T    T   F         T
LE                                                                                                                      6
             Not Less Than or Equal (NLE)                             T   F    F   T         F

             Not Greater Than                                         F   T    T   T         T
NGT                                                                                                                     7
             Greater Than (GT)                                        T   F    F   F         F

                     Key: ? = unordered, > = greater than, < = less than, = is equal, T = True, F = False
      Restrictions:

      The fields fs and ft must specify FPRs valid for operands of type fmt; if they are not valid, the result is UNPREDICT-
      ABLE.

      The operands must be values in format fmt; if they are not, the result is UNPREDICTABLE and the value of the
      operand FPRs becomes UNPREDICTABLE.

      Operation:
          if SNaN(ValueFPR(fs, fmt)) or SNaN(ValueFPR(ft, fmt)) or
              QNaN(ValueFPR(fs, fmt)) or QNaN(ValueFPR(ft, fmt)) then
              less <- false
              equal <- false
              unordered <- true
              if (SNaN(ValueFPR(fs,fmt)) or SNaN(ValueFPR(ft,fmt))) or
              (cond and (QNaN(ValueFPR(fs,fmt)) or QNaN(ValueFPR(ft,fmt)))) then
                     3
                   SignalException(InvalidOperation)
              endif
          else
              less <- ValueFPR(fs, fmt) <fmt ValueFPR(ft, fmt)
              equal <- ValueFPR(fs, fmt) =fmt ValueFPR(ft, fmt)
              unordered <- false
          endif
          condition <- (cond2 and less) or (cond1 and equal)
                   or (cond and unordered)
                              0
          SetFPConditionCode(cc, condition)
      Exceptions:

      Coprocessor Unusable, Reserved Instruction

      Floating Point Exceptions:

      Unimplemented Operation, Invalid Operation

      Programming Notes:

      FP computational instructions, including compare, that receive an operand value of Signaling NaN raise the Invalid
      Operation condition. Comparisons that raise the Invalid Operation condition for Quiet NaNs in addition to SNaNs
      permit a simpler programming model if NaNs are errors. Using these compares, programs do not need explicit code
      to check for QNaNs causing the unordered relation. Instead, they take an exception and allow the exception handling
      system to deal with the error when it occurs. For example, consider a comparison in which we want to know if two
      numbers are equal, but for which unordered would be an error.

          # comparisons using explicit tests for QNaN
               c.eq.d $f2,$f4# check for equal
               nop
               bc1t      L2       # it is equal
               c.un.d $f2,$f4# it is not equal,
                                  # but might be unordered
               bc1t      ERROR # unordered goes off to an error handler
          # not-equal-case code here
               ...
          # equal-case code here
          L2:
          # --------------------------------------------------------------
          # comparison using comparisons that signal QNaN
               c.seq.d $f2,$f4 # check for equal
               nop
               bc1t      L2            # it is equal
               nop
          # it is not unordered here
               ...
          # not-equal-case code here
               ...
          # equal-case code here

      Historical Information:

      The MIPS I architecture defines a single floating point condition code, implemented as the coprocessor 1 condition
      signal (Cp1Cond) and the C bit in the FP Control/Status register. MIPS I, II, and III architectures must have the CC
      field set to 0, which is implied by the first format in the !=Format!= section.

      The MIPS IV and MIPS32 architectures add seven more Condition Code bits to the original condition code 0. FP
      compare and conditional branch instructions specify the Condition Code bit to set or test. Both assembler formats are
      malid for MIPS IV and MIPS32.

      In the MIPS I, II, and III architectures there must be at least one instruction between the compare instruction that sets
      the condition code and the branch instruction that tests it. Hardware does not detect a violation of this restriction.');
INSERT INTO "instructions" VALUES('MIPS32','CACHE','



     Perform Cache Operation                                                                                          CACHE


    31               26 25               21 20                16 15                                                           0

         CACHE
                                base                 op                                         offset
          101111

             6                    5                   5                                           16


       Format: CACHE op, offset(base)                                                                                     MIPS32

       Purpose:

       To perform the cache operation specified by op.

       Description:

       The 16-bit offset is sign-extended and added to the contents of the base register to form an effective address. The
       effective address is used in one of the following ways based on the operation to be performed and the type of cache as
       described in the following table.

                                          Table 3-26 Usage of Effective Address

          Operation           Type of                                Usage of Effective Address
         Requires an          Cache

                                           The effective address is used to address the cache. It is implementation dependent
           Address            Virtual      whether an address translation is performed on the effective address (with the
                                           possibility that a TLB Refill or TLB Invalid exception might occur)

                                           The effective address is translated by the MMU to a physical address. The physical
           Address            Physical
                                           address is then used to address the cache

                                           The effective address is translated by the MMU to a physical address. It is
                                           implementation dependent whether the effective address or the translated physical
                                           address is used to index the cache.



                                           Assuming that the total cache size in bytes is CS, the associativity is A, and the
                                           number of bytes per tag is BPT, the following calculations give the fields of the
                                           address which specify the way and the index:

             Index             N/A                         OffsetBit <- Log2(BPT)
                                                           IndexBit <- Log2(CS / A)
                                                           WayBit <- IndexBit + Ceiling(Log2(A))
                                                           Way <- AddrWayBit-1..IndexBit
                                                           Index <- AddrIndexBit-1..OffsetBit

                                           For a direct-mapped cache, the Way calculation is ignored and the Index value
                                           fully specifies the cache tag. This is shown symbolically in the figure below.
 Perform Cache Operation                                                                               CACHE


                          Figure 3-1 Usage of Address Fields to Select Index and Way


                                                     WayBit   IndexBit                  OffsetBit
                                                                                                0

                              Unused               Way                Index         byte index



   A TLB Refill and TLB Invalid (both with cause code equal TLBL) exception can occur on any operation. For index
   operations (where the address is used to index the cache but need not match the cache tag) software should use
   unmapped addresses to avoid TLB exceptions. This instruction never causes TLB Modified exceptions nor TLB
   Refill exceptions with a cause code of TLBS, nor data Watch exceptions.

   A Cache Error exception may occur as a byproduct of some operations performed by this instruction. For example, if
   a Writeback operation detects a cache or bus error during the processing of the operation, that error is reported via a
   Cache Error exception. Similarly, a Bus Error Exception may occur if a bus operation invoked by this instruction is
   terminated in an error.

   An Address Error Exception (with cause code equal AdEL) may occur if the effective address references a portion of
   the kernel address space which would normally result in such an exception. It is implementation dependent whether
   such an exception does occur.

   It is implementation dependent whether a data watch is triggered by a cache instruction whose address matches the
   Watch register address match conditions.

   Bits [17:16] of the instruction specify the cache on which to perform the operation, as follows:

                           Table 3-27 Encoding of Bits[17:16] of CACHE Instruction

          Code              Name                                           Cache



          2#00                I            Primary Instruction




          2#01                D            Primary Data or Unified Primary




          2#10                T            Tertiary




          2#11                S            Secondary




   Bits [20:18] of the instruction specify the operation to perform. To provide software with a consistent base of cache
   operations, certain encodings must be supported on all processors. The remaining encodings are recommended.


   Perform Cache Operation                                                                                   CACHE


                         Table 3-28 Encoding of Bits [20:18] of the CACHE Instruction

Code     Caches            Name          Effective                       Operation                           Compliance
                                         Address
                                         Operand
                                          Type



                                                      Set the state of the cache block at the specified
                                                      index to invalid.
            I          Index Invalidate    Index                                                               Required
                                                      This required encoding may be used by
                                                      software to invalidate the entire instruction
                                                      cache by stepping through all valid indices.




                      Index Writeback                 For a write-back cache: If the state of the cache
            D         Invalidate / Index   Index      block at the specified index is valid and dirty,          Required
                         Invalidate                   write the block back to the memory address
2#000                                                 specified by the cache tag. After that operation
                                                      is completed, set the state of the cache block to
                                                      invalid. If the block is valid but not dirty, set the
                                                      state of the block to invalid.



                                                      For a write-through cache: Set the state of the
                                                      cache block at the specified index to invalid.
                      Index Writeback
          S, T        Invalidate / Index   Index      This required encoding may be used by                    Optional
                         Invalidate                   software to invalidate the entire data cache by
                                                      stepping through all valid indices. Note that
                                                      Index Store Tag should be used to initialize the
                                                      cache at powerup.




                                                      Read the tag for the cache block at the specified
                                                      index into the TagLo and TagHi Coprocessor 0
                                                      registers. If the DataLo and DataHi registers
                                                      are implemented, also read the data
                                                      corresponding to the byte index into the
                                                      DataLo and DataHi registers.
2#001      All         Index Load Tag      Index                                                             Recommended
                                                      The granularity and alignment of the data read
                                                      into the DataLo and DataHi registers is
                                                      implementation-dependent, but is typically the
                                                      result of an aligned access to the cache,
                                                      ignoring the appropriate low-order bits of the
                                                      byte index.


                 Table 3-28 Encoding of Bits [20:18] of the CACHE Instruction

Code   Caches      Name          Effective                      Operation                           Compliance
                                 Address
                                 Operand
                                   Type



                                              Write the tag for the cache block at the
                                              specified index from the TagLo and TagHi
                                              Coprocessor 0 registers.

                                              This required encoding may be used by
2#010   All    Index Store Tag     Index                                                               Required
                                              software to initialize the entire instruction of
                                              data caches by stepping through all valid
                                              indices. Doing so requires that the TagLo and
                                              TagHi registers associated with the cache be
                                              initialized first.




                                              Available for implementation-dependent
               Implementation                 operation.
2#011   All                      Unspecified                                                            Optional
                 Dependent




                                                                                                       Required
                                                                                                  (Instruction Cache
                                              If the cache block contains the specified
        I, D    Hit Invalidate    Address                                                          Encoding Only),
                                              address, set the state of the cache block to
                                                                                                    Recommended
                                              invalid.
2#100                                                                                                 otherwise
                                              This required encoding may be used by
                                              software to invalidate a range of addresses
                                              from the instruction cache by stepping through
                                              the address range by the line size of the cache.

        S, T    Hit Invalidate    Address                                                              Optional




                                              Fill the cache from the specified address.
          I          Fill         Address                                                           Recommended




               Hit Writeback                  For a write-back cache: If the cache block
         D     Invalidate / Hit   Address     contains the specified address and it is valid            Required
                 Invalidate                   and dirty, write the contents back to memory.
                                              After that operation is completed, set the state
2#101
                                              of the cache block to invalid. If the block is
                                              valid but not dirty, set the state of the block to
                                              invalid.

                                              For a write-through cache: If the cache block
                                              contains the specified address, set the state of
                                              the cache block to invalid.
               Hit Writeback
        S, T   Invalidate / Hit   Address                                                              Optional
                                              This required encoding may be used by
                 Invalidate
                                              software to invalidate a range of addresses
                                              from the data cache by stepping through the
                                              address range by the line size of the cache.



                         Table 3-28 Encoding of Bits [20:18] of the CACHE Instruction

Code    Caches            Name          Effective                        Operation                       Compliance
                                        Address
                                        Operand
                                          Type



            D          Hit Writeback     Address                                                         Recommended
                                                      If the cache block contains the specified
                                                      address and it is valid and dirty, write the
                                                      contents back to memory. After the operation is
2#110
                                                      completed, leave the state of the line valid, but
                                                      clear the dirty state. For a write-through cache,
                                                      this operation may be treated as a nop.
          S, T         Hit Writeback     Address                                                           Optional



                                                      If the cache does not contain the specified
                                                      address, fill it from memory, performing a
                                                      writeback if required, and set the state to valid
                                                      and locked. If the cache already contains the
                                                      specified address, set the state to locked. In
                                                      set-associative or fully-associative caches, the
                                                      way selected on a fill from memory is
                                                      implementation dependent.

                                                      The lock state may be cleared by executing an
                                                      Index Invalidate, Index Writeback Invalidate,
                                                      Hit Invalidate, or Hit Writeback Invalidate
                                                      operation to the locked line, or via an Index
                                                      Store Tag operation to the line that clears the
                                                      lock bit. Note that clearing the lock state via
                                                      Index Store Tag is dependent on the
                                                      implementation-dependent cache tag and
                                                      cache line organization, and that Index and
                                                      Index Writeback Invalidate operations are
2#111     I, D         Fetch and Lock    Address                                                         Recommended
                                                      dependent on cache line organization. Only Hit
                                                      and Hit Writeback Invalidate operations are
                                                      generally portable across implementations.

                                                      It is implementation dependent whether a
                                                      locked line is displaced as the result of an
                                                      external invalidate or intervention that hits on
                                                      the locked line. Software must not depend on
                                                      the locked line remaining in the cache if an
                                                      external invalidate or intervention would
                                                      invalidate the line if it were not locked.

                                                      It is implementation dependent whether a
                                                      Fetch and Lock operation affects more than
                                                      one line. For example, more than one line
                                                      around the referenced address may be fetched
                                                      and locked. It is recommended that only the
                                                      single line containing the referenced address be
                                                      affected.
 Perform Cache Operation (cont.)                                                                   CACHE


   Restrictions:

   The operation of this instruction is UNDEFINED for any operation/cache combination that is not implemented.

   The operation of this instruction is UNDEFINED if the operaation requires an address, and that address is uncache-
   able.

   Operation:

       vAddr <- GPR[base] + sign_extend(offset)
       (pAddr, uncached) <- AddressTranslation(vAddr, DataReadReference)
       CacheOp(op, vAddr, pAddr)

   Exceptions:

   TLB Refill Exception.

   TLB Invalid Exception

   Coprocessor Unusable Exception

   Address Error Exception

   Cache Error Exception

   Bus Error Exception');
INSERT INTO "instructions" VALUES('MIPS32','CEIL.W.fmt','


Floating Point Ceiling Convert to Word Fixed Point                                                                 CEIL.W.fmt


        31              26 25              21 20              16 15               11 10             6   5                   0

             COP1                                      0                                                      CEIL.W
                                   fmt                                    fs               fd
             010001                                 00000                                                      001110

                6                   5                  5                   5                5                    6


          Format: CEIL.W.S           fd, fs                                                             MIPS32 (MIPS II)
                     CEIL.W.D        fd, fs                                                             MIPS32 (MIPS II)

          Purpose:

          To convert an FP value to 32-bit fixed point, rounding up

          Description: fd <- convert_and_round(fs)

          The value in FPR fs, in format fmt, is converted to a value in 32-bit word fixed point format and rounding toward +%
          (rounding mode 2). The result is placed in FPR fd.

          When the source value is Infinity, NaN, or rounds to an integer outside the range -2^31 to 2^31-1, the result cannot be
          represented correctly, an IEEE Invalid Operation condition exists, and the Invalid Operation flag is set in the FCSR. If
          the Invalid Operation Enable bit is set in the FCSR, no result is written to fd and an Invalid Operation exception is
          taken immediately. Otherwise, the default result, 2^31-1, is written to fd.


          Restrictions:

          The fields fs and fd must specify valid FPRs; fs for type fmt and fd for word fixed point; if they are not valid, the result
          is UNPREDICTABLE.

          The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value of the operand
          FPR becomes UNPREDICTABLE.

          Operation:

              StoreFPR(fd, W, ConvertFmt(ValueFPR(fs, fmt), fmt, W))

          Exceptions:

          Coprocessor Unusable, Reserved Instruction

          Floating Point Exceptions:

          Invalid Operation, Unimplemented Operation, Inexact, Overflow');
INSERT INTO "instructions" VALUES('MIPS32','CFC1','


Move Control Word From Floating Point                                                                         CFC1


   31               26 25              21 20            16 15              11 10                               0

        COP1                   CF                                                                0
                                                 rt                 fs
       010001                00010                                                         000 0000 0000

           6                    5                5                  5                           11


     Format: CFC1 rt, fs                                                                          MIPS32 (MIPS I)

     Purpose:

     To copy a word from an FPU control register to a GPR

     Description: rt <- FP_Control[fs]

     Copy the 32-bit word from FP (coprocessor 1) control register fs into GPR rt.

     Restrictions:

     There are a few control registers defined for the floating point unit. The result is UNPREDICTABLE if fs specifies a
     register that does not exist.

     Operation:

         if fs = 0 then
               temp <- FIR
         elseif fs = 25 then
               temp <- 0   24  || FCSR         || FCSR
                                       31..25           23
         elseif fs = 26 then
               temp <- 0   14  || FCSR         || 0 || FCSR
                                                    5                 || 0 2
                                       17..12                   6..2
         elseif fs = 28 then
               temp <- 0   20  || FCSR       || 0 || FCSR
                                                  4              || FCSR
                                       11.7                   24           1..0
         elseif fs = 31 then
               temp <- FCSR
         else
               temp <- UNPREDICTABLE
         endif
         GPR[rt] <- temp
    Exceptions:

    Coprocessor Unusable, Reserved Instruction

    Historical Information:

    For the MIPS I, II and III architectures, the contents of GPR rt are UNPREDICTABLE for the instruction immedi-
    ately following CFC1.

    MIPS V and MIPS32 introduced the three control registers that access portions of FCSR. These registers were not
    available in MIPS I, II, III, or IV.');
INSERT INTO "instructions" VALUES('MIPS32','CFC2','


Move Control Word From Coprocessor 2                                                               CFC2


   31              26 25           21 20             16 15              11 10                       0

        COP2                 CF                                                         0
                                              rt                  rd
       010010              00010                                                  000 0000 0000

          6                  5                5                   5                    11


     Format: CFC2 rt, rd                                                                        MIPS32

     Purpose:

     To copy a word from a Coprocessor 2 control register to a GPR

     Description: rt <- CCR[2,rd]

     Copy the 32-bit word from Coprocessor 2 control register rd into GPR rt.

     Restrictions:

     The result is UNPREDICTABLE if fs specifies a register that does not exist.

     Operation:

         temp <- CCR[2,rd]
         GPR[rt] <- temp


     Exceptions:

     Coprocessor Unusable, Reserved Instruction');
INSERT INTO "instructions" VALUES('MIPS32','CLO','



 Count Leading Ones in Word                                                                                CLO


31              26 25                21 20              16 15             11 10                6   5                0

   SPECIAL2                                                                            0                  CLO
                             rs                  rt                 rd
     011100                                                                          00000              100001

       6                     5                   5                   5                 5                   6


  Format: CLO rd, rs                                                                                            MIPS32

  Purpose:

  To Count the number of leading ones in a word

  Description: rd <- count_leading_ones rs

  Bits 31..0 of GPR rs are scanned from most significant to least significant bit. The number of leading ones is counted
  and the result is written to GPR rd. If all of bits 31..0 were set in GPR rs, the result written to GPR rd is 32.

  Restrictions:

  To be compliant with the MIPS32 and MIPS64 Architecture, software must place the same GPR number in both the
  rt and rd fields of the instruction. The operation of the instruction is UNPREDICTABLE if the rt and rd fields of the
  instruction contain different values.

  Operation:

       temp <- 32
       for i in 31 .. 0
           if GPR[rs] = 0 then
                          i
                temp <- 31 - i
                break
           endif
       endfor
       GPR[rd] <- temp

  Exceptions:

  None');
INSERT INTO "instructions" VALUES('MIPS32','CLZ','



 Count Leading Zeros in Word                                                                              CLZ


31              26 25                21 20             16 15              11 10              6  5                0

   SPECIAL2                                                                           0                  CLZ
                             rs                 rt                rd
     011100                                                                         00000               100000

       6                     5                  5                  5                  5                   6


  Format: CLZ rd, rs                                                                                         MIPS32

  Purpose

   Count the number of leading zeros in a word

  Description: rd <- count_leading_zeros rs

  Bits 31..0 of GPR rs are scanned from most significant to least significant bit. The number of leading zeros is counted
  and the result is written to GPR rd. If no bits were set in GPR rs, the result written to GPR rt is 32.

  Restrictions:

  To be compliant with the MIPS32 and MIPS64 Architecture, software must place the same GPR number in both the
  rt and rd fields of the instruction. The operation of the instruction is UNPREDICTABLE if the rt and rd fields of the
  instruction contain different values.

  Operation:

       temp <- 32
       for i in 31 .. 0
           if GPR[rs] = 1 then
                          i
                temp <- 31 - i
                break
           endif
       endfor
       GPR[rd] <- temp

  Exceptions:

  None');
INSERT INTO "instructions" VALUES('MIPS32','COP2','


Coprocessor Operation to Coprocessor 2                                                                           COP2


   31              26 25 24                                                                                        0

        COP2           CO
                                                                     cofun
        010010          1

          6             1                                              25


     Format: COP2 func                                                                                      MIPS32

     Purpose:

     To performance an operation to Coprocessor 2

     Description: CoprocessorOperation(2, cofun)

     An implementation-dependent operation is performance to Coprocessor 2, with the cofun value passed as an argu-
     ment. The operation may specify and reference internal coprocessor registers, and may change the state of the copro-
     cessor conditions, but does not modify state within the processor. Details of coprocessor operation and internal state
     are described in the documentation for each Coprocessor 2 implementation.

     Restrictions:

     Operation:

         CoprocessorOperation(2, cofun)

     Exceptions:

     Coprocessor Unusable
     Reserved Instruction');
INSERT INTO "instructions" VALUES('MIPS32','CTC1','


Move Control Word to Floating Point                                                                                 CTC1


   31               26 25              21 20             16 15               11 10                                    0

         COP1                  CT                                                                   0
                                                   rt                 fs
        010001               00110                                                           000 0000 0000

           6                    5                  5                  5                            11


     Format:      CTC1     rt, fs                                                                    MIPS32 (MIPS I)

     Purpose:

     To copy a word from a GPR to an FPU control register

     Description: FP_Control[fs] <- rt

     Copy the low word from GPR rt into the FP (coprocessor 1) control register indicated by fs.

     Writing to the floating point Control/Status register, the FCSR, causes the appropriate exception if any Cause bit and
     its corresponding Enable bit are both set. The register is written before the exception occurs. Writing to FEXR to set a
     cause bit whose enable bit is already set, or writing to FENR to set an enable bit whose cause bit is already set causes
     the appropriate exception. The register is written before the exception occurs.

     Restrictions:

     There are a few control registers defined for the floating point unit. The result is UNPREDICTABLE if fs specifies a
     register that does not exist.
    Operation:

        temp <- GPR[rt]31..0
        if fs = 25 then
             if temp               24  then
                      31..8   != 0
                 UNPREDICTABLE
             else
                 FCSR <- temp7..1 || FCSR24 || temp0 || FCSR22..0
             endif
        elseif fs = 26 then
             if temp
                      22..18    != 0 then
                 UNPREDICTABLE
             else
                 FCSR <- FCSR31..18 || temp17..12 || FCSR11..7 ||
                 temp         || FCSR
                       6..2             1..0
             endif
        elseif fs = 28 then
             if temp
                      22..18    != 0 then
                 UNPREDICTABLE
             else
                 FCSR <- FCSR31..25 || temp2 || FCSR23..12 || temp11..7
                 || FCSR           || temp
                            6..2           1..0
             endif
        elseif fs = 31 then
             if temp
                      22..18    != 0 then
                 UNPREDICTABLE
             else
                 FCSR <- temp
             endif
        else
             UNPREDICTABLE
        endif

    Exceptions:

    Coprocessor Unusable, Reserved Instruction

    Floating Point Exceptions:

    Unimplemented Operation, Invalid Operation, Division-by-zero, Inexact, Overflow, Underflow

    Historical Information:

    For the MIPS I, II and III architectures, the contents of floating point control register fs are undefined for the instruc-
    tion immediately following CTC1.

    MIPS V and MIPS32 introduced the three control registers that access portions of FCSR. These registers were not
    available in MIPS I, II, III, or IV.');
INSERT INTO "instructions" VALUES('MIPS32','CTC2','


Move Control Word to Coprocessor 2                                                                       CTC2


   31              26 25           21 20            16 15              11 10                              0

        COP2                CT                                                                0
                                              rt                rd
       010010              00110                                                        000 0000 0000

          6                  5                5                 5                            11


     Format:     CTC2 rt, rd                                                                          MIPS32

     Purpose:

     To copy a word from a GPR to a Coprocessor 2 control register

     Description: CCR[2,rd] <- rt

     Copy the low word from GPR rt into the Coprocessor 2control register indicated by rd.

     Restrictions:

     The result is UNPREDICTABLE if rd specifies a register that does not exist.



     Operation:

         temp <- GPR[rt]
         CCR[2,rd] <- temp

     Exceptions:

     Coprocessor Unusable, Reserved Instruction');
INSERT INTO "instructions" VALUES('MIPS32','CVT.D.fmt','


Floating Point Convert to Double Floating Point                                                                CVT.D.fmt


        31               26 25            21 20               16 15               11 10          6   5                0

             COP1                                    0                                                     CVT.D
                                  fmt                                     fs             fd
            010001                                 00000                                                   100001

                6                  5                 5                     5              5                  6


          Format: CVT.D.S fd, fs                                                                      MIPS32 (MIPS I)
                      CVT.D.W fd, fs                                                                  MIPS32 (MIPS I)
                      CVT.D.L fd, fs                                                                MIPS64 (MIPS III)

          Purpose:

          To convert an FP or fixed point value to double FP

          Description: fd <- convert_and_round(fs)

          The value in FPR fs, in format fmt, is converted to a value in double floating point format and rounded according to
          the current rounding mode in FCSR. The result is placed in FPR fd. If fmt is S or W, then the operation is always
          exact.

          Restrictions:

          The fields fs and fd must specify valid FPRs--fs for type fmt and fd for double floating point--if they are not valid,
          the result is UNPREDICTABLE.

          The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value of the operand
          FPR becomes UNPREDICTABLE.

          Operation:

              StoreFPR (fd, D, ConvertFmt(ValueFPR(fs, fmt), fmt, D))

          Exceptions:

          Coprocessor Unusable, Reserved Instruction

          Floating Point Exceptions:

          Invalid Operation, Unimplemented Operation, Inexact');
INSERT INTO "instructions" VALUES('MIPS32','CVT.S.fmt','


Floating Point Convert to Single Floating Point                                                                   CVT.S.fmt


        31             26 25              21 20               16 15               11 10              6  5                0

             COP1                                      0                                                      CVT.S
                                   fmt                                     fs                fd
            010001                                   00000                                                    100000

                6                   5                  5                   5                  5                  6


          Format: CVT.S.D fd, fs                                                                          MIPS32 (MIPS I)
                     CVT.S.W fd, fs                                                                       MIPS32 (MIPS I)
                     CVT.S.L fd, fs                                                                    MIPS64 (MIPS III)

          Purpose:

          To convert an FP or fixed point value to single FP

          Description: fd <- convert_and_round(fs)

          The value in FPR fs, in format fmt, is converted to a value in single floating point format and rounded according to the
          current rounding mode in FCSR. The result is placed in FPR fd.

          Restrictions:

          The fields fs and fd must specify valid FPRs--fs for type fmt and fd for single floating point. If they are not valid, the
          result is UNPREDICTABLE.

          The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value of the operand
          FPR becomes UNPREDICTABLE.

          Operation:

              StoreFPR(fd, S, ConvertFmt(ValueFPR(fs, fmt), fmt, S))

          Exceptions:

          Coprocessor Unusable, Reserved Instruction

          Floating Point Exceptions:

          Invalid Operation, Unimplemented Operation, Inexact, Overflow, Underflow');
INSERT INTO "instructions" VALUES('MIPS32','CVT.W.fmt','


Floating Point Convert to Word Fixed Point                                                                        CVT.W.fmt


        31              26 25              21 20              16 15               11 10            6   5                  0

             COP1                                      0                                                     CVT.W
                                   fmt                                    fs               fd
            010001                                  00000                                                    100100

                6                   5                  5                   5                5                   6


          Format: CVT.W.S fd, fs                                                                        MIPS32 (MIPS I)
                     CVT.W.D fd, fs                                                                     MIPS32 (MIPS I)

          Purpose:

          To convert an FP value to 32-bit fixed point

          Description: fd <- convert_and_round(fs)

          The value in FPR fs, in format fmt, is converted to a value in 32-bit word fixed point format and rounded according to
          the current rounding mode in FCSR. The result is placed in FPR fd.

          When the source value is Infinity, NaN, or rounds to an integer outside the range -2^31 to 2^31-1, the result cannot be
          represented correctly, an IEEE Invalid Operation condition exists, and the Invalid Operation flag is set in the FCSR. If
          the Invalid Operation Enable bit is set in the FCSR, no result is written to fd and an Invalid Operation exception is
          taken immediately. Otherwise, the default result, 2^31-1, is written to fd.

          Restrictions:

          The fields fs and fd must specify valid FPRs--fs for type fmt and fd for word fixed point--if they are not valid, the
          result is UNPREDICTABLE.

          The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value of the operand
          FPR becomes UNPREDICTABLE.

          Operation:

              StoreFPR(fd, W, ConvertFmt(ValueFPR(fs, fmt), fmt, W))

          Exceptions:

          Coprocessor Unusable, Reserved Instruction

          Floating Point Exceptions:

          Invalid Operation, Unimplemented Operation, Inexact, Overflow');
INSERT INTO "instructions" VALUES('MIPS32','DERET','



    Debug Exception Return                                                                             DERET

   31              26 25 24                                                                  6  5                 0

         COP0          CO                                     0                                       DERET

        010000          1                         000 0000 0000 0000 0000                             011111

           6            1                                     19                                         6


     Format: DERET                                                                                           EJTAG

     Purpose:

     To Return from a debug exception.

     Description:

     DERET returns from Debug Mode and resumes non-debug execution at the instruction whose address is contained in
     the DEPC register. DERET does not execute the next instruction (i.e. it has no delay slot).

     Restrictions:

     A DERET placed between an LL and SC instruction does not cause the SC to fail.

     If the DEPC register with the return address for the DERET was modified by an MTC0 or a DMTC0 instruction, a
     CP0 hazard hazard exists that must be removed via software insertion of the apporpriate number of SSNOP instruc-
     tions.

     The DERET instruction implements a software barrier for all changes in the CP0 state that could affect the fetch and
     decode of the instruction at the PC to which the DERET returns, such as changes to the effective ASID, user-mode
     state, and addressing mode.

     This instruction is legal only if the processor is executing in Debug Mode.The operation of the processor is UNDE-
     FINED if a DERET is executed in the delay slot of a branch or jump instruction.
Debug Exception Return (cont.)                                                 DERET


 Operation:

     Debug
            DM  <- 0
     Debug
            IEXI  <- 0
     if IsMIPS16Implemented() then
         PC <- DEPC31..1 || 0
         ISAMode <- 0 || DEPC0
     else
         PC <- DEPC
     endif

 Exceptions:

  Coprocessor Unusable Exception
  Reserved Instruction Exception');
INSERT INTO "instructions" VALUES('MIPS32','DIV','


Divide Word                                                                                                            DIV


  31                 26 25               21 20             16 15                                6  5                  0

        SPECIAL                                                                0                            DIV
                                 rs                 rt
         000000                                                           00 0000 0000                    011010

            6                    5                  5                          10                            6


      Format: DIV rs, rt                                                                             MIPS32 (MIPS I)

      Purpose:

      To divide a 32-bit signed integers

      Description: (LO, HI) <- rs / rt

      The 32-bit word value in GPR rs is divided by the 32-bit value in GPR rt, treating both operands as signed values.
      The 32-bit quotient is placed into special register LO and the 32-bit remainder isplaced into special register HI.

      No arithmetic exception occurs under any circumstances.

      Restrictions:

      If the divisor in GPR rt is zero, the arithmetic result value is UNPREDICTABLE.

      Operation:
           q   <- GPR[rs]31..0 div GPR[rt]31..0
           LO <- q
           r   <- GPR[rs]31..0 mod GPR[rt]31..0
           HI <- r

      Exceptions:

      None
   Programming Notes:

   No arithmetic exception occurs under any circumstances. If divide-by-zero or overflow conditions are detected and
   some action taken, then the divide instruction is typically followed by additional instructions to check for a zero divi-
   sor and/or for overflow. If the divide is asynchronous then the zero-divisor check can execute in parallel with the
   divide. The action taken on either divide-by-zero or overflow is either a convention within the program itself, or more
   typically within the system software; one possibility is to take a BREAK exception with a code field value to signal
   the problem to the system software.

   As an example, the C programming language in a UNIX environment expects division by zero to either terminate
   the program or execute a program-specified signal handler. C does not expect overflow to cause any exceptional con-
   dition. If the C compiler uses a divide instruction, it also emits code to test for a zero divisor and execute a BREAK
   instruction to inform the operating system if a zero is detected.

   Where the size of the operands are known, software should place the shorter operand in GPR rt. This may reduce the
   latency of the instruction on those processors which implement data-dependent instruction latencies.

   In some processors the integer divide operation may proceed asynchronously and allow other CPU instructions to
   execute before it is complete. An attempt to read LO or HI before the results are written interlocks until the results are
   ready. Asynchronous execution does not affect the program result, but offers an opportunity for performance
   improvement by scheduling the divide so that other instructions can execute in parallel.

   Historical Perspective:

   In MIPS 1 through MIPS III, if either of the two instructions preceding the divide is an MFHI or MFLO, the result of
   the MFHI or MFLO is UNPREDICTABLE. Reads of the HI or LO special register must be separated from subse-
   quent instructions that write to them by two or more instructions. This restriction was removed in MIPS IV and
   MIPS32 and all subsequent levels of the architecture.');
INSERT INTO "instructions" VALUES('MIPS32','DIV.fmt','


Floating Point Divide                                                                                                  DIV.fmt


     31               26 25              21 20             16 15              11 10               6   5                  0

           COP1                                                                                                DIV
                                 fmt               ft                 fs                  fd
          010001                                                                                              000011

             6                    5                 5                 5                   5                     6


       Format: DIV.S fd, fs, ft                                                                         MIPS32 (MIPS I)
                  DIV.D fd, fs, ft                                                                      MIPS32 (MIPS I)

       Purpose:

       To divide FP values

       Description: fd <- fs / ft

       The value in FPR fs is divided by the value in FPR ft. The result is calculated to infinite precision, rounded according
       to the current rounding mode in FCSR, and placed into FPR fd. The operands and result are values in format fmt.

       Restrictions:

       The fields fs, ft, and fd must specify FPRs valid for operands of type fmt; if they are not valid, the result is UNPRED-
       ICABLE.

       The operands must be values in format fmt; if they are not, the result is UNPREDICTABLE and the value of the
       operand FPRs becomes UNPREDICTABLE.

       Operation:

           StoreFPR (fd, fmt, ValueFPR(fs, fmt) / ValueFPR(ft, fmt))

       Exceptions:

       Coprocessor Unusable, Reserved Instruction

       Floating Point Exceptions:

       Inexact, Invalid Operation, Unimplemented Operation, Division-by-zero, Overflow, Underflow');
INSERT INTO "instructions" VALUES('MIPS32','DIVU','


Divide Unsigned Word                                                                                                  DIVU


  31                 26 25               21 20             16 15                                6  5                   0

        SPECIAL                                                                 0                         DIVU
                                 rs                 rt
         000000                                                            00 0000 0000                   011011

            6                    5                  5                           10                            6


      Format: DIVU rs, rt                                                                            MIPS32 (MIPS I)

      Purpose:

      To divide a 32-bit unsigned integers

      Description: (LO, HI) <- rs / rt

      The 32-bit word value in GPR rs is divided by the 32-bit value in GPR rt, treating both operands as unsigned values.
      The 32-bit quotient is placed into special register LO and the 32-bit remainder is placed into special register HI.

      No arithmetic exception occurs under any circumstances.

      Restrictions:

      If the divisor in GPR rt is zero, the arithmetic result value is undefined.

      Operation:

           q   <- (0 || GPR[rs]31..0) div (0 || GPR[rt]31..0)
           r   <- (0 || GPR[rs]31..0) mod (0 || GPR[rt]31..0)
           LO <- sign_extend(q31..0)
           HI <- sign_extend(r31..0)

      Exceptions:

      None

      Programming Notes:

      See !=Programming Notes!= for the DIV instruction.

      Historical Perspective:

      In MIPS 1 through MIPS III, if either of the two instructions preceding the divide is an MFHI or MFLO, the result of
      the MFHI or MFLO is UNPREDICTABLE. Reads of the HI or LO special register must be separated from subse-
      quent instructions that write to them by two or more instructions. This restriction was removed in MIPS IV and
      MIPS32 and all subsequent levels of the architecture.');
INSERT INTO "instructions" VALUES('MIPS32','ERET','



Exception Return                                                                                                 ERET

 31                26 25 24                                                                  6   5                  0

        COP0           CO                                     0                                          ERET

       010000          1                           000 0000 0000 0000 0000                              011000

          6            1                                      19                                           6


    Format: ERET                                                                                              MIPS32

    Purpose:

    To return from interrupt, exception, or error trap.

    Description:

    ERET returns to the interrupted instruction at the completion of interrupt, exception, or error trap processing. ERET
    does not execute the next instruction (i.e., it has no delay slot).

    Restrictions:

    The operation of the processor is UNDEFINED if an ERET is executed in the delay slot of a branch or jump instruc-
    tion.

    An ERET placed between an LL and SC instruction will always cause the SC to fail.

    ERET implements a software barrier for all changes in the CP0 state that could affect the fetch and decode of the
    instruction at the PC to which the ERET returns, such as changes to the effective ASID, user-mode state, and address-
    ing mode.

    Operation:

         if Status        = 1 then
                     ERL
             temp <- ErrorEPC
             Status
                      ERL  <- 0
         else
             temp <- EPC
             Status
                      EXL  <- 0
         endif
         if IsMIPS16Implemented() then
             PC <- temp31..1 || 0
             ISAMode <- temp0
         else
             PC <- temp
         endif
         LLbit <- 0

    Exceptions:
             Coprocessor Unusable Exception');
INSERT INTO "instructions" VALUES('MIPS32','FLOOR.W.fmt','


Floating Point Floor Convert to Word Fixed Point                                                              FLOOR.W.fmt


         31             26 25               21 20              16 15               11 10           6   5                   0

              COP1                                      0                                                   FLOOR.W
                                    fmt                                    fs               fd
             010001                                  00000                                                    001111

                 6                   5                  5                   5                5                   6


           Format: FLOOR.W.S           fd, fs                                                           MIPS32 (MIPS II)
                      FLOOR.W.D        fd, fs                                                           MIPS32 (MIPS II)

           Purpose:

           To convert an FP value to 32-bit fixed point, rounding down

           Description: fd <- convert_and_round(fs)

           The value in FPR fs, in format fmt, is converted to a value in 32-bit word fixed point format and rounded toward -inf
           (rounding mode 3). The result is placed in FPR fd.

           When the source value is Infinity, NaN, or rounds to an integer outside the range -2^31 to 2^31-1, the result cannot be
           represented correctly, an IEEE Invalid Operation condition exists, and the Invalid Operation flag is set in the FCSR. If
           the Invalid Operation Enable bit is set in the FCSR, no result is written to fd and an Invalid Operation exception is
           taken immediately. Otherwise, the default result, 2^31-1, is written to fd.

           Restrictions:

           The fields fs and fd must specify valid FPRs--fs for type fmt and fd for word fixed point--if they are not valid, the
           result is UNPREDICTABLE.

           The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value of the operand
           FPR becomes UNPREDICTABLE.

           Operation:

               StoreFPR(fd, W, ConvertFmt(ValueFPR(fs, fmt), fmt, W))

           Exceptions:

           Coprocessor Unusable, Reserved Instruction

           Floating Point Exceptions:

           Invalid Operation, Unimplemented Operation, Inexact, Overflow');
INSERT INTO "instructions" VALUES('MIPS32','J','


Jump                                                                                                                        J


  31                26 25                                                                                              0

          J
                                                                   instr_index
       000010

          6                                                             26


     Format: J target                                                                                MIPS32 (MIPS I)

     Purpose:

     To branch within the current 256 MB-aligned region

     Description:

     This is a PC-region branch (not PC-relative); the effective target address is in the !=current!= 256 MB-aligned region.
     The low 28 bits of the target address is the instr_index field shifted left 2 bits. The remaining upper bits are the corre-
     sponding bits of the address of the instruction in the delay slot (not the branch itself).

     Jump to the effective target address. Execute the instruction that follows the jump, in the branch delay slot, before
     executing the jump itself.

     Restrictions:

     Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
     delay slot of a branch or jump.

     Operation:

         I:
         I+1:PC <- PCGPRLEN..28 || instr_index || 0               2


     Exceptions:

     None

     Programming Notes:

     Forming the branch target address by catenating PC and index bits rather than adding a signed offset to the PC is an
     advantage if all program code addresses fit into a 256 MB region aligned on a 256 MB boundary. It allows a branch
     from anywhere in the region to anywhere in the region, an action not allowed by a signed relative offset.

     This definition creates the following boundary case: When the jump instruction is in the last word of a 256 MB
     region, it can branch only to the following 256 MB region containing the branch delay slot.');
INSERT INTO "instructions" VALUES('MIPS32','JAL','


Jump and Link                                                                                                            JAL


  31                26 25                                                                                              0

         JAL
                                                                   instr_index
        000011

          6                                                             26


     Format: JAL target                                                                              MIPS32 (MIPS I)

     Purpose:

     To execute a procedure call within the current 256 MB-aligned region

     Description:

     Place the return address link in GPR 31. The return link is the address of the second instruction following the branch,
     at which location execution continues after a procedure call.

     This is a PC-region branch (not PC-relative); the effective target address is in the !=current!= 256 MB-aligned region.
     The low 28 bits of the target address is the instr_index field shifted left 2 bits. The remaining upper bits are the corre-
     sponding bits of the address of the instruction in the delay slot (not the branch itself).

     Jump to the effective target address. Execute the instruction that follows the jump, in the branch delay slot, before
     executing the jump itself.

     Restrictions:

     Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
     delay slot of a branch or jump.

     Operation:

         I: GPR[31]<- PC + 8
         I+1:PC         <- PCGPRLEN..28 || instr_index || 0           2


     Exceptions:

     None

     Programming Notes:

     Forming the branch target address by catenating PC and index bits rather than adding a signed offset to the PC is an
     advantage if all program code addresses fit into a 256 MB region aligned on a 256 MB boundary. It allows a branch
     from anywhere in the region to anywhere in the region, an action not allowed by a signed relative offset.

     This definition creates the following boundary case: When the branch instruction is in the last word of a 256 MB
     region, it can branch only to the following 256 MB region containing the branch delay slot.');
INSERT INTO "instructions" VALUES('MIPS32','JALR','


Jump and Link Register                                                                                               JALR




  31               26 25               21 20             16 15              11 10              6    5                 0

       SPECIAL                                    0                                                       JALR
                                rs                                    rd               hint
        000000                                  00000                                                     001001

           6                    5                 5                   5                 5                    6


     Format: JALR rs (rd = 31 implied)                                                               MIPS32 (MIPS I)
                 JALR rd, rs                                                                         MIPS32 (MIPS I)

     Purpose:

     To execute a procedure call to an instruction address in a register

     Description: rd <- return_addr, PC <- rs

     Place the return address link in GPR rd. The return link is the address of the second instruction following the branch,
     where execution continues after a procedure call.

     For processors that do not implement the MIPS16 ASE:

        . Jump to the effective target address in GPR rs. Execute the instruction that follows the jump, in the branch delay
          slot, before executing the jump itself.

     For processors that do implement the MIPS16 ASE:

        . Jump to the effective target address in GPR rs. Set the ISA Mode bit to the value in GPR rs bit 0. Bit 0 of the
          target address is always zero so that no Address Exceptions occur when bit 0 of the source register is one

     At this time the only defined hint field value is 0, which sets default handling of JALR. Future versions of the archi-
     tecture may define additional hint values.

     Restrictions:

     Register specifiers rs and rd must not be equal, because such an instruction does not have the same effect when reex-
     ecuted. The result of executing such an instruction is undefined. This restriction permits an exception handler to
     resume execution by reexecuting the branch when an exception occurs in the branch delay slot.

     The effective target address in GPR rs must be naturally-aligned. For processors that do not implement the MIPS16
     ASE, if either of the two least-significant bits are not zero, an Address Error exception occurs when the branch target
     is subsequently fetched as an instruction. For processors that do implement the MIPS16 ASE, if bit 0 is zero and bit 1
     is one, an Address Error exception occurs when the jump target is subsequently fetched as an instruction.

     Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
     delay slot of a branch or jump.');
INSERT INTO "instructions" VALUES('MIPS32','Jump','

Operation:

    I: temp <- GPR[rs]
         GPR[rd] <- PC + 8
    I+1:if Config1        = 0 then
                       CA
             PC <- temp
         else
             PC <- tempGPRLEN-1..1 || 0
             ISAMode <- temp0
         endif



Exceptions:

None

Programming Notes:

This is the only branch-and-link instruction that can select a register for the return link; all other link instructions use
GPR 31. The default register for GPR rd, if omitted in the assembly language instruction, is GPR 31.');
INSERT INTO "instructions" VALUES('MIPS32','JR','


Jump Register                                                                                                         JR


  31               26 25               21 20                                11 10             6   5                 0

       SPECIAL                                               0                                            JR
                                rs                                                    hint
        000000                                        00 0000 0000                                     001000

          6                     5                           10                         5                   6


     Format:     JR rs                                                                             MIPS32 (MIPS I)

     Purpose:

     To execute a branch to an instruction address in a register

     Description: PC <- rs

     Jump to the effective target address in GPR rs. Execute the instruction following the jump, in the branch delay slot,
     before jumping.

     For processors that implement the MIPS16 ASE, set the ISA Mode bit to the value in GPR rs bit 0. Bit 0 of the target
     address is always zero so that no Address Exceptions occur when bit 0 of the source register is one

     Restrictions:

     The effective target address in GPR rs must be naturally-aligned. For processors that do not implement the MIPS16
     ASE, if either of the two least-significant bits are not zero, an Address Error exception occurs when the branch target
     is subsequently fetched as an instruction. For processors that do implement the MIPS16 ASE, if bit 0 is zero and bit 1
     is one, an Address Error exception occurs when the jump target is subsequently fetched as an instruction.

     At this time the only defined hint field value is 0, which sets default handling of JR. Future versions of the architec-
     ture may define additional hint values.

     Processor operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is placed in the
     delay slot of a branch or jump.

     Operation:

          I: temp <- GPR[rs]
          I+1:if Config1         = 0 then
                             CA
                   PC <- temp
               else
                   PC <- tempGPRLEN-1..1 || 0
                   ISAMode <- temp0
               endif

     Exceptions:

     None');
INSERT INTO "instructions" VALUES('MIPS32','LB','


Load Byte                                                                                                             LB


   31               26 25              21 20             16 15                                                      0

          LB
                             base                rt                                   offset
        100000

           6                   5                  5                                    16


      Format:    LB rt, offset(base)                                                               MIPS32 (MIPS I)

      Purpose:

      To load a byte from memory as a signed value

      Description: rt <- memory[base+offset]

      The contents of the 8-bit byte at the memory location specified by the effective address are fetched, sign-extended,
      and placed in GPR rt. The 16-bit signed offset is added to the contents of GPR base to form the effective address.

      Restrictions:

      None

      Operation:

          vAddr <- sign_extend(offset) + GPR[base]
          (pAddr, CCA)<- AddressTranslation (vAddr, DATA, LOAD)
          pAddr <- pAddrPSIZE-1..2 || (pAddr1..0 xor ReverseEndian )               2

          memword<- LoadMemory (CCA, BYTE, pAddr, vAddr, DATA)
          byte     <- vAddr1..0 xor BigEndianCPU          2

          GPR[rt]<- sign_extend(memword7+8*byte..8*byte)

      Exceptions:

      TLB Refill, TLB Invalid, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','LBU','


Load Byte Unsigned                                                                                                  LBU


   31               26 25              21 20             16 15                                                      0

          LBU
                             base                rt                                   offset
        100100

           6                   5                  5                                    16


      Format: LBU rt, offset(base)                                                                 MIPS32 (MIPS I)

      Purpose:

      To load a byte from memory as an unsigned value

      Description: rt <- memory[base+offset]

      The contents of the 8-bit byte at the memory location specified by the effective address are fetched, zero-extended,
      and placed in GPR rt. The 16-bit signed offset is added to the contents of GPR base to form the effective address.

      Restrictions:

      None

      Operation:

          vAddr <- sign_extend(offset) + GPR[base]
          (pAddr, CCA)<- AddressTranslation (vAddr, DATA, LOAD)
          pAddr <- pAddrPSIZE-1..2 || (pAddr1..0 xor ReverseEndian )               2

          memword<- LoadMemory (CCA, BYTE, pAddr, vAddr, DATA)
          byte     <- vAddr1..0 xor BigEndianCPU          2

          GPR[rt]<- zero_extend(memword7+8*byte..8*byte)

      Exceptions:

      TLB Refill, TLB Invalid, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','LDC1','


Load Doubleword to Floating Point                                                                                 LDC1


   31              26 25              21 20              16 15                                                      0

        LDC1
                            base                 ft                                  offset
       110101

          6                   5                  5                                     16


     Format: LDC1 ft, offset(base)                                                               MIPS32 (MIPS II)

     Purpose:

     To load a doubleword from memory to an FPR

     Description: ft <- memory[base+offset]

     The contents of the 64-bit doubleword at the memory location specified by the aligned effective address are fetched
     and placed in FPR ft. The 16-bit signed offset is added to the contents of GPR base to form the effective address.

     Restrictions:

     An Address Error exception occurs if EffectiveAddress   2..0!= 0 (not doubleword-aligned).

     Operation:

         vAddr <- sign_extend(offset) + GPR[base]
         if vAddr           3
                    2..0 != 0 then
             SignalException(AddressError)
         endif
         (pAddr, CCA) <- AddressTranslation (vAddr, DATA, LOAD)
         memdoubleword <- LoadMemory(CCA, DOUBLEWORD, pAddr, vAddr, DATA)
         StoreFPR(ft, UNINTERPRETED_DOUBLEWORD, memdoubleword)



     Exceptions:

     Coprocessor Unusable, Reserved Instruction, TLB Refill, TLB Invalid, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','LDC2','


Load Doubleword to Coprocessor 2                                                                               LDC2


   31              26 25             21 20            16 15                                                     0

        LDC2
                            base                rt                                   offset
        110110

           6                  5                 5                                     16


     Format: LDC2 rt, offset(base)                                                                       MIPS32

     Purpose:

     To load a doubleword from memory to a Coprocessor 2 register

     Description: rt <- memory[base+offset]

     The contents of the 64-bit doubleword at the memory location specified by the aligned effective address are fetched
     and placed in Coprocessor 2 register rt. The 16-bit signed offset is added to the contents of GPR base to form the
     effective address.

     Restrictions:

     An Address Error exception occurs if EffectiveAddress 2..0!= 0 (not doubleword-aligned).

     Operation:

         vAddr <- sign_extend(offset) + GPR[base]
         if vAddr           3
                    2..0 != 0 then SignalException(AddressError) endif
         (pAddr, CCA) <- AddressTranslation (vAddr, DATA, LOAD)
         memdoubleword <- LoadMemory(CCA, DOUBLEWORD, pAddr, vAddr, DATA)
         CPR[2,rt,0] <- memdoubleword

     Exceptions:

     Coprocessor Unusable, Reserved Instruction, TLB Refill, TLB Invalid, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','LH','


Load Halfword                                                                                                       LH


   31              26 25            21 20              16 15                                                      0

          LH
                            base                rt                                   offset
        100001

           6                 5                  5                                      16


     Format: LH rt, offset(base)                                                                 MIPS32 (MIPS I)

     Purpose:

     To load a halfword from memory as a signed value

     Description: rt <- memory[base+offset]

     The contents of the 16-bit halfword at the memory location specified by the aligned effective address are fetched,
     sign-extended, and placed in GPR rt. The 16-bit signed offset is added to the contents of GPR base to form the effec-
     tive address.

     Restrictions:

     The effective address must be naturally-aligned. If the least-significant bit of the address is non-zero, an Address
     Error exception occurs.

     Operation:

          vAddr <- sign_extend(offset) + GPR[base]
          if vAddr != 0 then
                    0
              SignalException(AddressError)
          endif
          (pAddr, CCA) <- AddressTranslation (vAddr, DATA, LOAD)
          pAddr <- pAddrPSIZE1..2 || (pAddr1..0 xor (ReverseEndian || 0))
          memword <- LoadMemory (CCA, HALFWORD, pAddr, vAddr, DATA)
          byte    <- vAddr1..0 xor (BigEndianCPU || 0)
          GPR[rt] <- sign_extend(memword15+8*byte..8*byte)

     Exceptions:

     TLB Refill, TLB Invalid, Bus Error, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','LHU','


Load Halfword Unsigned                                                                                            LHU


   31              26 25            21 20              16 15                                                      0

         LHU
                            base                rt                                   offset
        100101

           6                 5                  5                                      16


     Format: LHU rt, offset(base)                                                                MIPS32 (MIPS I)

     Purpose:

     To load a halfword from memory as an unsigned value

     Description: rt <- memory[base+offset]

     The contents of the 16-bit halfword at the memory location specified by the aligned effective address are fetched,
     zero-extended, and placed in GPR rt. The 16-bit signed offset is added to the contents of GPR base to form the effec-
     tive address.

     Restrictions:

     The effective address must be naturally-aligned. If the least-significant bit of the address is non-zero, an Address
     Error exception occurs.

     Operation:

          vAddr <- sign_extend(offset) + GPR[base]
          if vAddr != 0 then
                    0
              SignalException(AddressError)
          endif
          (pAddr, CCA) <- AddressTranslation (vAddr, DATA, LOAD)
          pAddr <- pAddrPSIZE1..2 || (pAddr1..0 xor (ReverseEndian || 0))
          memword <- LoadMemory (CCA, HALFWORD, pAddr, vAddr, DATA)
          byte    <- vAddr1..0 xor (BigEndianCPU || 0)
          GPR[rt] <- zero_extend(memword15+8*byte..8*byte)

     Exceptions:

     TLB Refill, TLB Invalid, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','LL','


Load Linked Word                                                                                                       LL


   31               26 25               21 20             16 15                                                     0

           LL
                               base                rt                                  offset
        110000

            6                   5                  5                                    16


      Format: LL rt, offset(base)                                                                 MIPS32 (MIPS II)

      Purpose:

      To load a word from memory for an atomic read-modify-write

      Description: rt <- memory[base+offset]

      The LL and SC instructions provide the primitives to implement atomic read-modify-write (RMW) operations for
      cached memory locations.

      The 16-bit signed offset is added to the contents of GPR base to form an effective address. The contents of the 32-bit
      word at the memory location specified by the aligned effective address are fetched, sign-extended to the GPR register
      length if necessary, and written into GPR rt.

      This begins a RMW sequence on the current processor. There can be only one active RMW sequence per processor.

      When an LL is executed it starts an active RMW sequence replacing any other sequence that was active.

      The RMW sequence is completed by a subsequent SC instruction that either completes the RMW sequence atomi-
      cally and succeeds, or does not and fails.

      Executing LL on one processor does not cause an action that, by itself, causes an SC for the same block to fail on
      another processor.

      An execution of LL does not have to be followed by execution of SC; a program is free to abandon the RMW
      sequence without attempting a write.

      Restrictions:

      The addressed location must be cached; if it is not, the result is undefined.

      The effective address must be naturally-aligned. If either of the 2 least-significant bits of the effective address is
      non-zero, an Address Error exception occurs.

      Operation:

          vAddr <- sign_extend(offset) + GPR[base]
          if vAddr             2
                     1..0   != 0 then
               SignalException(AddressError)
          endif
          (pAddr, CCA) <- AddressTranslation (vAddr, DATA, LOAD)
          memword <- LoadMemory (CCA, WORD, pAddr, vAddr, DATA)
          GPR[rt] <- memword
          LLbit <- 1
    Exceptions:

    TLB Refill, TLB Invalid, Address Error, Reserved Instruction

    Programming Notes:

    There is no Load Linked Word Unsigned operation corresponding to Load Word Unsigned.');
INSERT INTO "instructions" VALUES('MIPS32','LUI','


Load Upper Immediate                                                                                            LUI


   31             26 25               21 20            16 15                                                    0

         LUI                   0
                                                  rt                             immediate
       001111                00000

          6                    5                  5                                  16


     Format: LUI rt, immediate                                                                 MIPS32 (MIPS I)

     Purpose:

     To load a constant into the upper half of a word

     Description: rt <- immediate || 0         16


     The 16-bit immediate is shifted left 16 bits and concatenated with 16 bits of low-order zeros. The 32-bit result is
     placed into GPR rt.

     Restrictions:

     None

     Operation:

         GPR[rt] <- immediate || 0          16


     Exceptions:

     None');
INSERT INTO "instructions" VALUES('MIPS32','LW','


Load Word                                                                                                             LW


   31              26 25            21 20               16 15                                                         0

         LW
                            base                 rt                                    offset
       100011

          6                   5                  5                                      16


     Format: LW rt, offset(base)                                                                    MIPS32 (MIPS I)

     Purpose:

     To load a word from memory as a signed value

     Description: rt <- memory[base+offset]

     The contents of the 32-bit word at the memory location specified by the aligned effective address are fetched,
     sign-extended to the GPR register length if necessary, and placed in GPR rt. The 16-bit signed offset is added to the
     contents of GPR base to form the effective address.

     Restrictions:

     The effective address must be naturally-aligned. If either of the 2 least-significant bits of the address is non-zero, an
     Address Error exception occurs.

     Operation:

         vAddr <- sign_extend(offset) + GPR[base]
         if vAddr           2
                    1..0 != 0 then
              SignalException(AddressError)
         endif
         (pAddr, CCA)<- AddressTranslation (vAddr, DATA, LOAD)
         memword<- LoadMemory (CCA, WORD, pAddr, vAddr, DATA)
         GPR[rt]<- memword

     Exceptions:

     TLB Refill, TLB Invalid, Bus Error, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','LWC1','


Load Word to Floating Point                                                                                    LWC1


   31             26 25              21 20           16 15                                                       0

        LWC1
                            base                rt                                 offset
       110001

          6                  5                  5                                   16


     Format: LWC1 ft, offset(base)                                                              MIPS32 (MIPS I)

     Purpose:

     To load a word from memory to an FPR

     Description: ft <- memory[base+offset]

     The contents of the 32-bit word at the memory location specified by the aligned effective address are fetched and
     placed into the low word of coprocessor 1 general register ft. The 16-bit signed offset is added to the contents of
     GPR base to form the effective address.

     Restrictions:

     An Address Error exception occurs if EffectiveAddress1..0!= 0 (not word-aligned).

     Operation:

         /* mem is aligned 64 bits from memory.               Pick out correct bytes. */
         vAddr <- sign_extend(offset) + GPR[base]
         if vAddr           2
                    1..0 != 0 then
             SignalException(AddressError)
         endif
         (pAddr, CCA) <- AddressTranslation (vAddr, DATA, LOAD)

         memword <- LoadMemory(CCA, WORD, pAddr, vAddr, DATA)

         StoreFPR(ft, UNINTERPRETED_WORD,
                      memword)

     Exceptions:

     TLB Refill, TLB Invalid, Address Error, Reserved Instruction, Coprocessor Unusable');
INSERT INTO "instructions" VALUES('MIPS32','LWC2','


Load Word to Coprocessor 2                                                                                         LWC2


   31              26 25             21 20           16 15                                                           0

        LWC2
                            base                rt                                   offset
        110010

          6                   5                 5                                      16


     Format: LWC2 rt, offset(base)                                                                MIPS32 (MIPS I)

     Purpose:

     To load a word from memory to a COP2 register

     Description: rt <- memory[base+offset]

     The contents of the 32-bit word at the memory location specified by the aligned effective address are fetched and
     placed into the low word of COP2 (Coprocessor 2) general register rt. The 16-bit signed offset is added to the contents
     of GPR base to form the effective address.

     Restrictions:

     An Address Error exception occurs if EffectiveAddress1..0!= 0 (not word-aligned).

     Operation:

         vAddr <- sign_extend(offset) + GPR[base]
         if vAddr             2
                     12..0!= 0 then
             SignalException(AddressError)
         endif
         (pAddr, CCA) <- AddressTranslation (vAddr, DATA, LOAD)

         memword <- LoadMemory(CCA, DOUBLEWORD, pAddr, vAddr, DATA)

         CPR[2,rt,0] <- memword

     Exceptions:

     TLB Refill, TLB Invalid, Address Error, Reserved Instruction, Coprocessor Unusable');
INSERT INTO "instructions" VALUES('MIPS32','LWL','


Load Word Left                                                                                                         LWL


   31             26 25                21 20             16 15                                                         0

         LWL
                              base                 rt                                    offset
        100010

          6                    5                   5                                      16


     Format:     LWL rt, offset(base)                                                                MIPS32 (MIPS I)

     Purpose:

     To load the most-significant part of a word as a signed value from an unaligned memory address

     Description: rt <- rt MERGE memory[base+offset]

     The 16-bit signed offset is added to the contents of GPR base to form an effective address (EffAddr). EffAddr is the
     address of the most-significant of 4 consecutive bytes forming a word (W) in memory starting at an arbitrary byte
     boundary.

     The most-significant 1 to 4 bytes of W is in the aligned word containing the EffAddr. This part of W is loaded into the
     most-significant (left) part of the word in GPR rt. The remaining least-significant part of the word in GPR rt is
     unchanged.

     The figure below illustrates this operation using big-endian byte ordering for 32-bit and 64-bit registers. The 4 con-
     secutive bytes in 2..5 form an unaligned word starting at location 2. A part of W, 2 bytes, is in the aligned word con-
     taining the most-significant byte at 2. First, LWL loads these 2 bytes into the left part of the destination register word
     and leaves the right part of the destination word unchanged. Next, the complementary LWR loads the remainder of
     the unaligned word

                              Figure 3-2 Unaligned Word Load Using LWL and LWR

                      Word at byte 2 in big-endian memory; each memory byte contains its own address

                             most      - significance -     least

                            0   1     2    3    4     5  6     7  8     9       Memory initial contents


                                      e     f   g     h          GPR 24 Initial contents


                                      2    3    g     h          After executing LWL $24,2($0)


                                      2    3    4     5          Then after LWR $24,5($0)');
INSERT INTO "instructions" VALUES('MIPS32','Load','
The bytes loaded from memory to the destination register depend on both the offset of the effective address within an
aligned word, that is, the low 2 bits of the address (vAddr       1..0), and the current byte-ordering mode of the processor
(big- or little-endian). The figure below shows the bytes loaded for every combination of offset and byte ordering.

                               Figure 3-3 Bytes Loaded by LWL Instruction


                 Memory contents and byte offsets                              Initial contents of Dest Register

       0      1     2    3 <-big-endian

        I      J   K     L        offset (vAddr    )                  e      f     g     h
                                                1..0

       3      2     1    0 <-little-endian                           most                least

      most              least                                          -- significance --

        -- significance --

                         Destination register contents after instruction (shaded is unchanged)

                                  Big-endian           vAddr1..0          Little-endian

                              I     J     K     L          0          L      f     g     h

                              J    K      L     h          1          K     L      g     h

                              K    L      g     h          2          J     K      L     h

                              L     f     g     h          3           I     J     K     L');
INSERT INTO "instructions" VALUES('MIPS32','LWR','


Load Word Right                                                                                                      LWR


   31              26 25               21 20               16 15                                                      0

         LWR
                              base                 rt                                  offset
        100110

           6                   5                   5                                     16


     Format: LWR rt, offset(base)                                                                   MIPS32 (MIPS I)

     Purpose:

     To load the least-significant part of a word from an unaligned memory address as a signed value

     Description: rt <- rt MERGE memory[base+offset]

     The 16-bit signed offset is added to the contents of GPR base to form an effective address (EffAddr). EffAddr is the
     address of the least-significant of 4 consecutive bytes forming a word (W) in memory starting at an arbitrary byte
     boundary.

     A part of W, the least-significant 1 to 4 bytes, is in the aligned word containing EffAddr. This part of W is loaded into
     the least-significant (right) part of the word in GPR rt. The remaining most-significant part of the word in GPR rt is
     unchanged.

     Executing both LWR and LWL, in either order, delivers a sign-extended word value in the destination register.

     The figure below illustrates this operation using big-endian byte ordering for 32-bit and 64-bit registers. The 4 con-
     secutive bytes in 2..5 form an unaligned word starting at location 2. A part of W, 2 bytes, is in the aligned word con-
     taining the least-significant byte at 5. First, LWR loads these 2 bytes into the right part of the destination register.
     Next, the complementary LWL loads the remainder of the unaligned word.
                             Figure 3-4 Unaligned Word Load Using LWL and LWR

                      Word at byte 2 in big-endian memory; each memory byte contains its own address

                          most         - significance -       least

                           0    1     2    3    4     5  6     7    8    9       Memory initial contents


                                      e     f   g     h           GPR 24 Initial contents


                                      e     f   4     5           After executing LWR $24,5($0)


                                      2    3    4     5           Then after LWL $24,2($0)




   The bytes loaded from memory to the destination register depend on both the offset of the effective address within an
   aligned word, that is, the low 2 bits of the address (vAddr   1..0), and the current byte-ordering mode of the processor
   (big- or little-endian). The figure below shows the bytes loaded for every combination of offset and byte ordering.
                                 Figure 3-5 Bytes Loaded by LWL Instruction


                   Memory contents and byte offsets                             Initial contents of Dest Register

           0     1    2    3 <-big-endian

            I    J   K     L        offset (vAddr1..0)                  e     f     g     h

           3     2    1    0 <-little-endian                           most               least

         most             least                                          -- significance--

            -- significance --

                           Destination register contents after instruction (shaded is unchanged)



                                    Big-endian           vAddr1..0            Little-endian         Little-endian

                                e     f     g     I          0          I     J     K     L

                                e     f      I    J          1          e     I     J     K

                                e     I     J     K          2          e     f     I     J

                                I     J     K     L          3          e     f     g     I
   Restrictions:

   None

   Operation:

       vAddr <- sign_extend(offset) + GPR[base]
       (pAddr, CCA)<- AddressTranslation (vAddr, DATA, LOAD)
       pAddr <- pAddrPSIZE-1..2 || (pAddr1..0 xor ReverseEndian )                 2

       if BigEndianMem = 0 then
            pAddr<- pAddrPSIZE-1..2 || 0        2

       endif
       byte      <- vAddr1..0 xor BigEndianCPU           2

       memword<- LoadMemory (CCA, byte, pAddr, vAddr, DATA)
       temp      <- memword31..32-8*byte || GPR[rt]318*byte..0
       GPR[rt]<- temp

   Exceptions:

   TLB Refill, TLB Invalid, Bus Error, Address Error

   Programming Notes:

   The architecture provides no direct support for treating unaligned words as unsigned values, that is, zeroing bits
   63..32 of the destination register when bit 31 is loaded.

   Historical Information

   In the MIPS I architecture, the LWL and LWR instructions were exceptions to the load-delay scheduling restriction.
   A LWL or LWR instruction which was immediately followed by another LWL or LWR instruction, and used the same
   destination register would correctly merge the 1 to 4 loaded bytes with the data loaded by the previous instruction. All
   such restrictions were removed from the architecture in MIPS II.');
INSERT INTO "instructions" VALUES('MIPS32','MADD','



 Multiply and Add Word to Hi,Lo                                                                         MADD


31              26 25               21 20             16 15               11 10             6   5                0

   SPECIAL2                                                       0                  0                MADD
                             rs                rt
     011100                                                      0000              00000              000000

        6                    5                 5                  5                  5                  6


  Format: MADD rs, rt                                                                                       MIPS32

  Purpose:

  To multiply two words and add the result to Hi, Lo

  Description: (LO,HI) <- (rs x rt) + (LO,HI)

  The 32-bit word value in GPR rs is multiplied by the 32-bit word value in GPR rt, treating both operands as signed
  values, to produce a 64-bit result. The product is added to the 64-bit concatenated values of HI and LO.. The most sig-
  nificant 32 bits of the result are written into HI and the least signficant 32 bits are written into LO. No arithmetic
  exception occurs under any circumstances.

  Restrictions:

  None

  This instruction does not provide the capability of writing directly to a target GPR.

  Operation:

      temp <- (HI || LO) + (GPR[rs] * GPR[rt])
      HI <- temp63..32
      LO <- temp31..0

  Exceptions:

  None

  Programming Notes:

  Where the size of the operands are known, software should place the shorter operand in GPR rt. This may reduce the
  latency of the instruction on those processors which implement data-dependent instruction latencies.');
INSERT INTO "instructions" VALUES('MIPS32','MADDU','



 Multiply and Add Unsigned Word to Hi,Lo                                                               MADDU


31              26 25               21 20             16 15               11 10             6   5                0

   SPECIAL2                                                       0                  0               MADDU
                             rs                rt
     011100                                                     00000              00000              000001

        6                    5                 5                  5                  5                  6


  Format: MADDU rs, rt                                                                                      MIPS32

  Purpose:

  To multiply two unsigned words and add the result to Hi, Lo.

  Description: (LO,HI) <- (rs x rt) + (LO,HI)

  The 32-bit word value in GPR rs is multiplied by the 32-bit word value in GPR rt, treating both operands as unsigned
  values, to produce a 64-bit result. The product is added to the 64-bit concatenated values of HI and LO.. The most sig-
  nificant 32 bits of the result are written into HI and the least signficant 32 bits are written into LO. No arithmetic
  exception occurs under any circumstances.

  Restrictions:

  None

  This instruction does not provide the capability of writing directly to a target GPR.

  Operation:

      temp <- (HI || LO) + (GPR[rs] * GPR[rt])
      HI <- temp63..32
      LO <- temp31..0

  Exceptions:

  None

  Programming Notes:

  Where the size of the operands are known, software should place the shorter operand in GPR rt. This may reduce the
  latency of the instruction on those processors which implement data-dependent instruction latencies.');
INSERT INTO "instructions" VALUES('MIPS32','MFC0','



 Move from Coprocessor 0                                                                                   MFC0


31               26 25              21 20              16 15               11 10                         3   2      0

      COP0                  MF                                                               0
                                                 rt                rd                                            sel
      010000               00000                                                         00000000

        6                    5                   5                 5                         8                   3


  Format: MFC0 rt, rd                                                                                         MIPS32

  Purpose:

  To move the contents of a coprocessor 0 register to a general register.

  Description: rt <- CPR[0,rd,sel]

  The contents of the coprocessor 0 register specified by the combination of rd and sel are loaded into general register
  rt. Note that not all coprocessor 0 registers support the sel field. In those instances, the sel field must be zero.

  Restrictions:

  The results are UNDEFINED if coprocessor 0 does not contain a register as specified by rd and sel.

  Operation:

       data <- CPR[0,rd,sel]
       GPR[rt] <- data

  Exceptions:

  Coprocessor Unusable

  Reserved Instruction');
INSERT INTO "instructions" VALUES('MIPS32','MFC1','


Move Word From Floating Point                                                                               MFC1


   31             26 25             21 20             16 15          11 10                                    0

        COP1                MF                                                              0
                                               rt                fs
       010001              00000                                                      000 0000 0000

          6                   5                5                 5                          11


     Format: MFC1 rt, fs                                                                     MIPS32 (MIPS I)

     Purpose:

     To copy a word from an FPU (CP1) general register to a GPR

     Description: rt <- fs

     The contents of FPR fs are loaded into general register rt.

     Restrictions:

     Operation:
         data <- ValueFPR(fs, UNINTERPRETED_WORD)
         GPR[rt] <- data


     Exceptions:

     Coprocessor Unusable, Reserved Instruction

     Historical Information:

     For MIPS I, MIPS II, and MIPS III the contents of GPR rt are undefined for the instruction immediately following
     MFC1.');
INSERT INTO "instructions" VALUES('MIPS32','MFC2','


Move Word From Coprocessor 2                                                                                    MFC2


   31              26 25              21 20             16 15              11 10                         3 2       0

         COP2                 MF                                                              0
                                                 rt                  rd                                       sel
        010010               00000                                                        000 0000 0

           6                   5                 5                   5                        8               3


     Format: MFC2 rt, rd                                                                                    MIPS32
                MFC2, rt, rd, sel                                                                           MIPS32

     Purpose:

     To copy a word from a COP2 general register to a GPR

     Description: rt <- rd

     The contents of GPR rt are and placed into the coprocessor 2 register specified by the rd and sel fields. Note that not
     all coprocessor 2 registers may support the sel field. In those instances, the sel field must be zero.

     Restrictions:

     The results are UNPREDICTABLE is coprocessor 2 does not contain a register as specified by rd and sel.

     Operation:

          data <- CPR[2,rd,sel]
          GPR[rt] <- data

     Exceptions:

     Coprocessor Unusable');
INSERT INTO "instructions" VALUES('MIPS32','MFHI','


Move From HI Register                                                                                            MFHI


   31                26 25                                 16 15             11 10           6   5                 0

       SPECIAL                             0                                           0               MFHI
                                                                     rd
        000000                       00 0000 0000                                    00000            010000

           6                               10                        5                 5                 6


     Format: MFHI rd                                                                               MIPS32 (MIPS I)

     Purpose:

     To copy the special purpose HI register to a GPR

     Description: rd <- HI

     The contents of special register HI are loaded into GPR rd.

     Restrictions:

     None

     Operation:

          GPR[rd] <- HI

     Exceptions:

     None

     Historical Information:

     In the MIPS I, II, and III architectures, the two instructions which follow the MFHI must not moodify the HI register.
     If this restriction is violated, the result of the MFHI is UNPREDICTABLE. This restriction was removed in MIPS
     IV and MIPS32, and all subsequent levels of the architecture.');
INSERT INTO "instructions" VALUES('MIPS32','MFLO','


Move From LO Register                                                                                           MFLO


   31                26 25                                 16 15             11 10           6   5                 0

       SPECIAL                             0                                           0              MFLO
                                                                     rd
        000000                       00 0000 0000                                    00000            010010

           6                               10                        5                 5                 6


     Format: MFLO           rd                                                                     MIPS32 (MIPS I)

     Purpose:

     To copy the special purpose LO register to a GPR

     Description: rd <- LO

     The contents of special register LO are loaded into GPR rd.

     Restrictions: None

     Operation:
          GPR[rd] <- LO

     Exceptions:

     None

     Historical Information:

     In the MIPS I, II, and III architectures, the two instructions which follow the MFHI must not moodify the HI register.
     If this restriction is violated, the result of the MFHI is UNPREDICTABLE. This restriction was removed in MIPS
     IV and MIPS32, and all subsequent levels of the architecture.');
INSERT INTO "instructions" VALUES('MIPS32','MOV.fmt','


Floating Point Move                                                                                           MOV.fmt


      31             26 25               21 20              16 15               11 10          6    5               0

           COP1                                     0                                                    MOV
                                 fmt                                    fs              fd
          010001                                  00000                                                 000110

             6                    5                 5                    5               5                 6


        Format: MOV.S fd, fs MIPS32 (MIPS I)
        MOV.D fd, fsMIPS32 (MIPS I)
        Purpose:

        To move an FP value between FPRs

        Description: fd <- fs

        The value in FPR fs is placed into FPR fd. The source and destination are values in format fmt.

        The move is non-arithmetic; it causes no IEEE 754 exceptions.

        Restrictions:

        The fields fs and fd must specify FPRs valid for operands of type fmt; if they are not valid, the result is UNPRE-
        DICTABLE.

        The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value of the operand
        FPR becomes UNPREDICTABLE.



        Operation:

           StoreFPR(fd, fmt, ValueFPR(fs, fmt))

        Exceptions:

        Coprocessor Unusable, Reserved Instruction

        Floating Point Exceptions:

        Unimplemented Operation');
INSERT INTO "instructions" VALUES('MIPS32','MOVF','


Move Conditional on Floating Point False                                                                       MOVF


   31              26 25              21 20     18 17 16 15               11 10             6   5                 0

       SPECIAL                                       0 tf                             0               MOVCI
                               rs            cc                    rd
        000000                                       0 0                           00000              000001

           6                   5              3      1 1           5                  5                  6


     Format: MOVF rd, rs, cc                                                                   MIPS32 (MIPS IV)

     Purpose:

     To test an FP condition code then conditionally move a GPR

     Description: if cc = 0 then rd <- rs

     If the floating point condition code specified by CC is zero, then the contents of GPR rs are placed into GPR rd.

     Restrictions:

     Operation:

          if FPConditionCode(cc) = 0 then
              GPR[rd] <- GPR[rs]
          endif



     Exceptions:

     Reserved Instruction, Coprocessor Unusable');
INSERT INTO "instructions" VALUES('MIPS32','MOVF.fmt','


Floating Point Move Conditional on Floating Point False                                                            MOVF.fmt


       31              26 25                21 20      18 17 16 15              11 10               6   5                   0

             COP1                                          0   tf                                              MOVCF
                                   fmt             cc                    fs                  fd
            010001                                         0   0                                               010001

               6                    5              3       1   1          5                  5                    6


         Format: MOVF.S fd, fs, cc MIPS32 (MIPS IV)
         MOVF.D fd, fs, cc MIPS32 (MIPS IV)
         Purpose:

         To test an FP condition code then conditionally move an FP value

         Description: if cc = 0 then fd <- fs

         If the floating point condition code specified by CC is zero, then the value in FPR fs is placed into FPR fd. The source
         and destination are values in format fmt.

         If the condition code is not zero, then FPR fs is not copied and FPR fd retains its previous value in format fmt. If fd did
         not contain a value either in format fmt or previously unused data from a load or move-to operation that could be
         interpreted in format fmt, then the value of fd becomes UNPREDICTABLE.

         The move is non-arithmetic; it causes no IEEE 754 exceptions.

         Restrictions:

         The fields fs and fd must specify FPRs valid for operands of type fmt; if they are not valid, the result is UNPRE-
         DICTABLE. The operand must be a value in format fmt; if it is not, the result is UNPREDITABLE and the value of
         the operand FPR becomes UNPREDICTABLE.
     Operation:


        if FPConditionCode(cc) = 0 then
             StoreFPR(fd, fmt, ValueFPR(fs, fmt))
        else
             StoreFPR(fd, fmt, ValueFPR(fd, fmt))



     Exceptions:

     Coprocessor Unusable, Reserved Instruction

     Floating Point Exceptions:

     Unimplemented Operation');
INSERT INTO "instructions" VALUES('MIPS32','MOVN','


Move Conditional on Not Zero                                                                                  MOVN


   31               26 25             21 20               16 15            11 10               6  5              0

       SPECIAL                                                                         0              MOVN
                               rs                  rt                rd
        000000                                                                       00000            001011

           6                   5                   5                  5                5                6


     Format: MOVN rd, rs, rt                                                                     MIPS32 (MIPS IV)

     Purpose:

     To conditionally move a GPR after testing a GPR value

     Description: if rt != 0 then rd <- rs

     If the value in GPR rt is not equal to zero, then the contents of GPR rs are placed into GPR rd.

     Restrictions:

     None

     Operation:

          if GPR[rt] != 0 then
               GPR[rd] <- GPR[rs]
          endif

     Exceptions:

     None

     Programming Notes:

     The non-zero value tested here is the condition true result from the SLT, SLTI, SLTU, and SLTIU comparison instruc-
     tions.');
INSERT INTO "instructions" VALUES('MIPS32','MOVN.fmt','


Floating Point Move Conditional on Not Zero                                                                        MOVN.fmt


       31               26 25              21 20               16 15              11 10              6  5                  0

             COP1                                                                                              MOVN
                                   fmt                  rt                fs                 fd
            010001                                                                                             010011

               6                    5                   5                 5                  5                   6


         Format: MOVN.S fd, fs, rt                                                                      MIPS32 (MIPS IV)
                    MOVN.D fd, fs, rt                                                                   MIPS32 (MIPS IV)

         Purpose:

         To test a GPR then conditionally move an FP value

         Description: if rt != 0 then fd <- fs

         If the value in GPR rt is not equal to zero, then the value in FPR fs is placed in FPR fd. The source and destination are
         values in format fmt.

         If GPR rt contains zero, then FPR fs is not copied and FPR fd contains its previous value in format fmt. If fd did not
         contain a value either in format fmt or previously unused data from a load or move-to operation that could be inter-
         preted in format fmt, then the value of fd becomes UNPREDICTABLE.

         The move is non-arithmetic; it causes no IEEE 754 exceptions.

         Restrictions:

         The fields fs and fd must specify FPRs valid for operands of type fmt; if they are not valid, the result is UNPRE-
         DICTABLE.

         The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value of the operand
         FPR becomes UNPREDICTABLE.');
INSERT INTO "instructions" VALUES('MIPS32','Floating','

Operation:

   if GPR[rt] != 0 then
        StoreFPR(fd, fmt, ValueFPR(fs, fmt))
   else
        StoreFPR(fd, fmt, ValueFPR(fd, fmt))
   endif



Exceptions:

Coprocessor Unusable, Reserved Instruction

Floating Point Exceptions:

Unimplemented Operation');
INSERT INTO "instructions" VALUES('MIPS32','MOVT','


Move Conditional on Floating Point True                                                                       MOVT


   31              26 25              21 20     18 17 16 15               11 10             6   5                0

       SPECIAL                                       0 tf                            0                MOVCI
                               rs            cc                   rd
        000000                                       0 1                           00000              000001

           6                   5              3      1 1           5                 5                    6


     Format:     MOVT rd, rs, cc                                                               MIPS32 (MIPS IV)

     Purpose:

     To test an FP condition code then conditionally move a GPR

     Description: if cc = 1 then rd <- rs

     If the floating point condition code specified by CC is one, then the contents of GPR rs are placed into GPR rd.

     Restrictions:

     Operation:

          if FPConditionCode(cc) = 1 then
              GPR[rd] <- GPR[rs]
          endif



     Exceptions:

     Reserved Instruction, Coprocessor Unusable');
INSERT INTO "instructions" VALUES('MIPS32','MOVT.fmt','


Floating Point Move Conditional on Floating Point True                                                           MOVT.fmt


       31              26 25               21 20       18 17 16 15            11 10               6  5                 0

             COP1                                         0  tf                                            MOVCF
                                  fmt             cc                   fs                 fd
            010001                                        0  1                                              010001

               6                    5              3      1  1          5                 5                    6


         Format: MOVT.S fd, fs, cc                                                                  MIPS32 (MIPS IV)
                     MOVT.D fd, fs, cc                                                              MIPS32 (MIPS IV)

         Purpose:

         To test an FP condition code then conditionally move an FP value

         Description: if cc = 1 then fd <- fs

         If the floating point condition code specified by CC is one, then the value in FPR fs is placed into FPR fd. The source
         and destination are values in format fmt.

         If the condition code is not one, then FPR fs is not copied and FPR fd contains its previous value in format fmt. If fd
         did not contain a value either in format fmt or previously unused data from a load or move-to operation that could be
         interpreted in format fmt, then the value of fd becomes undefined.

         The move is non-arithmetic; it causes no IEEE 754 exceptions.

         Restrictions:

         The fields fs and fd must specify FPRs valid for operands of type fmt; if they are not valid, the result is UNPRE-
         DICTABLE. The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value
         of the operand FPR becomes UNPREDICTABLE.');
INSERT INTO "instructions" VALUES('MIPS32','MOVZ','


Move Conditional on Zero                                                                                    MOVZ


   31               26 25              21 20             16 15              11 10             6  5             0

       SPECIAL                                                                          0           MOVZ
                               rs                  rt               rd
        000000                                                                       00000          001010

           6                    5                  5                 5                  5              6


     Format: MOVZ rd, rs, rt                                                                     MIPS32 (MIPS IV

     Purpose:

     To conditionally move a GPR after testing a GPR value

     Description: if rt = 0 then rd <- rs

     If the value in GPR rt is equal to zero, then the contents of GPR rs are placed into GPR rd.

     Restrictions:

     None

     Operation:

          if GPR[rt] = 0 then
               GPR[rd] <- GPR[rs]
          endif

     Exceptions:

     None

     Programming Notes:

     The zero value tested here is the condition false result from the SLT, SLTI, SLTU, and SLTIU comparison instruc-
     tions.');
INSERT INTO "instructions" VALUES('MIPS32','MOVZ.fmt','


Floating Point Move Conditional on Zero                                                                            MOVZ.fmt


       31               26 25               21 20             16 15              11 10              6  5                   0

             COP1                                                                                             MOVZ
                                   fmt                 rt                 fs                fd
            010001                                                                                            010010

               6                    5                  5                  5                 5                    6


         Format: MOVZ.S fd, fs, rt                                                                     MIPS32 (MIPS IV)
                    MOVZ.D fd, fs, rt                                                                  MIPS32 (MIPS IV)

         Purpose:

         To test a GPR then conditionally move an FP value

         Description: if rt = 0 then fd <- fs

         If the value in GPR rt is equal to zero then the value in FPR fs is placed in FPR fd. The source and destination are val-
         ues in format fmt.

         If GPR rt is not zero, then FPR fs is not copied and FPR fd contains its previous value in format fmt. If fd did not con-
         tain a value either in format fmt or previously unused data from a load or move-to operation that could be interpreted
         in format fmt, then the value of fd becomes UNPREDICTABLE.

         The move is non-arithmetic; it causes no IEEE 754 exceptions.

         Restrictions:

         The fields fs and fd must specify FPRs valid for operands of type fmt; if they are not valid, the result is UNPRE-
         DICTABLE.

         The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value of the operand
         FPR becomes UNPREDICTABLE.
     Operation:

        if GPR[rt] = 0 then
             StoreFPR(fd, fmt, ValueFPR(fs, fmt))
        else
             StoreFPR(fd, fmt, ValueFPR(fd, fmt))
        endif

     Exceptions:

     Coprocessor Unusable, Reserved Instruction

     Floating Point Exceptions:

     Unimplemented Operation');
INSERT INTO "instructions" VALUES('MIPS32','MSUB','



 Multiply and Subtract Word to Hi,Lo                                                                        MSUB


31              26 25               21 20             16 15               11 10            6   5                0

   SPECIAL2                                                       0                  0                MSUB
                             rs                rt
     011100                                                    00000               00000             000100

        6                    5                 5                  5                  5                 6


  Format:     MSUB rs, rt                                                                                  MIPS32

  Purpose:

  To multiply two words and subtract the result from Hi, Lo

  Description: (LO,HI) <- (rs x rt) - (LO,HI)

  The 32-bit word value in GPR rs is multiplied by the 32-bit value in GPR rt, treating both operands as signed values,
  to produce a 64-bit result. The product is subtracted from the 64-bit concatenated values of HI and LO.. The most sig-
  nificant 32 bits of the result are written into HI and the least signficant 32 bits are written into LO. No arithmetic
  exception occurs under any circumstances.

  Restrictions:

  None

  This instruction does not provide the capability of writing directly to a target GPR.

  Operation:

      temp <- (HI || LO) - (GPR[rs] * GPR[rt])
      HI <- temp63..32
      LO <- temp31..0

  Exceptions:

  None

  Programming Notes:

  Where the size of the operands are known, software should place the shorter operand in GPR rt. This may reduce the
  latency of the instruction on those processors which implement data-dependent instruction latencies.');
INSERT INTO "instructions" VALUES('MIPS32','MSUBU','



Multiply and Subtract Word to Hi,Lo                                                                          MSUBU


31              26 25               21 20             16 15               11 10             6   5                 0

   SPECIAL2                                                       0                  0                MSUBU
                             rs                rt
     011100                                                    00000               00000               000101

       6                     5                 5                  5                  5                    6


  Format: MSUBU rs, rt                                                                                       MIPS32

  Purpose:

  To multiply two words and subtract the result from Hi, Lo

  Description: (LO,HI) <- (rs x rt) - (LO,HI)

  The 32-bit word value in GPR rs is multiplied by the 32-bit word value in GPR rt, treating both operands as unsigned
  values, to produce a 64-bit result. The product is subtracted from the 64-bit concatenated values of HI and LO.. The
  most significant 32 bits of the result are written into HI and the least signficant 32 bits are written into LO. No arith-
  metic exception occurs under any circumstances.

  Restrictions:

  None

  This instruction does not provide the capability of writing directly to a target GPR.

  Operation:

      temp <- (HI || LO) - (GPR[rs] * GPR[rt])
      HI <- temp63..32
      LO <- temp31..0

  Exceptions:

  None

  Programming Notes:

  Where the size of the operands are known, software should place the shorter operand in GPR rt. This may reduce the
  latency of the instruction on those processors which implement data-dependent instruction latencies.');
INSERT INTO "instructions" VALUES('MIPS32','MTC0','



 Move to Coprocessor 0                                                                                          MTC0



31               26 25              21 20              16 15               11 10                         3    2       0

      COP0                 MT                                                                0
                                                rt                 rd                                            sel
     010000               00100                                                          0000 000

       6                    5                   5                   5                        8                    3


  Format:      MTC0 rt, rd                                                                                     MIPS32

  Purpose:

  To move the contents of a general register to a coprocessor 0 register.

  Description: CPR[r0, rd, sel] <- rt

  The contents of general register rt are loaded into the coprocessor 0 register specified by the combination of rd and
  sel. Not all coprocessor 0 registers support the the sel field. In those instances, the sel field must be set to zero.

  Restrictions:

  The results are UNDEFINED if coprocessor 0 does not contain a register as specified by rd and sel.



  Operation:

       CPR[0,rd,sel] <- data

  Exceptions:

  Coprocessor Unusable

  Reserved Instruction');
INSERT INTO "instructions" VALUES('MIPS32','MTC1','


Move Word to Floating Point                                                                                   MTC1


   31             26 25             21 20             16 15             11 10                                  0

        COP1                MT                                                                0
                                                rt               fs
       010001              00100                                                        000 0000 0000

          6                  5                  5                5                           11


     Format: MTC1 rt, fs                                                                       MIPS32 (MIPS I)

     Purpose:

     To copy a word from a GPR to an FPU (CP1) general register

     Description: fs <- rt

     The low word in GPR rt is placed into the low word of floating point (Coprocessor 1) general register fs.

     Restrictions:

     Operation:

         data <- GPR[rt]31..0
         StoreFPR(fs, UNINTERPRETED_WORD, data)



     Exceptions:

     Coprocessor Unusable

     Historical Information:

     For MIPS I, MIPS II, and MIPS III the value of FPR fs is UNPREDICTABLE for the instruction immediately follow-
     ing MTC1.');
INSERT INTO "instructions" VALUES('MIPS32','MTC2','


Move Word to Coprocessor 2                                                                                         MTC2


   31              26 25              21 20             16 15              11 10                                      0

        COP2                  MT                                                              0
                                                  rt               rd                                             sel
       010010                00100                                                       000 0000 0

          6                    5                  5                 5                         8                   3


     Format: MTC2 rt, rd                                                                                        MIPS32
                MTC2 rt, rd, sel                                                                                MIPS32

     Purpose:

     To copy a word from a GPR to a COP2 general register

     Description: rd <- rt

     The low word in GPR rt is placed into the low word of coprocessor 2 general register specified by the rd and sel
     fields. Note that not all coprocessor 2 registers may support the sel field. In those instances, the sel field must be zero.

     Restrictions:

     The results are UNPREDICTABLE is coprocessor 2 does not contain a register as specified by rd and sel.

     Operation:

         data <- GPR[rt]31..0
         CPR[2,rd,sel] <- data

     Exceptions:

     Coprocessor Unusable');
INSERT INTO "instructions" VALUES('MIPS32','MTHI','


Move to HI Register                                                                                              MTHI


   31                26 25             21 20                                                 6  5                  0

        SPECIAL                                                     0                                  MTHI
                                rs
         000000                                             000 0000 0000 0000                        010001

           6                    5                                   15                                   6


      Format: MTHI rs                                                                            MIPS32 (MIPS I)

      Purpose:

      To copy a GPR to the special purpose HI register

      Description: HI <- rs

      The contents of GPR rs are loaded into special register HI.

      Restrictions:

      A computed result written to the HI/LO pair by DIV, DIVU,MULT, or MULTU must be read by MFHI or MFLO
      before a new result can be written into either HI or LO.

      If an MTHI instruction is executed following one of these arithmetic instructions, but before an MFLO or MFHI
      instruction, the contents of LO are UNPREDICTABLE. The following example shows this illegal situation:
           MUL      r2,r4 # start operation that will eventually write to HI,LO
           ...               # code not containing mfhi or mflo
           MTHI     r6
           ...               # code not containing mflo
           MFLO     r3       # this mflo would get an UNPREDICTABLE value

      Operation:

           HI <- GPR[rs]

      Exceptions:

      None

      Historical Information:

      In MIPS I-III, if either of the two preceding instructions is MFHI, the result of that MFHI is UNPREDICTABLE.
      Reads of the HI or LO special register must be separated from any subsequent instructions that write to them by two
      or more instructions. In MIPS IV and later, including MIPS32 and MIPS64, this restriction does not exist.');
INSERT INTO "instructions" VALUES('MIPS32','MTLO','


Move to LO Register                                                                                              MTLO


   31                26 25              21 20                                                6  5                  0

        SPECIAL                                                     0                                 MTLO
                                rs
         000000                                             000 0000 0000 0000                        010011

           6                    5                                   15                                   6


      Format: MTLO rs                                                                            MIPS32 (MIPS I)

      Purpose:

      To copy a GPR to the special purpose LO register

      Description: LO <- rs

      The contents of GPR rs are loaded into special register LO.

      Restrictions:

      A computed result written to the HI/LO pair by DIV, DIVU, MULT, or MULTU must be read by MFHI or MFLO
      before a new result can be written into either HI or LO.

      If an MTLO instruction is executed following one of these arithmetic instructions, but before an MFLO or MFHI
      instruction, the contents of HI are UNPREDICTABLE. The following example shows this illegal situation:
           MUL      r2,r4 # start operation that will eventually write to HI,LO
           ...               # code not containing mfhi or mflo
           MTLO     r6
           ...               # code not containing mfhi
           MFHI     r3       # this mfhi would get an UNPREDICTABLE value

      Operation:

           LO <- GPR[rs]

      Exceptions:

      None

      Historical Information:

      In MIPS I-III, if either of the two preceding instructions is MFHI, the result of that MFHI is UNPREDICTABLE.
      Reads of the HI or LO special register must be separated from any subsequent instructions that write to them by two
      or more instructions. In MIPS IV and later, including MIPS32 and MIPS64, this restriction does not exist.');
INSERT INTO "instructions" VALUES('MIPS32','MUL','



 Multiply Word to GPR                                                                                          MUL


31              26 25               21 20            16 15               11 10                6  5                  0

   SPECIAL2                                                                            0                MUL
                             rs                rt                 rd
     011100                                                                         00000              000010

        6                    5                 5                  5                    5                  6


  Format:     MUL rd, rs, rt                                                                                 MIPS32

  Purpose:

  To multiply two words and write the result to a GPR.

  Description: rd <- rs & rt

  The 32-bit word value in GPR rs is multiplied by the 32-bit value in GPR rt, treating both operands as signed values,
  to produce a 64-bit result. The least significant 32 bits of the product are written to GPR rd. The contents of HI and
  LO are UNPREDICTABLE after the operation. No arithmetic exception occurs under any circumstances.

  Restrictions:

  Note that this instruction does not provide the capability of writing the result to the HI and LO registers.

  Operation:

      temp <- GPR[rs] * GPR[rt]
      GPR[rd] <- temp
                           31..0
      HI <- UNPREDICTABLE
      LO <- UNPREDICTABLE

  Exceptions:

  None

  Programming Notes:

  In some processors the integer multiply operation may proceed asynchronously and allow other CPU instructions to
  execute before it is complete. An attempt to read LO or HI before the results are written interlocks until the results are
  ready. Asynchronous execution does not affect the program result, but offers an opportunity for performance
  improvement by scheduling the multiply so that other instructions can execute in parallel.

  Programs that require overflow detection must check for it explicitly.

  Where the size of the operands are known, software should place the shorter operand in GPR rt. This may reduce the
  latency of the instruction on those processors which implement data-dependent instruction latencies.');
INSERT INTO "instructions" VALUES('MIPS32','MUL.fmt','


Floating Point Multiply                                                                                             MUL.fmt


      31              26 25              21 20             16 15               11 10               6   5                 0

           COP1                                                                                               MUL
                                 fmt                ft                 fs                  fd
          010001                                                                                             000010

             6                    5                 5                  5                    5                    6


        Format: MUL.S fd, fs, ft                                                                        MIPS32 (MIPS I)
                    MUL.D fd, fs, ft                                                                    MIPS32 (MIPS I)

        Purpose:

        To multiply FP values

        Description: fd <- fs & ft

        The value in FPR fs is multiplied by the value in FPR ft. The result is calculated to infinite precision, rounded accord-
        ing to the current rounding mode in FCSR, and placed into FPR fd. The operands and result are values in format fmt.

        Restrictions:

        The fields fs, ft, and fd must specify FPRs valid for operands of type fmt; if they are not valid, the result is UNPRE-
        DICTABLE.

        The operands must be values in format fmt; if they are not, the result is UNPREDICTABLE and the value of the
        operand FPRs becomes UNPREDICTABLE.

        Operation:

            StoreFPR (fd, fmt, ValueFPR(fs, fmt) &fmt ValueFPR(ft, fmt))

        Exceptions:

        Coprocessor Unusable, Reserved Instruction

        Floating Point Exceptions:

        Inexact, Unimplemented Operation, Invalid Operation, Overflow, Underflow');
INSERT INTO "instructions" VALUES('MIPS32','MULT','


Multiply Word                                                                                                         MULT


   31               26 25               21 20             16 15                                 6   5                   0

       SPECIAL                                                                0                            MULT
                                 rs                rt
         000000                                                         00 0000 0000                       011000

           6                     5                 5                         10                              6


      Format: MULT rs, rt                                                                             MIPS32 (MIPS I)

      Purpose:

      To multiply 32-bit signed integers

      Description: (LO, HI) <- rs & rt

      The 32-bit word value in GPR rt is multiplied by the 32-bit value in GPR rs, treating both operands as signed values,
      to produce a 64-bit result. The low-order 32-bit word of the result is placed into special register LO, and the
      high-order 32-bit word is splaced into special register HI.

      No arithmetic exception occurs under any circumstances.

      Restrictions:

      None

      Operation:

              prod       <- GPR[rs]31..0 & GPR[rt]31..0
              LO         <- prod31..0
              HI         <- prod63..32

      Exceptions:

      None

      Programming Notes:

      In some processors the integer multiply operation may proceed asynchronously and allow other CPU instructions to
      execute before it is complete. An attempt to read LO or HI before the results are written interlocks until the results are
      ready. Asynchronous execution does not affect the program result, but offers an opportunity for performance
      improvement by scheduling the multiply so that other instructions can execute in parallel.

      Programs that require overflow detection must check for it explicitly.

      Where the size of the operands are known, software should place the shorter operand in GPR rt. This may reduce the
      latency of the instruction on those processors which implement data-dependent instruction latencies.');
INSERT INTO "instructions" VALUES('MIPS32','MULTU','


Multiply Unsigned Word                                                                                              MULTU


    31              26 25               21 20             16 15                                 6   5                   0

       SPECIAL                                                                0                           MULTU
                                 rs                rt
         000000                                                         00 0000 0000                       011001

            6                    5                 5                         10                              6


      Format: MULTU rs, rt                                                                            MIPS32 (MIPS I)

      Purpose:

      To multiply 32-bit unsigned integers

      Description: (LO, HI) <- rs & rt

      The 32-bit word value in GPR rt is multiplied by the 32-bit value in GPR rs, treating both operands as unsigned val-
      ues, to produce a 64-bit result. The low-order 32-bit word of the result is placed into special register LO, and the
      high-order 32-bit word is placed into special register HI.

      No arithmetic exception occurs under any circumstances.

      Restrictions:

      None

      Operation:

               prod<- (0 || GPR[rs]31..0) & (0 || GPR[rt]31..0)
               LO <- prod31..0
               HI <- prod63..32

      Exceptions:

      None

      Programming Notes:

      In some processors the integer multiply operation may proceed asynchronously and allow other CPU instructions to
      execute before it is complete. An attempt to read LO or HI before the results are written interlocks until the results are
      ready. Asynchronous execution does not affect the program result, but offers an opportunity for performance
      improvement by scheduling the multiply so that other instructions can execute in parallel.

      Programs that require overflow detection must check for it explicitly.

      Where the size of the operands are known, software should place the shorter operand in GPR rt. This may reduce the
      latency of the instruction on those processors which implement data-dependent instruction latencies.');
INSERT INTO "instructions" VALUES('MIPS32','NEG.fmt','


Floating Point Negate                                                                                            NEG.fmt


      31             26 25               21 20             16 15               11 10            6    5                 0

            COP1                                     0                                                       NEG
                                 fmt                                    fs               fd
           010001                                 00000                                                    000111

             6                    5                  5                  5                5                    6


        Format: NEG.S fd, fs                                                                          MIPS32 (MIPS I)
                   NEG.D fd, fs                                                                       MIPS32 (MIPS I)

        Purpose:

        To negate an FP value

        Description: fd <- ''fs

        The value in FPR fs is negated and placed into FPR fd. The value is negated by changing the sign bit value. The oper-
        and and result are values in format fmt. This operation is arithmetic; a NaN operand signals invalid operation.

        Restrictions:

        The fields fs and fd must specify FPRs valid for operands of type fmt; if they are not valid, the result is UNPRE-
        DICTABLE. The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value
        of the operand FPR becomes UNPREDICTABLE.

        Operation:

            StoreFPR(fd, fmt, Negate(ValueFPR(fs, fmt)))



        Exceptions:

        Coprocessor Unusable, Reserved Instruction

        Floating Point Exceptions:

        Unimplemented Operation, Invalid Operation');
INSERT INTO "instructions" VALUES('MIPS32','NOP','


No Operation                                                                                                         NOP


   31              26 25             21 20            16 15                11 10              6  5                  0

      SPECIAL                 0                 0                    0                0                  SLL

        000000              00000             00000               00000            00000               000000

           6                  5                 5                    5                5                    6


     Format: NOP                                                                                   Assembly Idiom

     Purpose:

     To perform no operation.

     Description:

     NOP is the assembly idiom used to denote no operation. The actual instruction is interpreted by the hardware as SLL
     r0, r0, 0.

     Restrictions:

     None

     Operation:

     None

     Exceptions:

     None

     Programming Notes:

     The zero instruction word, which represents SLL, r0, r0, 0, is the preferred NOP for software to use to fill branch and
     jump delay slots and to pad out alignment sequences.');
INSERT INTO "instructions" VALUES('MIPS32','NOR','


Not Or                                                                                                        NOR


  31               26 25            21 20            16 15             11 10               6 5                 0

       SPECIAL                                                                      0               NOR
                               rs              rt               rd
        000000                                                                   00000             100111

           6                   5               5                5                   5                 6


      Format: NOR rd, rs, rt                                                                  MIPS32 (MIPS I)

      Purpose:

      To do a bitwise logical NOT OR

      Description: rd <- rs NOR rt

      The contents of GPR rs are combined with the contents of GPR rt in a bitwise logical NOR operation. The result is
      placed into GPR rd.

      Restrictions:

      None

      Operation:

          GPR[rd] <- GPR[rs] nor GPR[rt]

      Exceptions:

      None');
INSERT INTO "instructions" VALUES('MIPS32','OR','


Or                                                                                                           OR


  31             26 25           21 20            16 15             11 10              6  5                 0

     SPECIAL                                                                    0                 OR
                             rs             rt               rd
      000000                                                                  00000             100101

         6                   5              5                5                  5                  6


    Format: OR rd, rs, rt                                                                  MIPS32 (MIPS I)

    Purpose:

    To do a bitwise logical OR

    Description: rd <- rs or rt

    The contents of GPR rs are combined with the contents of GPR rt in a bitwise logical OR operation. The result is
    placed into GPR rd.

    Restrictions:

    None

    Operation:

        GPR[rd] <- GPR[rs] or GPR[rt]

    Exceptions:

    None');
INSERT INTO "instructions" VALUES('MIPS32','ORI','


Or Immediate                                                                                                    ORI


   31             26 25                21 20           16 15                                                     0

        ORI
                               rs                rt                              immediate
       001101

          6                     5                5                                   16


     Format: ORI rt, rs, immediate                                                             MIPS32 (MIPS I)

     Purpose:

     To do a bitwise logical OR with a constant

     Description: rt <- rs or immediate

     The 16-bit immediate is zero-extended to the left and combined with the contents of GPR rs in a bitwise logical OR
     operation. The result is placed into GPR rt.

     Restrictions:

     None

     Operation:

         GPR[rt] <- GPR[rs] or zero_extend(immediate)

     Exceptions:

     None');
INSERT INTO "instructions" VALUES('MIPS32','PREF','


Prefetch                                                                                                               PREF


   31                26 25                21 20             16 15                                                       0

           PREF
                                 base               hint                                    offset
          110011

             6                    5                   5                                      16


       Format: PREF hint,offset(base)                                                               MIPS32 (MIPS IV)

       Purpose:

       To move data between memory and cache.

       Description: prefetch_memory(base+offset)

       PREF adds the 16-bit signed offset to the contents of GPR base to form an effective byte address. The hint field sup-
       plies information about the way that the data is expected to be used.

       PREF enables the processor to take some action, typically prefetching the data into cache, to improve program perfor-
       mance. The action taken for a specific PREF instruction is both system and context dependent. Any action, including
       doing nothing, is permitted as long as it does not change architecturally visible state or alter the meaning of a pro-
       gram. Implementations are expected either to do nothing, or to take an action that increases the performance of the
       program.

       PREF does not cause addressing-related exceptions. If the address specified would cause an addressing exception, the
       exception condition is ignored and no data movement occurs.However even if no data is prefetched, some action that
       is not architecturally visible, such as writeback of a dirty cache line, can take place.

       PREF never generates a memory operation for a location with an uncached memory access type.

       If PREF results in a memory operation, the memory access type used for the operation is determined by the memory
       access type of the effective address, just as it would be if the memory operation had been caused by a load or store to
       the effective address.

       For a cached location, the expected and useful action for the processor is to prefetch a block of data that includes the
       effective address. The size of the block and the level of the memory hierarchy it is fetched into are implementation
       specific.

       The hint field supplies information about the way the data is expected to be used. A hint value cannot cause an action
       to modify architecturally visible state. A processor may use a hint value to improve the effectiveness of the prefetch
       action.
                          Table 3-29 Values of the hint Field for the PREF Instruction

           Value             Name                       Data Use and Desired Prefetch Action



                                            Use: Prefetched data is expected to be read (not modified).
              0     load
                                            Action: Fetch data as if for a load.




                                            Use: Prefetched data is expected to be stored or modified.
              1     store
                                            Action: Fetch data as if for a store.




             2-3    Reserved                Reserved for future use - not available to implementations.




                                            Use: Prefetched data is expected to be read (not modified) but not
                                            reused extensively; it !=streams!= through cache.
              4     load_streamed
                                            Action: Fetch data as if for a load and place it in the cache so that it
                                            does not displace data prefetched as !=retained.!=




                                            Use: Prefetched data is expected to be stored or modified but not
                                            reused extensively; it !=streams!= through cache.
              5     store_streamed
                                            Action: Fetch data as if for a store and place it in the cache so that
                                            it does not displace data prefetched as !=retained.!=




                                            Use: Prefetched data is expected to be read (not modified) and
                                            reused extensively; it should be !=retained!= in the cache.
              6     load_retained
                                            Action: Fetch data as if for a load and place it in the cache so that it
                                            is not displaced by data prefetched as !=streamed.!=




                                            Use: Prefetched data is expected to be stored or modified and reused
                                            extensively; it should be !=retained!= in the cache.
              7     store_retained
                                            Action: Fetch data as if for a store and place it in the cache so that
                                            it is not displaced by data prefetched as !=streamed.!=
                 Table 3-29 Values of the hint Field for the PREF Instruction



     8-24  Reserved                Reserved for future use - not available to implementations.




                                   Use: Data is no longer expected to be used.
           writeback_invalidate
      25                           Action: For a writeback cache, schedule a wirteback of any dirty
           (also known as !=nudge!=)
                                   data. At the completion of the writeback, mark the state of any
                                   cache lines written back as invalid.




           Implementation          Unassigned by the Architecture - available for
     26-29
           Dependent               implementation-dependent use.




                                   Use: Prepare the cache for writing an entire line, without the
                                   overhead involved in filling the line from memory.

                                   Action: If the reference hits in the cache, no action is taken. If the
      30   PrepareForStore
                                   reference misses in the cache, a line is selected for replacement, any
                                   valid and dirty victim is written back to memory, the entire line is
                                   filled with zero data, and the state of the line is marked as valid and
                                   dirty.




           Implementation          Unassigned by the Architecture - available for
      31
           Dependent               implementation-dependent use.
     Restrictions:

     None

     Operation:

          vAddr <- GPR[base] + sign_extend(offset)
          (pAddr, CCA) <- AddressTranslation(vAddr, DATA, LOAD)
          Prefetch(CCA, pAddr, vAddr, DATA, hint)

     Exceptions:

     Prefetch does not take any TLB-related or address-related exceptions under any circumstances.

     Programming Notes:

     Prefetch cannot prefetch data from a mapped location unless the translation for that location is present in the TLB.
     Locations in memory pages that have not been accessed recently may not have translations in the TLB, so prefetch
     may not be effective for such locations.

     Prefetch does not cause addressing exceptions. It does not cause an exception to prefetch using an address pointer
     value before the validity of a pointer is determined.

     Hint field encodings whose function is described as !=streamed!= or !=retained!= convey usage intent from software to
     hardware. Software should not assume that hardware will always prefetch data in an optimal way. If data is to be truly
     retained, software should use the Cache instruction to lock data into the cache.');
INSERT INTO "instructions" VALUES('MIPS32','ROUND.W.fmt','


Floating Point Round to Word Fixed Point                                                                        ROUND.W.fmt


         31              26 25              21 20              16 15               11 10             6   5                   0

              COP1                                     0                                                      ROUND.W
                                    fmt                                    fs               fd
              010001                                 00000                                                      001100

                6                    5                 5                    5                5                    6


           Format: ROUND.W.S          fd, fs                                                             MIPS32 (MIPS II)
                      ROUND.W.D       fd, fs                                                             MIPS32 (MIPS II)

           Purpose:

           To convert an FP value to 32-bit fixed point, rounding to nearest

           Description: fd <- convert_and_round(fs)

           The value in FPR fs, in format fmt, is converted to a value in 32-bit word fixed point format rounding to nearest/even
           (rounding mode 0). The result is placed in FPR fd.

           When the source value is Infinity, NaN, or rounds to an integer outside the range -2^31 to 2^31-1, the result cannot be
           represented correctly and an IEEE Invalid Operation condition exists. In this case the Invalid Operation flag is set in
           the FCSR. If the Invalid Operation Enable bit is set in the FCSR, no result is written to fd and an Invalid Operation
           exception is taken immediately. Otherwise, the default result, 2^31-1, is written to fd.

           Restrictions:

           The fields fs and fd must specify valid FPRs; fs for type fmt and fd for word fixed point; if they are not valid, the result
           is UNPREDICTABLE.

           The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value of the operand
           FPR becomes UNPREDICTABLE.

           Operation:

               StoreFPR(fd, W, ConvertFmt(ValueFPR(fs, fmt), fmt, W))');
INSERT INTO "instructions" VALUES('MIPS32','SB','


Store Byte                                                                                                         SB


   31               26 25               21 20            16 15                                                     0

          SB
                               base                 rt                                  offset
         101000

           6                     5                  5                                    16


      Format: SB rt, offset(base)                                                                MIPS32 (MIPS I)

      Purpose:

      To store a byte to memory

      Description: memory[base+offset] <- rt

      The least-significant 8-bit byte of GPR rt is stored in memory at the location specified by the effective address. The
      16-bit signed offset is added to the contents of GPR base to form the effective address.

      Restrictions:

      None

      Operation:
          vAddr          <- sign_extend(offset) + GPR[base]
          (pAddr, CCA)<- AddressTranslation (vAddr, DATA, STORE)
          pAddr          <- pAddrPSIZE-1..2 || (pAddr1..0 xor ReverseEndian )            2

          bytesel        <- vAddr1..0 xor BigEndianCPU        2

          dataword       <- GPR[rt]318*bytesel..0 || 0      8*bytesel

          StoreMemory (CCA, BYTE, dataword, pAddr, vAddr, DATA)

      Exceptions:

      TLB Refill, TLB Invalid, TLB Modified, Bus Error, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','SC','


Store Conditional Word                                                                                                 SC


   31                26 25               21 20             16 15                                                      0

            SC
                                base                 rt                                 offset
          111000

            6                     5                  5                                   16


      Format: SC rt, offset(base)                                                                  MIPS32 (MIPS II)

      Purpose:

      To store a word to memory to complete an atomic read-modify-write

      Description: if atomic_update then memory[base+offset] <- rt, rt <- 1 else rt <- 0

      The LL and SC instructions provide primitives to implement atomic read-modify-write (RMW) operations for cached
      memory locations.

      The 16-bit signed offset is added to the contents of GPR base to form an effective address.

      The SC completes the RMW sequence begun by the preceding LL instruction executed on the processor. To complete
      the RMW sequence atomically, the following occur:

     . The least-significant 32-bit word of GPR rt is stored into memory at the location specified by the aligned effective
       address.

     . A 1, indicating success, is written into GPR rt.

      Otherwise, memory is not modified and a 0, indicating failure, is written into GPR rt.

      If either of the following events occurs between the execution of LL and SC, the SC fails:

     . A coherent store is completed by another processor or coherent I/O module into the block of physical memory
       containing the word. The size and alignment of the block is implementation dependent, but it is at least one word and
       at most the minimum page size.

     . An exception occurs on the processor executing the LL/SC.

      If either of the following events occurs between the execution of LL and SC, the SC may succeed or it may fail; the
      success or failure is not predictable. Portable programs should not cause one of these events.

     . A load, store, or prefetch is executed on the processor executing the LL/SC.

     . The instructions executed starting with the LL and ending with the SC do not lie in a 2048-byte contiguous region of
       virtual memory. The region does not have to be aligned, other than the alignment required for instruction words.

      The following conditions must be true or the result of the SC is undefined:

     . Execution of SC must have been preceded by execution of an LL instruction.

     . A RMW sequence executed without intervening exceptions must use the same address in the LL and SC. The address
       is the same if the virtual address, physical address, and cache-coherence algorithm are identical.
    correctly depends on the system implementation and the memory access type used for the location:

   . MP atomicity: To provide atomic RMW among multiple processors, all accesses to the location must be made with
     a memory access type of cached coherent.

   . Uniprocessor atomicity: To provide atomic RMW on a single processor, all accesses to the location must be made
     with memory access type of either cached noncoherent or cached coherent. All accesses must be to one or the other
     access type, and they may not be mixed.

    I/O System: To provide atomic RMW with a coherent I/O system, all accesses to the location must be made with a
    memory access type of cached coherent. If the I/O system does not use coherent memory operations, then atomic
    RMW cannot be provided with respect to the I/O reads and writes.

    Restrictions:

    The addressed location must have a memory access type of cached noncoherent or cached coherent; if it does not, the
    result is undefined.

    The effective address must be naturally-aligned. If either of the 2 least-significant bits of the address is non-zero, an
    Address Error exception occurs.

    Operation:

        vAddr <- sign_extend(offset) + GPR[base]
        if vAddr            2
                   1..0  != 0 then
              SignalException(AddressError)
        endif
        (pAddr, CCA)<- AddressTranslation (vAddr, DATA, STORE)
        dataword<- GPR[rt]
        if LLbit then
              StoreMemory (CCA, WORD, dataword, pAddr, vAddr, DATA)
        endif
        GPR[rt]<- 0     31 || LLbit
    Exceptions:

    TLB Refill, TLB Invalid, TLB Modified, Address Error, Reserved Instruction

    Programming Notes:

    LL and SC are used to atomically update memory locations, as shown below.


         L1:
             LL       T1, (T0)      # load counter
             ADDI     T2, T1, 1 # increment
             SC       T2, (T0)      # try to store, checking for atomicity
             BEQ      T2, 0, L1 # if not atomic (0), try again
             NOP                    # branch-delay slot

    Exceptions between the LL and SC cause SC to fail, so persistent exceptions must be avoided. Some examples of
    these are arithmetic operations that trap, system calls, and floating point operations that trap or require software emu-
    lation assistance.

    LL and SC function on a single processor for cached noncoherent memory so that parallel programs can be run on
    uniprocessor systems that do not support cached coherent memory access types.');
INSERT INTO "instructions" VALUES('MIPS32','SDBBP','


Software Debug Breakpoint                                                                                       SDBBP


  31               26 25                                                                     6   5                 0

      SPECIAL2                                                                                         SDBBP
                                                        code
        011100                                                                                         111111

          6                                              20                                              6


     Format: SDBBP code                                                                                      EJTAG

     Purpose:

     To cause a debug breakpoint exception

     Description:

     This instruction causes a debug exception, passing control to the debug exception handler. The code field can be used
     for passing information to the debug exception handler, and is retrieved by the debug exception handler only by load-
     ing the contents of the memory word containing the instruction, using the DEPC register. The CODE field is not used
     in any way by the hardware.

     Restrictions:

     Operation:

          If Debug      = 0 then
                    DM
              SignalDebugBreakpointException()
          else
              SignalDebugModeBreakpointException()
          endif

     Exceptions:

     Debug Breakpoint Exception');
INSERT INTO "instructions" VALUES('MIPS32','SDC1','


Store Doubleword from Floating Point                                                                            SDC1


   31               26 25               21 20            16 15                                                   0

         SDC1
                               base                 ft                                  offset
        111101

           6                     5                  5                                    16


      Format: SDC1 ft, offset(base)                                                             MIPS32 (MIPS II)

      Purpose:

      To store a doubleword from an FPR to memory

      Description: memory[base+offset] <- ft

      The 64-bit doubleword in FPR ft is stored in memory at the location specified by the aligned effective address. The
      16-bit signed offset is added to the contents of GPR base to form the effective address.

      Restrictions:

      An Address Error exception occurs if EffectiveAddress  2..0!= 0 (not doubleword-aligned).

      Operation:

          vAddr <- sign_extend(offset) + GPR[base]
          if vAddr              3
                     2..0   != 0 then
               SignalException(AddressError)
          endif
          (pAddr, CCA) <- AddressTranslation(vAddr, DATA, STORE)
          datadoubleword <- ValueFPR(ft, UNINTERPRETED_DOUBLEWORD)
          StoreMemory(CCA, DOUBLEWORD, datadoubleword, pAddr, vAddr, DATA)

      Exceptions:

      Coprocessor Unusable, Reserved Instruction, TLB Refill, TLB Invalid, TLB Modified, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','SDC2','


Store Doubleword from Coprocessor 2                                                                               SDC2


   31               26 25              21 20               16 15                                                   0

         SDC2
                               base                rt                                   offset
         111110

            6                    5                 5                                     16


      Format: SDC2 rt, offset(base)                                                                            MIPS32

      Purpose:

      To store a doubleword from a Coprocessor 2 register to memory

      Description: memory[base+offset] <- rt

      The 64-bit doubleword in Coprocessor 2 register rt is stored in memory at the location specified by the aligned effec-
      tive address. The 16-bit signed offset is added to the contents of GPR base to form the effective address.

      Restrictions:

      An Address Error exception occurs if EffectiveAddress    2..0!= 0 (not doubleword-aligned).

      Operation:
           vAddr <- sign_extend(offset) + GPR[base]
           if vAddr            3
                     2..0  != 0 then
               SignalException(AddressError)
           endif
           (pAddr, CCA) <- AddressTranslation(vAddr, DATA, STORE)
           datadoubleword <- CPR[2,rt,0]
           StoreMemory(CCA, DOUBLEWORD, datadoubleword, pAddr, vAddr, DATA)

      Exceptions:

      Coprocessor Unusable, Reserved Instruction, TLB Refill, TLB Invalid, TLB Modified, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','SH','


Store Halfword                                                                                                        SH


   31               26 25              21 20               16 15                                                    0

           SH
                              base                 rt                                   offset
        101001

            6                   5                  5                                     16


      Format: SH rt, offset(base)                                                                    MIPS32 (MIPS I)

      Purpose:

      To store a halfword to memory

      Description: memory[base+offset] <- rt

      The least-significant 16-bit halfword of register rt is stored in memory at the location specified by the aligned effec-
      tive address. The 16-bit signed offset is added to the contents of GPR base to form the effective address.

      Restrictions:

      The effective address must be naturally-aligned. If the least-significant bit of the address is non-zero, an Address
      Error exception occurs.

      Operation:

           vAddr <- sign_extend(offset) + GPR[base]
           if vAddr != 0 then
                     0
               SignalException(AddressError)
           endif
           (pAddr, CCA) <- AddressTranslation (vAddr, DATA, STORE)
           pAddr <- pAddrPSIZE-1..2 || (pAddr11..0 xor (ReverseEndian || 0))
           bytesel<- vAddr11..0 xor (BigEndianCPU || 0)
           dataword<- GPR[rt]318*bytesel..0 || 0           8*bytesel

           StoreMemory (CCA, HALFWORD, dataword, pAddr, vAddr, DATA)

      Exceptions:

      TLB Refill, TLB Invalid, TLB Modified, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','SLL','


Shift Word Left Logical                                                                                               SLL


   31                26 25             21 20              16 15           11 10               6   5                  0

       SPECIAL                  0                                                                        SLL
                                                  rt                rd                sa
        000000                00000                                                                    000000

            6                   5                 5                 5                 5                    6


      Format: SLL rd, rt, sa                                                                       MIPS32 (MIPS I)

      Purpose:

      To left-shift a word by a fixed number of bits

      Description: rd <- rt << sa

      The contents of the low-order 32-bit word of GPR rt are shifted left, inserting zeros into the emptied bits; the word
      result is placed in GPR rd. The bit-shift amount is specified by sa.

      Restrictions:

      None

      Operation:
          s         <- sa
          temp      <- GPR[rt](31-s)..0 || 0      s

          GPR[rd]<- temp

      Exceptions:

      None

      Programming Notes:

      SLL r0, r0, 0, expressed as NOP, is the assembly idiom used to denote no operation.

      SLL r0, r0, 1, expressed as SSNOP, is the assembly idiom used to denote no operation that causes an issue break on
      superscalar processors.');
INSERT INTO "instructions" VALUES('MIPS32','SLLV','


Shift Word Left Logical Variable                                                                                    SLLV


   31              26 25             21 20              16 15             11 10                6  5                  0

       SPECIAL                                                                         0                 SLLV
                              rs                 rt               rd
        000000                                                                      00000               000100

           6                  5                  5                 5                   5                   6


      Format:    SLLV rd, rt, rs                                                                   MIPS32 (MIPS I)

      Purpose: To left-shift a word by a variable number of bits

      Description: rd <- rt << rs

      The contents of the low-order 32-bit word of GPR rt are shifted left, inserting zeros into the emptied bits; the result
      word is placed in GPR rd. The bit-shift amount is specified by the low-order 5 bits of GPR rs.

      Restrictions: None

      Operation:
          s        <- GPR[rs]4..0
          temp     <- GPR[rt](31-s)..0 || 0      s

          GPR[rd]<- temp

      Exceptions: None

      Programming Notes:

      None');
INSERT INTO "instructions" VALUES('MIPS32','SLT','


Set on Less Than                                                                                                SLT


    31               26 25               21 20             16 15              11 10               6 5            0

        SPECIAL                                                                              0           SLT
                                  rs               rt                   rd
         000000                                                                           00000         101010

            6                     5                5                    5                    5            6


       Format: SLT rd, rs, rt                                                                        MIPS32 (MIPS I)

       Purpose:

       To record the result of a less-than comparison

       Description: rd <- (rs < rt)

       Compare the contents of GPR rs and GPR rt as signed integers and record the Boolean result of the comparison in
       GPR rd. If GPR rs is less than GPR rt, the result is 1 (true); otherwise, it is 0 (false).

       The arithmetic comparison does not cause an Integer Overflow exception.

       Restrictions:

       None

       Operation:

           if GPR[rs] < GPR[rt] then
               GPR[rd] <- 0       GPRLEN-1  || 1
           else
               GPR[rd] <- 0       GPRLEN

           endif

       Exceptions:

       None');
INSERT INTO "instructions" VALUES('MIPS32','SLTI','


Set on Less Than Immediate                                                                                             SLTI


    31               26 25               21 20            16 15                                                        0

           SLTI
                                  rs               rt                                  immediate
         001010

            6                     5                5                                       16


       Format: SLTI rt, rs, immediate                                                                 MIPS32 (MIPS I)

       Purpose:

       To record the result of a less-than comparison with a constant

       Description: rt <- (rs < immediate)

       Compare the contents of GPR rs and the 16-bit signed immediate as signed integers and record the Boolean result of
       the comparison in GPR rt. If GPR rs is less than immediate, the result is 1 (true); otherwise, it is 0 (false).

       The arithmetic comparison does not cause an Integer Overflow exception.

       Restrictions:

       None

       Operation:

           if GPR[rs] < sign_extend(immediate) then
               GPR[rd] <- 0       GPRLEN-1|| 1
           else
               GPR[rd] <- 0       GPRLEN

           endif

       Exceptions:

       None');
INSERT INTO "instructions" VALUES('MIPS32','SLTIU','


Set on Less Than Immediate Unsigned                                                                                     SLTIU


    31                26 25             21 20             16 15                                                            0

          SLTIU
                                 rs                rt                                 immediate
         001011

             6                    5                5                                       16


       Format: SLTIU rt, rs, immediate                                                                 MIPS32 (MIPS I)

       Purpose:

       To record the result of an unsigned less-than comparison with a constant

       Description: rt <- (rs < immediate)

       Compare the contents of GPR rs and the sign-extended 16-bit immediate as unsigned integers and record the Boolean
       result of the comparison in GPR rt. If GPR rs is less than immediate, the result is 1 (true); otherwise, it is 0 (false).

       Because the 16-bit immediate is sign-extended before comparison, the instruction can represent the smallest or largest
       unsigned numbers. The representable values are at the minimum [0, 32767] or maximum [max_unsigned-32767,
       max_unsigned] end of the unsigned range.

       The arithmetic comparison does not cause an Integer Overflow exception.

       Restrictions:

       None

       Operation:

           if (0 || GPR[rs]) < (0 || sign_extend(immediate)) then
                 GPR[rd] <- 0    GPRLEN-1 || 1
           else
                 GPR[rd] <- 0    GPRLEN

           endif

       Exceptions:

       None');
INSERT INTO "instructions" VALUES('MIPS32','SLTU','


Set on Less Than Unsigned                                                                                       SLTU


    31               26 25              21 20              16 15              11 10               6 5            0

        SPECIAL                                                                              0          SLTU
                                 rs                rt                   rd
         000000                                                                           00000         101011

            6                     5                5                    5                    5            6


       Format: SLTU rd, rs, rt                                                                       MIPS32 (MIPS I)

       Purpose:

       To record the result of an unsigned less-than comparison

       Description: rd <- (rs < rt)

       Compare the contents of GPR rs and GPR rt as unsigned integers and record the Boolean result of the comparison in
       GPR rd. If GPR rs is less than GPR rt, the result is 1 (true); otherwise, it is 0 (false).

       The arithmetic comparison does not cause an Integer Overflow exception.

       Restrictions:

       None

       Operation:

           if (0 || GPR[rs]) < (0 || GPR[rt]) then
               GPR[rd] <- 0      GPRLEN-1 || 1
           else
               GPR[rd] <- 0      GPRLEN

           endif

       Exceptions:

       None');
INSERT INTO "instructions" VALUES('MIPS32','SQRT.fmt','


Floating Point Square Root                                                                                      SQRT.fmt


       31               26 25               21 20             16 15              11 10          6   5                  0

             COP1                                       0                                                  SQRT
                                   fmt                                   fs               fd
            010001                                   00000                                                000100

               6                    5                   5                 5                5                 6


         Format: SQRT.S fd, fs                                                                      MIPS32 (MIPS II)
                    SQRT.D fd, fs                                                                   MIPS32 (MIPS II)

         Purpose:

         To compute the square root of an FP value

         Description: fd <- SQRT(fs)

         The square root of the value in FPR fs is calculated to infinite precision, rounded according to the current rounding
         mode in FCSR, and placed into FPR fd. The operand and result are values in format fmt.

         If the value in FPR fs corresponds to 0, the result is 0.

         Restrictions:

         If the value in FPR fs is less than 0, an Invalid Operation condition is raised.

         The fields fs and fd must specify FPRs valid for operands of type fmt; if they are not valid, the result is UNPRE-
         DICTABLE.

         The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value of the operand
         FPR becomes UNPREDICTABLE.

         Operation:

              StoreFPR(fd, fmt, SquareRoot(ValueFPR(fs, fmt)))

         Exceptions:

         Coprocessor Unusable, Reserved Instruction

         Floating Point Exceptions:

         Invalid Operation, Inexact, Unimplemented Operation');
INSERT INTO "instructions" VALUES('MIPS32','SRA','


Shift Word Right Arithmetic                                                                                           SRA


   31               26 25                21 20            16 15             11 10              6  5                    0

       SPECIAL                   0                                                                         SRA
                                                   rt                rd                 sa
         000000                00000                                                                      000011

             6                   5                 5                 5                  5                   6


      Format: SRA rd, rt, sa                                                                       MIPS32 (MIPS I)

      Purpose:

      To execute an arithmetic right-shift of a word by a fixed number of bits

      Description: rd <- rt >> sa                (arithmetic)

      The contents of the low-order 32-bit word of GPR rt are shifted right, duplicating the sign-bit (bit 31) in the emptied
      bits; the word result is placed in GPR rd. The bit-shift amount is specified by sa.

      Restrictions:

      None

      Operation:

           s       <- sa
           temp    <- (GPR[rt]31) || GPR[rt]31..s
                                       s

           GPR[rd]<- temp

      Exceptions: None');
INSERT INTO "instructions" VALUES('MIPS32','SRAV','


Shift Word Right Arithmetic Variable                                                                                 SRAV


   31               26 25                21 20            16 15             11 10              6   5                   0

       SPECIAL                                                                          0                  SRAV
                                 rs                rt                rd
         000000                                                                       00000               000111

             6                   5                 5                 5                  5                    6


      Format: SRAV rd, rt, rs                                                                       MIPS32 (MIPS I)

      Purpose:

      To execute an arithmetic right-shift of a word by a variable number of bits

      Description: rd <- rt >> rs                (arithmetic)

      The contents of the low-order 32-bit word of GPR rt are shifted right, duplicating the sign-bit (bit 31) in the emptied
      bits; the word result is placed in GPR rd. The bit-shift amount is specified by the low-order 5 bits of GPR rs.

      Restrictions:

      None

      Operation:

           s       <- GPR[rs]4..0
           temp    <- (GPR[rt]31) || GPR[rt]31..s
                                       s

           GPR[rd]<- temp

      Exceptions:

      None');
INSERT INTO "instructions" VALUES('MIPS32','SRL','


Shift Word Right Logical                                                                                             SRL


   31                26 25              21 20             16 15           11 10                6   5                  0

       SPECIAL                  0                                                                         SRL
                                                   rt               rd                 sa
        000000                00000                                                                     000010

            6                   5                  5                5                  5                   6


      Format: SRL rd, rt, sa                                                                        MIPS32 (MIPS I)

      Purpose:

      To execute a logical right-shift of a word by a fixed number of bits

      Description: rd <- rt >> sa               (logical)

      The contents of the low-order 32-bit word of GPR rt are shifted right, inserting zeros into the emptied bits; the word
      result is placed in GPR rd. The bit-shift amount is specified by sa.

      Restrictions:

      None

      Operation:

          s         <- sa
          temp      <- 0 || GPR[rt]31..s
                          s

          GPR[rd]<- temp

      Exceptions:

      None');
INSERT INTO "instructions" VALUES('MIPS32','SRLV','


Shift Word Right Logical Variable                                                                                    SRLV


   31                26 25              21 20             16 15             11 10              6   5                  0

       SPECIAL                                                                         0                 SRLV
                                rs                 rt               rd
        000000                                                                       00000              000110

            6                   5                  5                5                  5                   6


      Format: SRLV rd, rt, rs                                                                       MIPS32 (MIPS I)

      Purpose:

      To execute a logical right-shift of a word by a variable number of bits

      Description: rd <- rt >> rs                (logical)

      The contents of the low-order 32-bit word of GPR rt are shifted right, inserting zeros into the emptied bits; the word
      result is placed in GPR rd. The bit-shift amount is specified by the low-order 5 bits of GPR rs.

      Restrictions:

      None

      Operation:

          s         <- GPR[rs]4..0
          temp      <- 0 || GPR[rt]31..s
                          s

          GPR[rd]<- temp

      Exceptions:

      None');
INSERT INTO "instructions" VALUES('MIPS32','SSNOP','


Superscalar No Operation                                                                                        SSNOP


    31               26 25              21 20             16 15                11 10            6   5                 0

        SPECIAL                  0                  0                 0                  1                 SLL

         000000                00000              00000             00000              00001             000000

            6                    5                  5                 5                  5                  6


      Format: SSNOP                                                                                            MIPS32

      Purpose:

      Break superscalar issue on a superscalar processor.

      Description:

      SSNOP is the assembly idiom used to denote superscalar no operation. The actual instruction is interpreted by the
      hardware as SLL r0, r0, 1.

      This instruction alters the instruction issue behavior on a superscalar processor by forcing the SSNOP instruction to
      single-issue. The processor must then end the current instruction issue between the instruction previous to the SSNOP
      and the SSNOP. The SSNOP then issues alone in the next issue slot.

      On a single-issue processor, this instruction is a NOP that takes an issue slot.

      Restrictions:

      None

      Operation:

      None

      Exceptions:

      None

      Programming Notes:

      SSNOP is intended for use primarily to allow the programmer control over CP0 hazards by converting instructions
      into cycles in a superscalar processor. For example, to insert at least two cycles between an MTC0 and an ERET, one
      would use the following sequence:

          mtc0      x,y
          ssnop
          ssnop
          eret

      Based on the normal issues rules of the processor, the MTC0 issues in cycle T. Because the SSNOP instructions must
      issue alone, they may issue no earlier than cycle T+1 and cycle T+2, respectively. Finally, the ERET issues no earlier
      than cycle T+3.    Note that although the instruction after an SSNOP may issue no earlier than the cycle after the
      SSNOP is issued, that instruction may issue later. This is because other implementation-dependent issue rules may
      apply that prevent an issue in the next cycle. Processors should not introduce any unnecessary delay in issuing
      SSNOP instructions.');
INSERT INTO "instructions" VALUES('MIPS32','SUB','


Subtract Word                                                                                                        SUB


  31                 26 25              21 20            16 15             11 10               6    5                0

        SPECIAL                                                                         0                  SUB
                                 rs                rt               rd
         000000                                                                       00000               100010

            6                     5                 5                5                 5                     6


      Format: SUB rd, rs, rt                                                                         MIPS32 (MIPS I)

      Purpose:

      To subtract 32-bit integers. If overflow occurs, then trap

      Description: rd <- rs - rt

      The 32-bit word value in GPR rt is subtracted from the 32-bit value in GPR rs to produce a 32-bit result. If the sub-
      traction results in 32-bit 2''s complement arithmetic overflow, then the destination register is not modified and an Inte-
      ger Overflow exception occurs. If it does not overflow, the 32-bit result is placed into GPR rd.

      Restrictions:

      None

      Operation:

           temp <- (GPR[rs]31||GPR[rs]31..0) '' (GPR[rt]31||GPR[rt]31..0)
           if temp
                     32  != temp31 then
               SignalException(IntegerOverflow)
           else
               GPR[rd] <- temp31..0
           endif

      Exceptions:

      Integer Overflow

      Programming Notes:

      SUBU performs the same arithmetic operation but does not trap on overflow.');
INSERT INTO "instructions" VALUES('MIPS32','SUB.fmt','


  Floating Point Subtract                                                                                              SUB.fmt
');
INSERT INTO "instructions" VALUES('MIPS32','c','

31              26 25              21 20             16 15              11 10               6   5                  0

     COP1                                                                                               SUB
                           fmt                ft                fs                  fd
    010001                                                                                             000001

       6                    5                 5                 5                   5                     6


  Format: SUB.S fd, fs, ft                                                                       MIPS32 (MIPS I)
             SUB.D fd, fs, ft                                                                    MIPS32 (MIPS I)

  Purpose:

  To subtract FP values

  Description: fd <- fs - ft

  The value in FPR ft is subtracted from the value in FPR fs. The result is calculated to infinite precision, rounded
  according to the current rounding mode in FCSR, and placed into FPR fd. The operands and result are values in for-
  mat fmt. Restrictions:

  The fields fs, ft, and fd must specify FPRs valid for operands of type fmt. If they are not valid, the result is UNPRE-
  DICTABLE.

  The operands must be values in format fmt; if they are not, the result is UNPREDICTABLE and the value of the
  operand FPRs becomes UNPREDICTABLE.

  Operation:

      StoreFPR (fd, fmt, ValueFPR(fs, fmt)        ValueFPR(ft, fmt))
                                                          fmt

  CPU Exceptions:

  Coprocessor Unusable, Reserved Instruction

  FPU Exceptions:

  Inexact, Overflow, Underflow, Invalid Op, Unimplemented Op');
INSERT INTO "instructions" VALUES('MIPS32','SUBU','


Subtract Unsigned Word                                                                                             SUBU


   31              26 25               21 20             16 15             11 10              6   5                  0

        SPECIAL                                                                        0                 SUBU
                                rs                rt                rd
         000000                                                                      00000               100011

           6                    5                 5                 5                  5                    6


      Format: SUBU rd, rs, rt                                                                       MIPS32 (MIPS I)

      Purpose:

      To subtract 32-bit integers

      Description: rd <- rs - rt

      The 32-bit word value in GPR rt is subtracted from the 32-bit value in GPR rs and the 32-bit arithmetic result is and
      placed into GPR rd.

      No integer overflow exception occurs under any circumstances.

      Restrictions:

      None

      Operation:

          temp     <- GPR[rs] - GPR[rt]
          GPR[rd]<- temp

      Exceptions:

      None

      Programming Notes:

      The term !=unsigned!= in the instruction name is a misnomer; this operation is 32-bit modulo arithmetic that does not
      trap on overflow. It is appropriate for unsigned arithmetic, such as address arithmetic, or integer arithmetic environ-
      ments that ignore overflow, such as C language arithmetic.');
INSERT INTO "instructions" VALUES('MIPS32','SW','


Store Word                                                                                                             SW


   31               26 25              21 20              16 15                                                        0

          SW
                              base                 rt                                   offset
        101011

           6                   5                   5                                     16


      Format: SW rt, offset(base)                                                                    MIPS32 (MIPS I)

      Purpose:

      To store a word to memory

      Description: memory[base+offset] <- rt

      The least-significant 32-bit word of register rt is stored in memory at the location specified by the aligned effective
      address. The 16-bit signed offset is added to the contents of GPR base to form the effective address.

      Restrictions:

      The effective address must be naturally-aligned. If either of the 2 least-significant bits of the address is non-zero, an
      Address Error exception occurs.

      Operation:

          vAddr <- sign_extend(offset) + GPR[base]
          if vAddr            2
                     1..0  != 0 then
               SignalException(AddressError)
          endif
          (pAddr, CCA)<- AddressTranslation (vAddr, DATA, STORE)
          dataword<- GPR[rt]
          StoreMemory (CCA, WORD, dataword, pAddr, vAddr, DATA)

      Exceptions:

      TLB Refill, TLB Invalid, TLB Modified, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','SWC1','


Store Word from Floating Point                                                                                  SWC1


   31               26 25               21 20            16 15                                                   0

         SWC1
                               base                 ft                                  offset
        111001

           6                     5                  5                                    16


      Format: SWC1 ft, offset(base)                                                             MIPS32 (MIPS I)

      Purpose:

      To store a word from an FPR to memory

      Description: memory[base+offset] <- ft

      The low 32-bit word from FPR ft is stored in memory at the location specified by the aligned effective address. The
      16-bit signed offset is added to the contents of GPR base to form the effective address.

      Restrictions:

      An Address Error exception occurs if EffectiveAddress  1..0!= 0 (not word-aligned).



      Operation:

          vAddr <- sign_extend(offset) + GPR[base]
          if vAddr              3
                     1..0   != 0 then
               SignalException(AddressError)
          endif
          (pAddr, CCA) <- AddressTranslation(vAddr, DATA, STORE)
          dataword <- ValueFPR(ft, UNINTERPRETED_WORD)
          StoreMemory(CCA, WORD, dataword, pAddr, vAddr, DATA)

      Exceptions:

      Coprocessor Unusable, Reserved Instruction, TLB Refill, TLB Invalid, TLB Modified, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','SWC2','


Store Word from Coprocessor 2                                                                                        SWC2


   31               26 25              21 20               16 15                                                      0

         SWC2
                              base                 rt                                   offset
         111010

           6                    5                  5                                     16


      Format: SWC2 rt, offset(base)                                                                  MIPS32 (MIPS I)

      Purpose:

      To store a word from a COP2 register to memory

      Description: memory[base+offset] <- ft

      The low 32-bit word from COP2 (Coprocessor 2) register rt is stored in memory at the location specified by the
      aligned effective address. The 16-bit signed offset is added to the contents of GPR base to form the effective address.

      Restrictions:

      An Address Error exception occurs if EffectiveAddress    1..0!= 0 (not word-aligned).



      Operation:

          vAddr <- sign_extend(offset) + GPR[base]
          if vAddr             3
                     2..0  != 0 then
               SignalException(AddressError)
          endif
          (pAddr, CCA) <- AddressTranslation(vAddr, DATA, STORE)
          dataword <- CPR[2,rt,0]
          StoreMemory(CCA, WORD, dataword, pAddr, vAddr, DATA)

      Exceptions:

      Coprocessor Unusable, Reserved Instruction, TLB Refill, TLB Invalid, TLB Modified, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','SWL','


Store Word Left                                                                                                       SWL


   31                 26 25              21 20               16 15                                                     0

          SWL
                                 base                 rt                                    offset
         101010

            6                     5                   5                                       16


      Format: SWL rt, offset(base)                                                                        MIPS32 (MIPS I)

      Purpose:

      To store the most-significant part of a word to an unaligned memory address

      Description: memory[base+offset] <- rt

      The 16-bit signed offset is added to the contents of GPR base to form an effective address (EffAddr). EffAddr is the
      address of the most-significant of 4 consecutive bytes forming a word (W) in memory starting at an arbitrary byte
      boundary.

      A part of W, the most-significant 1 to 4 bytes, is in the aligned word containing EffAddr. The same number of the
      most-significant (left) bytes from the word in GPR rt are stored into these bytes of W.

      The following figure illustrates this operation using big-endian byte ordering for 32-bit and 64-bit registers. The 4
      consecutive bytes in 2..5 form an unaligned word starting at location 2. A part of W, 2 bytes, is located in the aligned
      word containing the most-significant byte at 2. First, SWL stores the most-significant 2 bytes of the low word from
      the source register into these 2 bytes in memory. Next, the complementary SWR stores the remainder of the unaligned
      word.

                                 Figure 3-6 Unaligned Word Store Using SWL and SWR


                    Word at byte 2 in memory, big-endian byte order; each memory byte contains its own address

                                    most  -- significance --        least


                                 0    1  2     3    4    5    6    7    8   ...     Memory: Initial contents


                                                              GPR 24                      E    F    G  H




                                 0    1  E     F    4    5    6   ... After executing SWL $24,2($0)


                                 0    1  E     F   G     H    6   ... Then after SWR $24,5($0)




      The bytes stored from the source register to memory depend on both the offset of the effective address within an
      aligned word--that is, the low 2 bits of the address (vAddr1..0)--and the current byte-ordering mode of the processor
      (big- or little-endian). The following figure shows the bytes stored for every combination of offset and byte ordering.
                               Figure 3-7 Bytes Stored by an SWL Instruction


                       Memory contents and byte offsets            Initial contents of Dest Register

                 0   1   2    3 <-big-endian                                  64-bit register

                 i   j   k    l          offset (vAddr1..0)      A    B     C    D    E    F   G    H

                 3   2   1    0 <-little-endian                 most        -- significance --         least

                most        least                                  32-bit register    E    F   G    H

                -- significance --

                              Memory contents after instruction (shaded is unchanged)

                                    Big-endian                     Little-endian
                                   byte ordering    vAddr1..0      byte ordering

                                  E    F    G   H        0        i   j     k    E

                                  i    E    F   G        1        i   j     E    F

                                  i    j    E   F        2        i   E     F    G

                                  i    j    k   E        3       E    F     G    H




    Restrictions:

    None

    Operation:

       vAddr <- sign_extend(offset) + GPR[base]
       (pAddr, CCA)<- AddressTranslation (vAddr, DATA, STORE)
       pAddr <- pAddrPSIZE-1..2 || (pAddr1..0 xor                   ReverseEndian )     2

       If BigEndianMem = 0 then
            pAddr <- pAddrPSIZE-1..2 || 0       2

       endif
       byte     <- vAddr1..0 xor BigEndianCPU          2

       dataword<- 0    248*byte  || GPR[rt]
       StoreMemory(CCA, byte, dataword, pAddr, vAddr, DATA)

    Exceptions:

    TLB Refill, TLB Invalid, TLB Modified, Bus Error, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','SWR','


Store Word Right                                                                                                     SWR


   31               26 25               21 20              16 15                                                      0

          SWR
                               base                 rt                                     offset
         101110

            6                    5                  5                                       16


      Format: SWR rt, offset(base)                                                                       MIPS32 (MIPS I)

      Purpose:

      To store the least-significant part of a word to an unaligned memory address

      Description: memory[base+offset] <- rt

      The 16-bit signed offset is added to the contents of GPR base to form an effective address (EffAddr). EffAddr is the
      address of the least-significant of 4 consecutive bytes forming a word (W) in memory starting at an arbitrary byte
      boundary.

      A part of W, the least-significant 1 to 4 bytes, is in the aligned word containing EffAddr. The same number of the
      least-significant (right) bytes from the word in GPR rt are stored into these bytes of W.

      The following figure illustrates this operation using big-endian byte ordering for 32-bit and 64-bit registers. The 4
      consecutive bytes in 2..5 form an unaligned word starting at location 2. A part of W, 2 bytes, is contained in the
      aligned word containing the least-significant byte at 5. First, SWR stores the least-significant 2 bytes of the low word
      from the source register into these 2 bytes in memory. Next, the complementary SWL stores the remainder of the
      unaligned word.

                               Figure 3-8 Unaligned Word Store Using SWR and SWL


                      Word at byte 2 in memory, big-endian byte order, each mem byte contains its address

                              least          -- significance --             least


                               0    1    2    3   4   5    6    7     8   ...     Memory: Initial contents


                                                           GPR 24                       E    F    G   H




                               0    1    2    3   G   H    6    ... After executing SWR $24,5($0)


                               0    1    E    F   G   H    6    ... Then after SWL $24,2($0)
    aligned word--that is, the low 2 bits of the address (vAddr1..0)--and the current byte-ordering mode of the processor
    (big- or little-endian). The following figure shows the bytes stored for every combination of offset and byte-ordering.

                                     Figure 3-9 Bytes Stored by SWR Instruction


                          Memory contents and byte offsets              Initial contents of Dest Register

                    0   1    2    3 <- big-endian                                 64-bit register

                    i    j   k    l           offset (vAddr1..0)     A     B    C    D    E    F    G    H

                    3   2    1    0 <- little-endian                most         -- significance --         least

                   most         least                                 32-bit register     E    F    G    H

                   -- significance --

                                  Memory contents after instruction (shaded is unchanged)

                                        Big-endian                   Little-endian byte
                                       byte ordering    vAddr1..0         ordering

                                      H    j    k    l       0       E     F    G    H

                                      G    H    k    l       1       F     G    H    l

                                      F    G   H     l       2       G     H     k   l

                                      E    F   G     H       3       H     j     k   l




    Restrictions:

    None

    Operation:
        vAddr <- sign_extend(offset) + GPR[base]
        (pAddr, CCA)<- AddressTranslation (vAddr, DATA, STORE)
        pAddr <- pAddrPSIZE-1..2 || (pAddr1..0 xor ReverseEndian )                       2

        If BigEndianMem = 0 then
              pAddr <- pAddrPSIZE-1..2 || 0          2

        endif
        byte       <- vAddr1..0 xor BigEndianCPU          2

        dataword<- GPR[rt] || 0             8*byte

        StoreMemory(CCA, WORD-byte, dataword, pAddr, vAddr, DATA)

    Exceptions:

    TLB Refill, TLB Invalid, TLB Modified, Bus Error, Address Error');
INSERT INTO "instructions" VALUES('MIPS32','SYNC','


Synchronize Shared Memory                                                                                           SYNC


   31              26 25               21 20           16 15               11 10             6   5                   0

       SPECIAL                                   0                                                      SYNC
                                                                                    stype
        000000                          00 0000 0000 0000 0                                             001111

           6                                     15                                   5                    6


      Format: SYNC (stype = 0 implied)                                                            MIPS32 (MIPS II)

      Purpose:

      To order loads and stores.

      Description:

      Simple Description:

    . SYNC affects only uncached and cached coherent loads and stores. The loads and stores that occur before the SYNC
      must be completed before the loads and stores after the SYNC are allowed to start.

    . Loads are completed when the destination register is written. Stores are completed when the stored value is visible to
      every other processor in the system.

    . SYNC is required, potentially in conjunction with SSNOP, to guarantee that memory reference results are visible
      across operating mode changes. For example, a SYNC is required on some implementations on entry to and exit from
      Debug Mode to guarantee that memory affects are handled correctly.

      Detailed Description:

    . When the stype field has a value of zero, every synchronizable load and store that occurs in the instruction stream
      before the SYNC instruction must be globally performed before any synchronizable load or store that occurs after the
      SYNC can be performed, with respect to any other processor or coherent I/O module.

    . SYNC does not guarantee the order in which instruction fetches are performed. The stype values 1-31 are reserved;
      they produce the same result as the value zero.

    .
    Terms:

    Synchronizable: A load or store instruction is synchronizable if the load or store occurs to a physical location in
    shared memory using a virtual location with a memory access type of either uncached or cached coherent. Shared
    memory is memory that can be accessed by more than one processor or by a coherent I/O system module.

    Performed load: A load instruction is performed when the value returned by the load has been determined. The result
    of a load on processor A has been determined with respect to processor or coherent I/O module B when a subsequent
    store to the location by B cannot affect the value returned by the load. The store by B must use the same memory
    access type as the load.

    Performed store: A store instruction is performed when the store is observable. A store on processor A is observable
    with respect to processor or coherent I/O module B when a subsequent load of the location by B returns the value
    written by the store. The load by B must use the same memory access type as the store.

    Globally performed load: A load instruction is globally performed when it is performed with respect to all processors
    and coherent I/O modules capable of storing to the location.

    Globally performed store: A store instruction is globally performed when it is globally observable. It is globally
    observable when it is observable by all processors and I/O modules capable of loading from the location.

    Coherent I/O module: A coherent I/O module is an Input/Output system component that performs coherent Direct
    Memory Access (DMA). It reads and writes memory independently as though it were a processor doing loads and
    stores to locations with a memory access type of cached coherent.

    The effect of SYNC on the global order of loads and stores for memory access types other than uncached and cached
    coherent is UNPREDICTABLE.

    Operation:
         SyncOperation(stype)

    Exceptions:

    None

    Programming Notes:

    A processor executing load and store instructions observes the order in which loads and stores using the same mem-
    ory access type occur in the instruction stream; this is known as program order.

    A parallel program has multiple instruction streams that can execute simultaneously on different processors. In mul-
    tiprocessor (MP) systems, the order in which the effects of loads and stores are observed by other processors--the
    global order of the loads and store--determines the actions necessary to reliably share data in parallel programs.

    When all processors observe the effects of loads and stores in program order, the system is strongly ordered. On such
    systems, parallel programs can reliably share data without explicit actions in the programs. For such a system, SYNC
    has the same effect as a NOP. Executing SYNC on such a system is not necessary, but neither is it an error.

    If a multiprocessor system is not strongly ordered, the effects of load and store instructions executed by one processor
    may be observed out of program order by other processors. On such systems, parallel programs must take explicit
    actions to reliably share data. At critical points in the program, the effects of loads and stores from an instruction
    stream must occur in the same order for all processors. SYNC separates the loads and stores executed on the proces-
    sor into two groups, and the effect of all loads and stores in one group is seen by all processors before the effect of any
    load or store in the subsequent group. In effect, SYNC causes the system to be strongly ordered for the executing pro-
    cessor at the instant that the SYNC is executed.

    Many MIPS-based multiprocessor systems are strongly ordered or have a mode in which they operate as strongly
    ordered for at least one memory access type. The MIPS architecture also permits implementation of MP systems that
    are not strongly ordered; SYNC enables the reliable use of shared memory on such systems. A parallel program that
    does not use SYNC generally does not operate on a system that is not strongly ordered. However, a program that does
    use SYNC works on both types of systems. (System-specific documentation describes the actions needed to reliably
    share data in parallel programs for that system.)

    The behavior of a load or store using one memory access type is undefined if a load or store was previously made to
    the same physical location using a different memory access type. The presence of a SYNC between the references
    does not alter this behavior.
    ally affect the physical memory-system ordering or synchronization issues that arise in system programming. The
    effect of SYNC on implementation-specific aspects of the cached memory system, such as writeback buffers, is not
    defined. The effect of SYNC on reads or writes to memory caused by privileged implementation-specific instructions,
    such as CACHE, also is not defined.

         # Processor A (writer)
         # Conditions at entry:
         # The value 0 has been stored in FLAG and that value is observable by B
         SW       R1, DATA               # change shared DATA value
         LI       R2, 1
         SYNC                            # Perform DATA store before performing FLAG store
         SW       R2, FLAG               # say that the shared DATA value is valid

             # Processor B (reader)
                  LI       R2, 1
             1: LW         R1, FLAG      # Get FLAG
                  BNE      R2, R1, 1B# if it says that DATA is not valid, poll again
                  NOP
                  SYNC                   # FLAG value checked before doing DATA read
                  LW       R1, DATA      # Read (valid) shared DATA value

    Prefetch operations have no effect detectable by User-mode programs, so ordering the effects of prefetch operations is
    not meaningful.

    The code fragments above shows how SYNC can be used to coordinate the use of shared data between separate writer
    and reader instruction streams in a multiprocessor environment. The FLAG location is used by the instruction streams
    to determine whether the shared data item DATA is valid. The SYNC executed by processor A forces the store of
    DATA to be performed globally before the store to FLAG is performed. The SYNC executed by processor B ensures
    that DATA is not read until after the FLAG value indicates that the shared data is valid.');
INSERT INTO "instructions" VALUES('MIPS32','SYSCALL','


System Call                                                                                                   SYSCALL




      31              26 25                                                                    6   5                  0

         SPECIAL                                                                                        SYSCALL
                                                           code
          000000                                                                                         001100

             6                                             20                                               6


        Format: SYSCALL                                                                              MIPS32 (MIPS I)

        Purpose:

        To cause a System Call exception

        Description:

        A system call exception occurs, immediately and unconditionally transferring control to the exception handler.

        The code field is available for use as software parameters, but is retrieved by the exception handler only by loading
        the contents of the memory word containing the instruction.

        Restrictions:

        None

        Operation:

            SignalException(SystemCall)

        Exceptions:

        System Call');
INSERT INTO "instructions" VALUES('MIPS32','TEQ','


Trap if Equal                                                                                                     TEQ


   31                26 25              21 20            16 15                              6   5                  0

         SPECIAL                                                                                         TEQ
                                rs                rt                         code
          000000                                                                                       110100

            6                    5                 5                          10                          6


       Format: TEQ rs, rt                                                                       MIPS32 (MIPS II)

       Purpose:

       To compare GPRs and do a conditional trap

       Description: if rs = rt then Trap

       Compare the contents of GPR rs and GPR rt as signed integers; if GPR rs is equal to GPR rt, then take a Trap excep-
       tion.

       The contents of the code field are ignored by hardware and may be used to encode information for system software.
       To retrieve the information, system software must load the instruction word from memory.

       Restrictions:

       None

       Operation:

           if GPR[rs] = GPR[rt] then
                SignalException(Trap)
           endif

       Exceptions:

       Trap');
INSERT INTO "instructions" VALUES('MIPS32','TEQI','


Trap if Equal Immediate                                                                                           TEQI


   31                26 25            21 20             16 15                                                     0

        REGIMM                                 TEQI
                                rs                                                 immediate
         000001                                01100

             6                  5               5                                     16


       Format: TEQI rs, immediate                                                               MIPS32 (MIPS II)

       Purpose:

       To compare a GPR to a constant and do a conditional trap

       Description: if rs = immediate then Trap

       Compare the contents of GPR rs and the 16-bit signed immediate as signed integers; if GPR rs is equal to immediate,
       then take a Trap exception.

       Restrictions:

       None

       Operation:

           if GPR[rs] = sign_extend(immediate) then
                    SignalException(Trap)
           endif

       Exceptions:

       Trap');
INSERT INTO "instructions" VALUES('MIPS32','TGE','


Trap if Greater or Equal                                                                                            TGE


   31                26 25              21 20            16 15                                6   5                  0

        SPECIAL                                                                                           TGE
                                rs                rt                         code
         000000                                                                                          110000

            6                    5                 5                          10                           6


       Format: TGE rs, rt                                                                         MIPS32 (MIPS II)

       Purpose:

       To compare GPRs and do a conditional trap

       Description: if rs # rt then Trap

       Compare the contents of GPR rs and GPR rt as signed integers; if GPR rs is greater than or equal to GPR rt, then take
       a Trap exception.

       The contents of the code field are ignored by hardware and may be used to encode information for system software.
       To retrieve the information, system software must load the instruction word from memory.

       Restrictions:

       None

       Operation:

           if GPR[rs] # GPR[rt] then
                SignalException(Trap)
           endif

       Exceptions:

       Trap');
INSERT INTO "instructions" VALUES('MIPS32','TGEI','


Trap if Greater or Equal Immediate                                                                                  TGEI


   31               26 25              21 20            16 15                                                        0

        REGIMM                                  TGEI
                                rs                                                 immediate
          000001                                01000

            6                    5               5                                     16


       Format: TGEI rs, immediate                                                                MIPS32 (MIPS II)

       Purpose:

       To compare a GPR to a constant and do a conditional trap

       Description: if rs # immediate then Trap

       Compare the contents of GPR rs and the 16-bit signed immediate as signed integers; if GPR rs is greater than or equal
       to immediate, then take a Trap exception.

       Restrictions:

       None

       Operation:

           if GPR[rs] # sign_extend(immediate) then
               SignalException(Trap)
           endif

       Exceptions:

       Trap');
INSERT INTO "instructions" VALUES('MIPS32','TGEIU','


Trap if Greater or Equal Immediate Unsigned                                                                      TGEIU


    31               26 25              21 20            16 15                                                       0

        REGIMM                                  TGEIU
                                rs                                                  immediate
         000001                                  01001

            6                   5                  5                                    16


       Format:    TGEIU rs, immediate                                                             MIPS32 (MIPS II)

       Purpose:

       To compare a GPR to a constant and do a conditional trap

       Description: if rs # immediate then Trap

       Compare the contents of GPR rs and the 16-bit sign-extended immediate as unsigned integers; if GPR rs is greater
       than or equal to immediate, then take a Trap exception.

       Because the 16-bit immediate is sign-extended before comparison, the instruction can represent the smallest or largest
       unsigned numbers. The representable values are at the minimum [0, 32767] or maximum [max_unsigned-32767,
       max_unsigned] end of the unsigned range.

       Restrictions:

       None

       Operation:

           if (0 || GPR[rs]) # (0 || sign_extend(immediate)) then
                SignalException(Trap)
           endif

       Exceptions:

       Trap');
INSERT INTO "instructions" VALUES('MIPS32','TGEU','


Trap if Greater or Equal Unsigned                                                                              TGEU


   31                26 25              21 20            16 15                              6  5                 0

        SPECIAL                                                                                       TGEU
                                rs                rt                         code
         000000                                                                                       110001

            6                    5                 5                          10                         6


       Format: TGEU rs, rt                                                                      MIPS32 (MIPS II)

       Purpose:

       To compare GPRs and do a conditional trap

       Description: if rs # rt then Trap

       Compare the contents of GPR rs and GPR rt as unsigned integers; if GPR rs is greater than or equal to GPR rt, then
       take a Trap exception.

       The contents of the code field are ignored by hardware and may be used to encode information for system software.
       To retrieve the information, system software must load the instruction word from memory.

       Restrictions:

       None

       Operation:

           if (0 || GPR[rs]) # (0 || GPR[rt]) then
                SignalException(Trap)
           endif

       Exceptions:

       Trap');
INSERT INTO "instructions" VALUES('MIPS32','TLBP','



 Probe TLB for Matching Entry                                                                               TLBP



31                26 25 24                                                               6   5                  0

       COP0          CO                                     0                                       TLBP

      010000          1                        000 0000 0000 0000 0000                              001000

          6           1                                    19                                          6


   Format: TLBP                                                                                          MIPS32

   Purpose:

   To find a matching entry in the TLB.

   Description:

   The Index register is loaded with the address of the TLB entry whose contents match the contents of the EntryHi reg-
   ister. If no TLB entry matches, the high-order bit of the Index register is set.

   Restrictions:



   Operation:

        Index <- 1 || UNPREDICTABLE          31

        for i in 0...TLBEntries-1
              if ((TLB[i]       and not (TLB[i]           )) =
                           VPN2                       Mask
                          (EntryHi        and not (TLB[i]           ))) and
                                    VPN2                       Mask
                 ((TLB[i] = 1) or (TLB[i]                = EntryHi        ))then
                            G                     ASID               ASID
                 Index <- i
              endif
        endfor

   Exceptions:

   Coprocessor Unusable');
INSERT INTO "instructions" VALUES('MIPS32','TLBR','



  Read Indexed TLB Entry                                                                                            TLBR

   31                 26 25 24                                                                 6   5                  0

          COP0           CO                                     0                                          TLBR

         010000           1                          000 0000 0000 0000 0000                              000001

             6            1                                     19                                           6


       Format: TLBR                                                                                             MIPS32

       Purpose:

       To read an entry from the TLB.

       Description:

       The EntryHi, EntryLo0, EntryLo1, and PageMask registers are loaded with the contents of the TLB entry pointed to
       by the Index register. Note that the value written to the EntryHi, EntryLo0, and EntryLo1 registers may be different
       from that originally written to the TLB via these registers in that:

         . The value returned in the VPN2 field of the EntryHi register may havethose bits set to zero corresponding to the
            one bits in the Mask field of the TLB entry (the least significant bit of VPN2 corresponds to the least significant
            bit of the Mask field). It is implementation dependent whether these bits are preserved or zeroed after a TLB
            entry is written and then read.

         . The value returned in the PFN field of the EntryLo0 and EntryLo1 registers may havethose bits set to zero
            corresponding to the one bits in the Mask field of the TLB entry (the least significant bit of PFN corresponds to
            the least significant bit of the Mask field). It is implementation dependent whether these bits are preserved or
            zeroed after a TLB entry is written and then read.

         . The value returned in the G bit in both the EntryLo0 and EntryLo1 registers comes from the single G bit in the
            TLB entry. Recall that this bit was set from the logical AND of the two G bits in EntryLo0 and EntryLo1 when
            the TLB was written.

       Restrictions:

       The operation is UNDEFINED if the contents of the Index register are greater than or equal to the number of TLB
       entries in the processor.
Read Indexed TLB Entry                                                                          TLBR


     Operation:

        i <- Index
        if i > (TLBEntries - 1) then
            UNDEFINED
        endif
        PageMask
                  Mask <- TLB[i]Mask
        EntryHi <-
                    (TLB[i]    and not TLB[i]    ) || # Masking implementation dependent
                           VPN2              Mask
                     5
                    0 || TLB[i]
                                ASID
        EntryLo1 <- 0 || 2

                    (TLB[i]    and not TLB[i]    ) || # Masking mplementation dependent
                           PFN1              Mask
                    TLB[i]   || TLB[i]  || TLB[i]   || TLB[i]
                          C1          D1         V1            G
        EntryLo0 <- 0 || 2

                    (TLB[i]    and not TLB[i]    ) || # Masking mplementation dependent
                           PFN0              Mask
                    TLB[i]   || TLB[i]  || TLB[i]   || TLB[i]
                          C0          D0         V0            G



     Exceptions:

     Coprocessor Unusable');
INSERT INTO "instructions" VALUES('MIPS32','TLBWI','



    Write Indexed TLB Entry                                                                                  TLBWI


   31               26 25 24                                                                 6   5                  0

        COP0           CO                                     0                                        TLBWI

        010000          1                          000 0000 0000 0000 0000                             000010

           6            1                                    19                                           6


     Format: TLBWI                                                                                          MIPS32

     Purpose:

     To write a TLB entry indexed by the Index register.

     Description:

     The TLB entry pointed to by the Index register is written from the contents of the EntryHi, EntryLo0, EntryLo1, and
     PageMask registers. The information written to the TLB entry may be different from that in the EntryHi, EntryLo0,
     and EntryLo1 registers, in that:

        . The value written to the VPN2 field of the TLB entry may have those bits set to zero corresponding to the one
          bits in the Mask field of the PageMask register (the least significant bit of VPN2 corresponds to the least
          significant bit of the Mask field). It is implementation dependent whether these bits are preserved or zeroed
          during a TLB write.

        . The value written to the PFN0 and PFN1 fields of the TLB entry may have those bits set to zero corresponding to
          the one bits in the Mask field of PageMask register (the least significant bit of PFN corresponds to the least
          significant bit of the Mask field). It is implementation dependent whether these bits are preserved or zeroed
          during a TLB write.

        . The single G bit in the TLB entry is set from the logical AND of the G bits in the EntryLo0 and EntryLo1
          registers.

     Restrictions:

     The operation is UNDEFINED if the contents of the Index register are greater than or equal to the number of TLB
     entries in the processor.
Write Indexed TLB Entry                                                                  TLBWI


 Operation:

      i <- Index
      TLB[i]
             Mask<- PageMaskMask
      TLB[i]
             VPN2<- EntryHiVPN2 and not PageMaskMask # Implementation dependent
      TLB[i]
             ASID<- EntryHiASID
      TLB[i] <- EntryLo1G and EntryLo0G
             G
      TLB[i]
             PFN1<- EntryLo1PFN and not PageMaskMask # Implementation dependent
      TLB[i]
             C1 <- EntryLo1C
      TLB[i]
             D1 <- EntryLo1D
      TLB[i]
             V1 <- EntryLo1V
      TLB[i]
             PFN0<- EntryLo0PFN and not PageMaskMask # Implementation dependent
      TLB[i]
             C0 <- EntryLo0C
      TLB[i]
             D0 <- EntryLo0D
      TLB[i]
             V0 <- EntryLo0V

 Exceptions:

  Coprocessor Unusable');
INSERT INTO "instructions" VALUES('MIPS32','TLBWR','



   Write Random TLB Entry                                                                                       TLBWR


    31                26 25 24                                                                 6   5                  0

          COP0           CO                                     0                                        TLBWR

         010000           1                          000 0000 0000 0000 0000                             000110

             6            1                                    19                                           6


       Format: TLBWR                                                                                          MIPS32

       Purpose:

       To write a TLB entry indexed by the Random register.

       Description:

       The TLB entry pointed to by the Random register is written from the contents of the EntryHi, EntryLo0, EntryLo1,
       and PageMask registers. The information written to the TLB entry may be different from that in the EntryHi,
       EntryLo0, and EntryLo1 registers, in that:

         . The value written to the VPN2 field of the TLB entry may have those bits set to zero corresponding to the one
            bits in the Mask field of the PageMask register (the least significant bit of VPN2 corresponds to the least
            significant bit of the Mask field). It is implementation dependent whether these bits are preserved or zeroed
            during a TLB write.

         . The value written to the PFN0 and PFN1 fields of the TLB entry may have those bits set to zero corresponding to
            the one bits in the Mask field of PageMask register (the least significant bit of PFN corresponds to the least
            significant bit of the Mask field). It is implementation dependent whether these bits are preserved or zeroed
            during a TLB write.

         . The value returned in the G bit in both the EntryLo0 and EntryLo1 registers comes from the single G bit in the
            TLB entry. Recall that this bit was set from the logical AND of the two G bits in EntryLo0 and EntryLo1 when
            the TLB was written.

       Restrictions:

       The operation is UNDEFINED if the contents of the Index register are greater than or equal to the number of TLB
       entries in the processor.
Write Random TLB Entry                                                                     TLBWR


    Operation:

        i <- Random
        TLB[i]
               Mask<- PageMaskMask
        TLB[i]
               VPN2<- EntryHiVPN2 and not PageMaskMask # Implementation dependent
        TLB[i]
               ASID<- EntryHiASID
        TLB[i] <- EntryLo1G and EntryLo0G
               G
        TLB[i]
               PFN1<- EntryLo1PFN and not PageMaskMask # Implementation dependent
        TLB[i]
               C1 <- EntryLo1C
        TLB[i]
               D1 <- EntryLo1D
        TLB[i]
               V1 <- EntryLo1V
        TLB[i]
               PFN0<- EntryLo0PFN and not PageMaskMask # Implementation dependent
        TLB[i]
               C0 <- EntryLo0C
        TLB[i]
               D0 <- EntryLo0D
        TLB[i]
               V0 <- EntryLo0V

    Exceptions:

    Coprocessor Unusable');
INSERT INTO "instructions" VALUES('MIPS32','TLT','


Trap if Less Than                                                                                                   TLT


   31                26 25              21 20            16 15                               6  5                   0

         SPECIAL                                                                                          TLT
                                rs                rt                         code
          000000                                                                                        110010

            6                    5                 5                          10                           6


       Format: TLT rs, rt                                                                        MIPS32 (MIPS II)

       Purpose:

       To compare GPRs and do a conditional trap

       Description: if rs < rt then Trap

       Compare the contents of GPR rs and GPR rt as signed integers; if GPR rs is less than GPR rt, then take a Trap excep-
       tion.

       The contents of the code field are ignored by hardware and may be used to encode information for system software.
       To retrieve the information, system software must load the instruction word from memory.

       Restrictions:

       None

       Operation:

           if GPR[rs] < GPR[rt] then
                SignalException(Trap)
           endif

       Exceptions:

       Trap');
INSERT INTO "instructions" VALUES('MIPS32','TLTI','


Trap if Less Than Immediate                                                                                        TLTI


   31                26 25            21 20             16 15                                                      0

        REGIMM                                 TLTI
                                rs                                                 immediate
         000001                                01010

             6                  5                5                                    16


       Format: TLTI rs, immediate                                                                MIPS32 (MIPS II)

       Purpose:

       To compare a GPR to a constant and do a conditional trap

       Description: if rs < immediate then Trap

       Compare the contents of GPR rs and the 16-bit signed immediate as signed integers; if GPR rs is less than immediate,
       then take a Trap exception.

       Restrictions:

       None

       Operation:

           if GPR[rs] < sign_extend(immediate) then
               SignalException(Trap)
           endif

       Exceptions:

       Trap');
INSERT INTO "instructions" VALUES('MIPS32','TLTIU','


Trap if Less Than Immediate Unsigned                                                                              TLTIU


   31               26 25              21 20            16 15                                                        0

        REGIMM                                  TLTIU
                                rs                                                  immediate
         000001                                 01011

            6                   5                 5                                     16


       Format: TLTIU rs, immediate                                                                MIPS32 (MIPS II)

       Purpose:

       To compare a GPR to a constant and do a conditional trap

       Description: if rs < immediate then Trap

       Compare the contents of GPR rs and the 16-bit sign-extended immediate as unsigned integers; if GPR rs is less than
       immediate, then take a Trap exception.

       Because the 16-bit immediate is sign-extended before comparison, the instruction can represent the smallest or largest
       unsigned numbers. The representable values are at the minimum [0, 32767] or maximum [max_unsigned-32767,
       max_unsigned] end of the unsigned range.

       Restrictions:

       None

       Operation:

           if (0 || GPR[rs]) < (0 || sign_extend(immediate)) then
               SignalException(Trap)
           endif

       Exceptions:

       Trap');
INSERT INTO "instructions" VALUES('MIPS32','TLTU','


Trap if Less Than Unsigned                                                                                     TLTU


   31                26 25              21 20            16 15                              6  5                0

         SPECIAL                                                                                     TLTU
                                rs                rt                         code
         000000                                                                                     110011

            6                    5                 5                          10                       6


       Format: TLTU rs, rt                                                                     MIPS32 (MIPS II)

       Purpose:

       To compare GPRs and do a conditional trap

       Description: if rs < rt then Trap

       Compare the contents of GPR rs and GPR rt as unsigned integers; if GPR rs is less than GPR rt, then take a Trap
       exception.

       The contents of the code field are ignored by hardware and may be used to encode information for system software.
       To retrieve the information, system software must load the instruction word from memory.

       Restrictions:

       None

       Operation:

           if (0 || GPR[rs]) < (0 || GPR[rt]) then
                SignalException(Trap)
           endif

       Exceptions:

       Trap');
INSERT INTO "instructions" VALUES('MIPS32','TNE','


Trap if Not Equal                                                                                               TNE


   31                26 25              21 20            16 15                              6  5                0

        SPECIAL                                                                                      TNE
                                rs                rt                         code
         000000                                                                                     110110

            6                    5                 5                          10                       6


       Format: TNE rs, rt                                                                      MIPS32 (MIPS II)

       Purpose:

       To compare GPRs and do a conditional trap

       Description: if rs != rt then Trap

       Compare the contents of GPR rs and GPR rt as signed integers; if GPR rs is not equal to GPR rt, then take a Trap
       exception.

       The contents of the code field are ignored by hardware and may be used to encode information for system software.
       To retrieve the information, system software must load the instruction word from memory.

       Restrictions:

       None

       Operation:

           if GPR[rs] != GPR[rt] then
                     SignalException(Trap)
           endif

       Exceptions:

       Trap');
INSERT INTO "instructions" VALUES('MIPS32','TNEI','


Trap if Not Equal                                                                                                 TNEI


   31                26 25              21 20          16 15                                                       0

        REGIMM                                 TNEI
                                 rs                                               immediate
         000001                                01110

             6                   5              5                                    16


       Format: TNEI rs, immediate                                                              MIPS32 (MIPS II)

       Purpose:

       To compare a GPR to a constant and do a conditional trap

       Description: if rs != immediate then Trap

       Compare the contents of GPR rs and the 16-bit signed immediate as signed integers; if GPR rs is not equal to imme-
       diate, then take a Trap exception.

       Restrictions:

       None

       Operation:

           if GPR[rs] != sign_extend(immediate) then
                SignalException(Trap)
           endif

       Exceptions:

       Trap');
INSERT INTO "instructions" VALUES('MIPS32','TRUNC.W.fmt','


Floating Point Truncate to Word Fixed Point                                                                     TRUNC.W.fmt


         31              26 25              21 20              16 15               11 10             6   5                   0

              COP1                                     0                                                      TRUNC.W
                                   fmt                                     fs               fd
              010001                                00000                                                       001101

                6                   5                  5                    5                5                    6


           Format: TRUNC.W.S fd, fs                                                                      MIPS32 (MIPS II)
                      TRUNC.W.D fd, fs                                                                   MIPS32 (MIPS II)

           Purpose:

           To convert an FP value to 32-bit fixed point, rounding toward zero

           Description: fd <- convert_and_round(fs)

           The value in FPR fs, in format fmt, is converted to a value in 32-bit word fixed point format using rounding toward
           zero (rounding mode 1). The result is placed in FPR fd.

           When the source value is Infinity, NaN, or rounds to an integer outside the range -2^31 to 2^31-1, the result cannot be
           represented correctly and an IEEE Invalid Operation condition exists. In this case the Invalid Operation flag is set in
           the FCSR. If the Invalid Operation Enable bit is set in the FCSR, no result is written to fd and an Invalid Operation
           exception is taken immediately. Otherwise, the default result, 2^31-1, is written to fd.

           Restrictions:

           The fields fs and fd must specify valid FPRs; fs for type fmt and fd for word fixed point; if they are not valid, the result
           is UNPREDICTABLE.

           The operand must be a value in format fmt; if it is not, the result is UNPREDICTABLE and the value of the operand
           FPR becomes UNPREDICTABLE.

           Operation:

               StoreFPR(fd, W, ConvertFmt(ValueFPR(fs, fmt), fmt, W))
     Exceptions:

     Coprocessor Unusable, Reserved Instruction

     Floating Point Exceptions:

     Inexact, Invalid Operation, Overflow, Unimplemented Operation');
INSERT INTO "instructions" VALUES('MIPS32','WAIT','



   Enter Standby Mode                                                                                             WAIT


  31               26 25 24                                                                      6    5                0

        COP0            CO                                                                                  WAIT
                                                Implementation-Dependent Code
       010000           1                                                                                   100000

          6             1                                      19                                              6


    Format: WAIT                                                                                                MIPS32

    Purpose:

    Wait for Event

    Description:

    The WAIT instruction performs an implementation-dependent operation, usually involving a lower power mode.
    Software may use bits 24:6 of the instruction to communicate additional information to the processor, and the proces-
    sor may use this information as control for the lower power mode. A value of zero for bits 24:6 is the default and must
    be valid in all implementations.


    The WAIT instruction is typically implemented by stalling the pipeline at the completion of the instruction and enter-
    ing a lower power mode. The pipeline is restarted when an external event, such as an interrupt or external request
    occurs, and execution continues with the instruction following the WAIT instruction. It is implementation-dependent
    whether the pipeline restarts when a non-enabled interrupt is requested. In this case, software must poll for the cause
    of the restart. If the pipeline restarts as the result of an enabled interrupt, that interrupt is taken between the WAIT
    instruction and the following instruction (EPC for the interrupt points at the instruction following the WAIT instruc-
    tion).


    The assertion of any reset or NMI must restart the pipelihne and the corresponding exception myust be taken.

    Restrictions:

    The operation of the processor is UNDEFINED if a WAIT instruction is placed in the delay slot of a branch or a
    jump.
Enter Standby Mode (cont.)                                                               WAIT

 Operation:

      Enter implementation dependent lower power mode

 Exceptions:

  Coprocessor Unusable Exception');
INSERT INTO "instructions" VALUES('MIPS32','XOR','


Exclusive OR                                                                                                XOR


   31              26 25              21 20         16 15             11 10            6   5                 0

        SPECIAL                                                                  0                XOR
                               rs             rt               rd
         000000                                                                00000             100110

           6                   5              5                5                5                   6


      Format: XOR rd, rs, rt                                                                MIPS32 (MIPS I)

      Purpose:

      To do a bitwise logical Exclusive OR

      Description: rd <- rs XOR rt

      Combine the contents of GPR rs and GPR rt in a bitwise logical Exclusive OR operation and place the result into
      GPR rd.

      Restrictions:

      None

      Operation:

          GPR[rd] <- GPR[rs] xor GPR[rt]

      Exceptions:

      None');
COMMIT;
