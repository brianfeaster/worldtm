VMOP(NOP,,,,)

VMOP(MV,R00,,I,) VMOP(MV,R01,,I,) VMOP(MV,R02,,I,) VMOP(MV,R03,,I,) VMOP(MV,R04,,I,) VMOP(MV,R05,,I,) VMOP(MV,R06,,I,) VMOP(MV,R07,,I,)
VMOP(MV,R0E,,I,)
VMOP(MV,R10,,I,) VMOP(MV,R11,,I,) VMOP(MV,R12,,I,) VMOP(MV,R13,,I,)

VMOP(MV,R00,R00,,) VMOP(MV,R00,R01,,) VMOP(MV,R00,R02,,) VMOP(MV,R00,R03,,) VMOP(MV,R00,R04,,) VMOP(MV,R00,R07,,) VMOP(MV,R00,R0D,,) VMOP(MV,R00,R0E,,) VMOP(MV,R00,R10,,) VMOP(MV,R00,R11,,)
VMOP(MV,R01,R00,,) VMOP(MV,R01,R02,,) VMOP(MV,R01,R03,,) VMOP(MV,R01,R04,,)
VMOP(MV,R02,R00,,) VMOP(MV,R02,R01,,) VMOP(MV,R02,R03,,)
VMOP(MV,R03,R00,,) VMOP(MV,R03,R01,,) VMOP(MV,R03,R11,,)
VMOP(MV,R04,R00,,)
VMOP(MV,R05,R00,,) VMOP(MV,R05,R08,,) VMOP(MV,R05,R09,,) VMOP(MV,R05,R0B,,) VMOP(MV,R05,R0C,,)
VMOP(MV,R06,R01,,)
VMOP(MV,R07,R02,,)
VMOP(MV,R08,R00,,)
VMOP(MV,R0B,R00,,) VMOP(MV,R0B,R05,,) VMOP(MV,R0B,R09,,)
VMOP(MV,R0C,R00,,) VMOP(MV,R0C,R05,,) VMOP(MV,R0C,R08,,)
VMOP(MV,R10,R11,,) VMOP(MV,R10,R12,,) VMOP(MV,R10,R13,,)
VMOP(MV,R11,R00,,) VMOP(MV,R11,R01,,)
VMOP(MV,R12,R02,,)

VMOP(LD,R00,R01,,)
VMOP(LD,R11,R01,,)

VMOP(LD,R00,R01,R02,)

VMOP(LD,R00,R00,I,) VMOP(LD,R00,R01,I,) VMOP(LD,R00,R02,I,) VMOP(LD,R00,R0B,I,) VMOP(LD,R00,R0C,I,) VMOP(LD,R00,R1F,I,)
VMOP(LD,R01,R00,I,) VMOP(LD,R01,R01,I,) VMOP(LD,R01,R0B,I,) VMOP(LD,R01,R0C,I,)
VMOP(LD,R02,R00,I,) VMOP(LD,R02,R01,I,) VMOP(LD,R02,R02,I,)
VMOP(LD,R05,R00,I,) VMOP(LD,R05,R0B,I,)
VMOP(LD,R0B,R00,I,)
VMOP(LD,R0C,R00,I,)
VMOP(LD,R10,R00,I,) VMOP(LD,R10,R01,I,) VMOP(LD,R10,R02,I,) VMOP(LD,R10,R03,I,)
VMOP(LD,R11,R01,I,) VMOP(LD,R11,R02,I,) VMOP(LD,R11,R03,I,)

VMOP(ST,R00,R01,I,) VMOP(ST,R00,R0C,I,) VMOP(ST,R00,R05,I,) VMOP(ST,R00,R0B,I,) VMOP(ST,R00,R1F,I,)
VMOP(ST,R02,R00,I,) VMOP(ST,R02,R01,I,)
VMOP(ST,R03,R00,I,)
VMOP(ST,R05,R00,I,)

VMOP(ST,R11,R01,,)

VMOP(ST,R00,R01,R02,)

VMOP(PUSH,R00,,,) VMOP(PUSH,R01,,,) VMOP(PUSH,R02,,,) VMOP(PUSH,R03,,,) VMOP(PUSH,R04,,,) VMOP(PUSH,R05,,,) VMOP(PUSH,R07,,,)
VMOP(PUSH,R09,,,) VMOP(PUSH,R0A,,,) VMOP(PUSH,R0B,,,) VMOP(PUSH,R0C,,,)

VMOP(PUSH,R10,,,) VMOP(PUSH,R11,,,) VMOP(PUSH,R12,,,) VMOP(PUSH,R13,,,) VMOP(PUSH,R1C,,,)

VMOP(POP,R00,,,) VMOP(POP,R01,,,) VMOP(POP,R02,,,) VMOP(POP,R03,,,) VMOP(POP,R04,,,) VMOP(POP,R05,,,) VMOP(POP,R07,,,)
VMOP(POP,R09,,,) VMOP(POP,R0A,,,) VMOP(POP,R0B,,,) VMOP(POP,R0C,,,)

VMOP(POP,R10,,,) VMOP(POP,R11,,,) VMOP(POP,R12,,,) VMOP(POP,R13,,,) VMOP(POP,R1C,,,)

VMOP(LSL,R02,,I,)
VMOP(LSL,R10,,I,)

VMOP(LSR,R10,,I,)

VMOP(ADD,R00,R01,,) VMOP(ADD,R00,R02,,)
VMOP(ADD,R01,R00,,) VMOP(ADD,R01,R02,,)
VMOP(ADD,R02,R00,,) VMOP(ADD,R02,R01,,) VMOP(ADD,R02,R03,,)
VMOP(ADD,R03,R02,,)
VMOP(ADD,R10,R00,,) VMOP(ADD,R10,R13,,)
VMOP(ADD,R11,R00,,) VMOP(ADD,R11,R10,,)
VMOP(ADD,R12,R00,,) VMOP(ADD,R12,R10,,)
VMOP(ADD,R13,R00,,) VMOP(ADD,R13,R10,,)
VMOP(ADD,R00,,I,) VMOP(ADD,R01,,I,) VMOP(ADD,R02,,I,) VMOP(ADD,R03,,I,)
VMOP(ADD,R10,,I,) VMOP(ADD,R11,,I,) VMOP(ADD,R12,,I,) VMOP(ADD,R1F,,I,)

VMOP(MUL,R11,R10,,)
VMOP(MUL,R12,R10,,)
VMOP(MUL,R13,R10,,)
VMOP(MUL,R14,R10,,)

VMOP(BEQ,R00,,I,O)
VMOP(BEQ,R01,,I,O)
VMOP(BEQ,R02,,I,O)
VMOP(BEQ,R07,,I,O)
VMOP(BEQ,R10,,I,O)
VMOP(BEQ,R11,,I,O)

VMOP(BNE,R00,,I,O) VMOP(BNE,R01,,I,O) VMOP(BNE,R02,,I,O) VMOP(BNE,R03,,I,O) VMOP(BNE,R05,,I,O) VMOP(BNE,R10,,I,O)

VMOP(BLT,R01,,I,O)

VMOP(BLT,R11,,I,O)

VMOP(BRA,,,,O)

VMOP(JMP,R00,,,) VMOP(JMP,R02,,,)

VMOP(JAL,R00,,,) VMOP(JAL,R02,,,)

VMOP(RET,,,,)

VMOP(SYS,,,I,)
VMOP(SYS,R00,,,)

VMOP(QUIT,,,,)
