# A course Lab SysY Compiler

This is a course project at Peking University.

# Optimation Passes
1. Generate basic SSA form for each basic block.
    - Use a virtual register for each local alloc i32
    - There kinds of virtual registers are left:
        1. k_{}: local variable
        2. t_{}: temporary variable
        3. g_{}: global variable(store the address of global variable)
    - It is asserted that only k_{} and g_{} registers will need to be added to block parameters in step2.
    - In some sense, global variables are deemed as arguments of the function.
    - Use `la` to load all global addresses at the beginning of the function

2. Do local optimization on SSA Basic Block
    1. constant folding
    2. common subexpression elimination
    3. replication elimination
    4. dead code elimination

3. Generate global SSA form by active varaibles analysis across basic blocks.
    - Use a 2-pass approach.
        1. Do active variable scan for each basic block
        2. Do block parameter elimination until it converges

4. Convert SSA form to Basic RV form by substituting virtual Call instructions with real Call instructions.
   But we still use virtual registers to represent local variables.

5. Do register allocation using graph coloring. Add stack allocation and deallocation instructions.
    - Provide hints for register merging

6. Add prologue and epilogue and do a final peephole optimization and dead code elimination.

7. Generate final RV code. (Remove all consecutive `j` instruction with block next to it)

# Performance Evaluation
In PKU 2025 performance evaluation, we get 180.42s.

## Material
[Course Website](https://pku-minic.github.io/online-doc/#/)

[Test Dataset](https://gitlab.eduxiji.net/csc1/nscscc/compiler2024/-/tree/main)