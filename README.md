# A course Lab SysY Compiler

This is a course project at Peking University.

# Optimation Passes
1. Generate basic SSA form for each basic block.
    - Use a virtual register for each local alloc i32
    - Do local variable numbering for each assignment in the block
    - There kinds of virtual registers are left:
        1. k_{}: local variable
        2. t_{}: temporary variable
        3. g_{}: global variable(store the address of global variable)
    - It is asserted that only k_{} and g_{} registers will need to be added to block parameters in step2.
    - In some sense, global variables are deemed as arguments of the function.
    - Use `la` to load all global addresses at the beginning of the function

2. Generate global SSA form by active varaibles analysis across basic blocks.
    - Use a 2-pass approach.
        1. Do active variable scan for each basic block
        2. Do block parameter elimination until it converges
3. Do constant propagation and folding and dead code elimination using DAG on SSA block

## Material
[Course Website](https://pku-minic.github.io/online-doc/#/)

[Test Dataset](https://gitlab.eduxiji.net/csc1/nscscc/compiler2024/-/tree/main)