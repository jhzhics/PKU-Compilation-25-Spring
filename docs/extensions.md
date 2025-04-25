# 项目背景
侧重于前端.

## Motivation
编译器前端技术是当前主流Editor代码解析插件的核心部分，其中为性能考虑最为关键的部分是AST的动态实时生成. 同时由于代码编辑过程中大部分时候输入是不可接受状态，如果可以拓展语法使得可以接受这些不完整的程序并生成相应的AST，那么就可以提供更友好的代码帮助，同时和动态AST相配合.

# 项目设计

输入是 一个修改序列. 比如说 "int"
序列是[(append, 'i'), (append, 'n'), (append, 't')]
在每个输入处输出动态更新的AST

采用网络上的可用库或者手写LR(1)，或手写更新代码.这需要进一步调研

由于不太清楚实现难度，计划按照如下顺序进行.
1. 支持 只含加减法和ident的表达式,如`a + b - c + d`; APPEND 操作
2. DELETE 操作; INSERT 操作
3. 支持 一般的四则运算表达式 如 `a - (b - c + d) * e / f`
4. 拓展语法和AST支持不完整的表达式 如`a + b -`
5. 支持表达式外的其他语法

# 预期效果
通过一个视频演示上面的功能.
目前预期完成到第3步.

# 参考
[tree-sitter](https://tree-sitter.github.io/tree-sitter/index.html)
[hazel](https://github.com/hazelgrove/hazel)
