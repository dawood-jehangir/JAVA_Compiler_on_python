# JAVA Compiler on Python

This is a JAVA compiler made using lexical analysis in python. It is able to run and test simple java code.

## Getting Started

```
python java_compiler.py <arg1>
```
where
- arg1 = a text file containing the java code to be tested

For example
```
python java_compiler.py java_test.txt
```

### Prerequisites

You would also need to install the following libraries:
i. Ply

```
pip install ply
```

## Limitations & Future Work

1. Multiple Java function cannot be interpreted
2. Multiple arguments of functions cannot be interpreted
3. Only single if statement (including nested ones) can be correctly interpreted
4. Mutiple if-elseif-else statements (including nested ones) can be parsed but not interpreted
5. Array indexing not working in print statements => e.g. System.out.println(array[0]); 
6. For Loop is working fine. But it cannot interpret while loops.
