---
title: Hello, JIT World from D
tags: just-in-time, d programming language, beginner
date: 2013-01-15 00:00:00
---
_This post is inspired by [Hello, JIT World: The Joy of Simple JITs](http://blog.reverberate.org/2012/12/hello-jit-world-joy-of-simple-jits.html), especially the first short program that he used to demonstrate that JIT programming can be simple. In this post, I will convert the program he wrote in C to [D][wiki] to show some capabilities and convenience of D._

What follows is the whole source of the program in D. You can view the original source in C in the blog post linked above.

```d
import std.c.string;
import std.c.linux.linux;
import std.conv;
import std.stdio;

int main(string[] args)
{
    // Machine code for:
    //   mov eax, 0
    //   ret
    ubyte code[] = [0xb8, 0x00, 0x00, 0x00, 0x00, 0xc3];

    if (args.length < 2)
    {
        stderr.writeln("Usage: jit1 <integer>");
        return 1;
    }

    // Overwrite immediate value "0" in the instruction
    // with the user's value.  This will make our code:
    //   mov eax, <user's value>
    //   ret
    int num = to!int(args[1]);
    memcpy(&code[1], &num, 4);

    // Allocate writable/executable memory.
    // Note: real programs should not map memory both writable
    // and executable because it is a security risk.
    void* mem = mmap(null, code.sizeof, PROT_WRITE | PROT_EXEC,
            MAP_ANON | MAP_PRIVATE, -1, 0);
    memcpy(mem, code, code.sizeof);

    // The function will return the user's value.
    int function() func = cast(int function())mem;
    return func();
}
```

Let's go through some first impressions.

D programming language is in the same family of languages that have the C flavor, so you might already find the code familiar and quite easy to read. Some familiarities are

* double forward slashes starts a line comment

```d
// a line comment which extends only to the end of the line
```

* curly braces around a block of code

```d
{
    // block of code
}
```

* every statement ends in a semicolon

```d
statement; // assuming you have statement defined
```

* type goes before the identifier in a declaration

```d
int a;
long b;
```

* all executions start from the `main` function which can have one of the following signatures

```d
int main();
int main(string[]);
void main();
void main(string[]);
```

* and unlike C (or C++ for that matter), the D standard actually have `void main(...)` functions return 0 (which means success) to the shell.
* indexing operator is `[]`

```d
int[2] a;
a[1] = 1; // use this to index into the second element of a
```

* address-of operator is `&`

```d
int a;
int* b = &a;
```

* from the above example you can also see that there are pointer types in D and the syntax for declaring one is just like C's
* there's actually a `null` value for pointers, not just a cast from zero as in C
* bitwise operators in D are the same as in C, e.g. `|`, `&` and `~`
* you can cast from one time to another, which is not recommended unless you know exactly what you are doing as the safe subset of D is powerful enough for most applications, by doing

```d
void* a;
char* b = cast(char*)a;
```

* syntax for returning from a function is

```d
return expression; // where expression is any expression, e.g. 1 + 1
```

So D is quite similar to C/C++. But the differences that set D apart is even more interesting.

* there is a concept of modules that can `import` each other, so that a source file does not have to be processed so many times in compilation (well, there's more to this but the reduction in compilation time that comes with this is, to me, the most significant benefit)
* string is actually supported, not something that feels bolted on like in C (or C++)
* there's no `unsigned` keyword. Most types have their unsigned counterparts named with a `u` prepended.
* arrays know their own lengths, which can be accessed by doing

```d
int[200] a;
writeln(a.length); // assuming you have `import std.stdio;` before this.
// Prints out 200.
```

* `to` in `to!int` is a generic function (and you specialize it using the bang-type syntax). The syntax, though unfamiliar, but greatly simplifies parsing, both for compilers and for humans.
* C code can be called directly from D after creating declarations for it. The function `memcpy` and `mmap` are C functions whose declarations are defined in packages `string` and `linux`.
* the size of some variable in bytes can be obtained from doing `variable.sizeof`
* any pointer type is automatically convertible to `void*`, not so for the converse.
* there is a `function` keyword so that the type of a function pointer or complex types can be declared without hurting your brain

```d
// fp is a pointer to a function that takes a function taking a char
// returning an int and returns a typeless pointer
// (try saying that five times fast)
void* function(int function(char)) fp;
// the equivalent for that in C is
// void* (*fp)(int (*)(char));

// let's look at something more interesting
// signal is a function that takes an int and a pointer to a function
// taking an int returning nothing and returns a pointer to a function
// taking an int returning nothing
void function(int) signal(int sig, void function(int) func));
// the equivalent for that in C is
// void (*signal (int sig, void (*func)(int)) )(int);
// try reading that five times fast :)
```

Those are the differences that are present in this code snippet, you can check out more about D at the [D home page](http://dlang.org) or a summary at the [wikipedia page][wiki].

[wiki]: http://en.wikipedia.org/wiki/D_(programming_language)
