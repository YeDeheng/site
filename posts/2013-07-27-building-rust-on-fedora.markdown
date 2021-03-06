---
title: Building Rust on Fedora
date: 2013-07-27 12:10:23
tags: rust, fedora, build
---
## What is Rust? ##
[Rust](http://rust-lang.org) is a new programming language headed by [Mozilla](http://mozilla.org), the same people bringing you the [Firefox browser](http://firefox.com).

## Getting Started ##
According to [Rust's wiki](https://github.com/mozilla/rust/wiki/Note-getting-started-developing-Rust), you'll need the following to build the compiler.

1. A recent Linux, OS X 10.6 or later, Win32 or FreeBSD system
2. 1.6 GiB RAM available for the build process (see note below about memory usage)
3. Python 2.x (version 2.7 is known to work)
4. GNU make 3.81
5. git 1.7
6. g++ 4.4 at least on Linux, 4.5 on Windows, and the 4.x gcc in Apple's SDK for OS X.
7. curl
8. Valgrind 3.5 or later (recommended, but not required for Linux)
9. pandoc 1.8 at least (optional, if you wish to build HTML docs)
10. pdflatex (optional, if you wish to build PDF docs)
11. ccache (optional)

Here is the command to get all of the above.

```bash
sudo yum install -y python make git gcc-c++ curl valgrind pandoc texlive-latex ccache
```

## Download and Build Rust ##
```bash
git clone git://github.com/mozilla/rust.git
cd rust
./configure --enable-ccache # this may take a while if this is your first time, as it downloads LLVM
make # this will download stage-0 Rust compiler in order to compile the Rust source
```

If the process runs smoothly, you will have the Rust compiler and toolchain residing in `<archtecture>/stage2/bin` (on my machine, it's `x86_64-unknown-linux-gnu/stage2/bin`).
