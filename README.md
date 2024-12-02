This library aims to transpile a Common Lisp live image to a standalone C++ program. It is mostly kludge and supports only a very small subset of the language. It should not be used for anything.

The transpiler makes heavy use of `CL:FUNCTION-LAMBDA-EXPRESSION`, so the implementation needs to support it more than what the underspecified standard requires. SBCL is the implementation I tested this on, and even then, programs need to be loaded with high debug optimization.

## Usage

As opposed to a regular source-to-source transpile, this library offers only a single `CK-LLI-CPP-TRANSPILER:TRANSPILE` function that accepts a symbol that represents a Lisp function that has a signature analogous to a C++ entry point, i.e., it should accept either two or zero arguments.

The transpiler then applies reflection against the live Lisp image to determine what other routines, variables, etc. the program uses, building a "minimally equivalent" C++ program.


Here's an example:

```
CL-USER> (ql:quickload :ck-lli-cpp-transpiler)
To load "ck-lli-cpp-transpiler":
  Load 1 ASDF system:
    ck-lli-cpp-transpiler
; Loading "ck-lli-cpp-transpiler"
[package ck-lli-cpp-transpiler]...........
(:CK-LLI-CPP-TRANSPILER)
CL-USER> (defun g ()
           (loop for i from 0 to 10
                 when (> i 5)
                   do (princ i)
                      (terpri)))
G
CL-USER> (defun main () (g) 0)
MAIN
CL-USER> (ck-lli-cpp-transpiler:transpile 'main)
; #<REQUIREMENT CK-LLI-CPP-TRANSPILER::IOSTREAM> requested by #<EXPRESSION-OP PRINC>
; Transpiled routine G from COMMON-LISP-USER
; Transpiled routine MAIN from COMMON-LISP-USER (entry point)
#include <iostream>

namespace ck_lli_cpp {  
  
  auto COMMON_LISP_USER_SUB290_G() {
    []() {
      {  
        auto ARG_G295 = []() {
          return 0;
        }();
        {  
          auto ARG_G291 = [&ARG_G295]() {
            return 0;
          }();
          auto ARG_G292 = [&ARG_G295]() {
            return 10;
          }();
          auto ARG_G293 = [&ARG_G295]() {
            return 1;
          }();
          {  
            auto ARG_G294 = [&ARG_G295, &ARG_G291, &ARG_G292, &ARG_G293]() {
              return ARG_G291;
            }();
            auto ARG_I = [&ARG_G295, &ARG_G291, &ARG_G292, &ARG_G293]() {
              return ARG_G291;
            }();
            /* Ignored operator DECLARE from COMMON-LISP */
            if (!(ARG_G294 <= ARG_G292)) {
              goto LABEL_END_LOOP300;
            }
            ARG_G294 = (ARG_G293 + ARG_G294);
            LABEL_G297299:
            {  
              auto ARG_G298 = [&ARG_G295, &ARG_G291, &ARG_G292, &ARG_G293, &ARG_G294, &ARG_I]() {
                return ARG_I > 5;
              }();
              if (ARG_G298) {
                {
                  [&ARG_G295, &ARG_G291, &ARG_G292, &ARG_G293, &ARG_G294, &ARG_I, &ARG_G298]() {
                    auto ARG_G301 = ARG_I;
                    std::cout << ARG_G301;
                    return ARG_G301;
                  }();
                  [&ARG_G295, &ARG_G291, &ARG_G292, &ARG_G293, &ARG_G294, &ARG_I, &ARG_G298]() {
                    std::cout << std::endl;
                    return nullptr;
                  }();
                }
              }
              else {
                (1 || 0);
              }
            }
            if (!(ARG_G294 <= ARG_G292)) {
              goto LABEL_END_LOOP300;
            }
            ARG_I = ARG_G294;
            ARG_G294 = (ARG_G293 + ARG_G294);
            goto LABEL_G297299;
            LABEL_END_LOOP300:
            return ARG_G295;
          }
        }
      }
    }();
  }
}

int main() {
  ck_lli_cpp::COMMON_LISP_USER_SUB290_G();
  return 0;
}
```

## Supported Lisp Features

The transpiler is both quite expansive and very limited. It is expansive by virtue of being able to reflect against the Lisp image, allowing it to expand macros and read routines, therefore only really requiring the implementation only of the most atomic parts of SBCL, and make use of just what it needs.

Limited, because, as of this moment, it only supports primitive programs. It was incrementally developed to allow me to transpile [simple Lisp programs such as these](https://github.com/daedsidog/bit-counter-benchmarks/blob/master/src/lisp/plb.lisp) to C++.

You can consult the source to see what operators are supported.
