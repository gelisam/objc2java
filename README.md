objc2java
=========

Convert pure ObjectiveC code to pure Java code, libraries be damned.


usage
-----

Pipe an ObjectiveC file into `objc2java` and we will convert what we can.

    > cat tests/hello.m | objc2java
    // First program example
    
    #import <Foundation/Foundation.h>
    
    int main (int argc, const char * argv[])
    {
      NSAutoreleasePool *pool = new NSAutoreleasePool();
      NSLog("Hello, World!");
      pool.drain();
      return 0;
    }

For comparison, the original ObjectiveC file looked like this.

    > cat tests/hello.m
    // First program example
    
    #import <Foundation/Foundation.h>
    
    int main (int argc, const char * argv[])
    {
      NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
      NSLog (@"Hello, World!");
      [pool drain];
      return 0;
    }


purpose
-------
The goal is to help programmers port ObjectiveC code to Java by automating the boring parts. We help, but we don't do all the work: the generated code won't compile, and will still contain a lot of ObjectiveC idiosyncrasies. The idea is to let the programmer focus on translating those idiosyncrasies instead of wasting time endlessly translating `[object method]` into `object(method)`.


status
------

So far, only the method call syntax is converted.


future work
-----------

Coming up next: customizable idiom translation (i.e. `[[AAA alloc] init]` becomes `new AAA()`).

After that: method signatures.

By the way, since the parser is based on [invertible-syntax](http://hackage.haskell.org/package/invertible-syntax), it would not be a lot of work to do the translation in the opposite direction, i.e., converting the syntax from Java to ObjectiveC. But there is already a tool for that, called [java2objc](https://code.google.com/p/java2objc/).
