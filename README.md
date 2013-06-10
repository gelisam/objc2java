objc2java
=========

Convert pure ObjectiveC code to pure Java code, libraries be damned.

My goals are similar to that of [java2objc](https://code.google.com/p/java2objc/):

* Attempts to create well-crafted Java code as if it was written by hand.
* The generated code is not likely to have the exact same behavior and may not even compile. The generated code is a mere suggestion. A human should look at the generated code and tweak it manually.

(in development)


usage
-----

    > echo "[[Hello alloc] init];" | objc2java
    new Hello();


status
------

Currently, the objc2java executable can filter an ObjectiveC source file and substitute Java-style calls where appropriate. Example output:

    > make demo
    // First program example
    
    #import <Foundation/Foundation.h>
    
    int main (int argc, const char * argv[])
    {
      NSAutoreleasePool *pool = new NSAutoreleasePool();
      NSLog("Hello, World!");
      pool.drain();
      return 0;
    }
