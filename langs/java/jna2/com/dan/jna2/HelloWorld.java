package com.dan.jna2;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;

/** Simple example of JNA interface mapping and usage. */
public class HelloWorld {

  // This is the standard, stable way of mapping, which supports extensive
  // customization and mapping of Java to native types.
  public interface MyLibrary extends Library {
    MyLibrary INSTANCE = (MyLibrary)
      Native.loadLibrary(("my_lib"), MyLibrary.class);

    int foo();
  }

  public static void main(String[] args) {
//    System.setProperty("jna.Library.path", 
//        "/Users/daniel/sandbox/java/jna2/jna_so/build/apple-release/");
    System.out.println("Calling foo...");
    int result = MyLibrary.INSTANCE.foo();
    System.out.print("result = ");
    System.out.println(result);
  }
}

