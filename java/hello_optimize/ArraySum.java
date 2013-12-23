import java.util.Scanner;

public class ArraySum {
  public static float asum(float[] xs) {
    float ret = 0;
    for(int i = 0; i < xs.length; ++i)
        ret += xs[i];
    return ret;
  }
  public static void main(String[] args) {
    System.out.println("ArraySum...");
    //Scanner in = new Scanner(System.in);
    // int a = in.nextInt();
    //
    float[] array = new float[Integer.parseInt(args[0])];
    for (int i = 0; i < array.length; ++i) {
      array[i] = 1;
    }
    System.out.println(asum(array));

  }
}
