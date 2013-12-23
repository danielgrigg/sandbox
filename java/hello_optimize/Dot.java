import java.util.Scanner;

public class Dot {
  public static double asum(double[] as, double[] bs) {
    double sum = 0.0;
    for (int i = 0; i < as.length; ++i) {
      sum += as[i] * bs[i];
    }
    return sum;
  }
  public static void main(String[] args) {
    for (int i = 0; i < 10000000; ++i) {
      double[] as = new double[] { 1.0, 2.0, 3.0, 4.0 };
      double[] bs = new double[] { 1.0, 2.0, 3.0, 4.0 };
      asum(as, bs);
    }
  }
}
