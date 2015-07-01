import java.util.*;
import java.lang.*;

class program 
{
//input

//output

private static Random __rand = new Random(System.currentTimeMillis());
private static class bivar
{
public static double x = 0.0;
public static double y = 0.0;

public static double mean(double[] d) { double sum = 0.0;
for(double i : d) { sum += i; } return sum/((double) d.length);
}
public static double[] run()
{
double[] res = new double[2];
double[] __x = new double[5000];
x = __rand.nextFloat();
double[] __y = new double[5000];
y = __rand.nextFloat();
for (int i=0; i<5000; i++){
for (int j=0;j<100;j++){
 x = gamma_sample(3., y * y + 4., 0.);
y = random_sample(/*1. / x + 1, 1. / Math.sqrt(2 * x + 2)*/);
}
__x[i] = x;
__y[i] = y;
}
x = mean(__x);
y = mean(__y);
res[0] = x;
res[1] = y;
return res;
}
}private static double gamma_sample(double alpha, double beta, double lambda)
{
double gamma = 0.0;
gamma = 0.;
if (alpha <= 0. || beta <= 0.)
{
System.out.println("alpha and beta must be positive.");
return 0 - 1.;
}
if (alpha < 1.)
{
double b = 0.0;
double p = 0.0;
int flag = 0;
flag = 0;
b = 1 + alpha * 0.3678794;
while (flag == 0) {
p = b * __rand.nextFloat();
if (p > 1.)
{
gamma = 0 - Math.log(b - p / alpha);
if (__rand.nextFloat() <= Math.pow(gamma,alpha - 1.))
{
flag = 1;
}
}
else
{
gamma = Math.pow(p,1. / alpha);
if (__rand.nextFloat() <= Math.pow(2.718282,0 - gamma))
{
flag = 1;
}
}
}
}
else
{
if (alpha == 1.)
{
gamma = 0 - Math.log(__rand.nextFloat());
}
else
{
double y = 0.0;
y = 0 - Math.log(__rand.nextFloat());
while (__rand.nextFloat() > Math.pow(y * 1. - y,alpha - 1.)) {
y = 0 - Math.log(__rand.nextFloat());
}
gamma = alpha * y;
}
}
return beta * gamma + lambda;
}
private static double random_sample()
{
double u = 0.0;
u = __rand.nextFloat();
if (u < 0.5)
{
return Math.sqrt(0 - 0.5 * 3.141593 * Math.log(1 - Math.pow(2 * u - 1,2)));
}
else
{
return 0 - Math.sqrt(0 - 0.5 * 3.141593 * Math.log(1 - Math.pow(1 - 2 * u,2)));
}
}

public static void main(String[] args){
__rand = new Random(1234);
double[] res = bivar.run();
System.out.println(res[0]);
System.out.println(res[1]);

return ;
}
}
