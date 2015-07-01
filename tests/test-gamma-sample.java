import java.util.*;
import java.lang.*;

class program 
{
//input

//output

private static Random __rand = new Random(System.currentTimeMillis());
private static double gamma_sample(double alpha,double beta,double lambda)
{
double gamma = 0.0;
gamma = 0.;
if (alpha <= 0. || beta <= 0.)
{
return 0 - 1.;
System.out.println("alpha and beta must be positive.");
}
else
{
}
if (alpha < 1.)
{
while (flag == 0) {
if (p > 1.)
{
if (__rand.nextFloat() <= Math.pow(gamma,alpha - 1.))
{
flag = 1;
}
else
{
}
gamma = 0 - Math.log(b - p / alpha);
}
else
{
if (__rand.nextFloat() <= Math.pow(2.718282,0 - gamma))
{
flag = 1;
}
else
{
}
gamma = Math.pow(p,1. / alpha);
}
p = b * __rand.nextFloat();
}
b = 1 + alpha * 0.3678794;
flag = 0;
int flag = 0;
double p = 0.0;
double b = 0.0;
}
else
{
if (alpha == 1.)
{
gamma = 0 - Math.log(__rand.nextFloat());
}
else
{
gamma = alpha * y;
while (__rand.nextFloat() > Math.pow(y * 1. - y,alpha - 1.)) {
y = 0 - Math.log(__rand.nextFloat());
}
y = 0 - Math.log(__rand.nextFloat());
double y = 0.0;
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
int i = 0;
i = 0;
double avg = 0.0;
avg = 0.;
while (i < 1000) {
i = i + 1;
avg = avg + gamma_sample(3., 3., 1.);
}
System.out.println("close to 10");
System.out.println(avg / 1000);

return ;
}
}
