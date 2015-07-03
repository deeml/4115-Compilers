import java.io.*;
import java.util.*;
import java.lang.*;

class program 
{
//input

//output

public static HashMap<String, ArrayList> VALUES_BY_NAME;
public static HashMap<String, String> TYPES_OF_VARS;
private static Random __rand = new Random(System.currentTimeMillis());
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
private static double gamma_sample(double alpha, double beta, double lambda)
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
Integer flag = 0;
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
while (__rand.nextFloat() > Math.pow(y * Math.pow(2.718282,1. - y),alpha - 1.)) {
y = 0 - Math.log(__rand.nextFloat());
}
gamma = alpha * y;
}
}
return beta * gamma + lambda;
}
private static class bivar
{
public static double x = 0.0;
public static double y = 0.0;

public double mean(double[] d) { double sum = 0.0;
for(double i : d) { sum += i; } return sum/((double) d.length);
}
public void run()
{
double[] res = new double[2];
double[] __x = new double[500];
x = __rand.nextFloat();
double[] __y = new double[500];
y = __rand.nextFloat();
for (int i=0; i<500; i++){
for (int j=0;j<10;j++){
 x = gamma_sample(3., y * y + 4., 0.);
y = 1. / x + 1 + random_sample() / Math.sqrt(2 * x + 2);
}
__x[i] = x;
__y[i] = y;
}
x = mean(__x);
y = mean(__y);

}
}
public static void main(String[] args){
VALUES_BY_NAME = new HashMap<String, ArrayList>();
TYPES_OF_VARS = new HashMap<String, String>();


String __csvFile = "input.csv";

    BufferedReader __br = null;

    String __line = "";
 
    String __csvSplitBy = ",";
 
    try {

    
    ArrayList<String> __variables = new ArrayList<String>();

    __br = new BufferedReader(new FileReader(__csvFile));

    int __j = 0;

    while ((__line = __br.readLine()) != null) {

              String[] __result = __line.split(__csvSplitBy);


              if (__j == 0)

              { 

                int __size = __result.length;


                for(int __i=0; __i<__size; ++__i)

                {

                  if (VALUES_BY_NAME.containsKey(__result[__i]))

                  {
                    __variables.add(__result[__i]);

                  }
                  else {__variables.add("");}


                  }

                ++__j;

                continue; 

    
              }

              
              for(int __i=0; __i < __variables.size(); ++__i) 

              {

                if (! __variables.get(__i).equals(""))

                    {

                    if(TYPES_OF_VARS.get(__variables.get(__i)).equals("Integer"))

                    {

                      VALUES_BY_NAME.get(__variables.get(__i)).add(Integer.parseInt(__result[__i]));

                    }

                    else if(TYPES_OF_VARS.get(__variables.get(__i)).equals("Double"))
                    {

                      VALUES_BY_NAME.get(__variables.get(__i)).add(Double.parseDouble(__result[__i]));

                    }

                    else if(TYPES_OF_VARS.get(__variables.get(__i)).equals("Boolean"))

                    {

                      VALUES_BY_NAME.get(__variables.get(__i)).add(Boolean.parseBoolean(__result[__i]));

                    }

                    else 

                    {

                      VALUES_BY_NAME.get(__variables.get(__i)).add(__result[__i]);

                    }

              }


         
      }

         
    }} catch (FileNotFoundException e) {

            e.printStackTrace();

            System.out.println("ProbL: Error - file not found.");

          } catch (IOException e) {

            //e.printStackTrace();

          } finally {

            if (__br != null) {

              try {

                __br.close();

              } catch (IOException e) {

              //e.printStackTrace();


              }

            }

          }


__rand = new Random(1234);
double x = 0.0;
double y = 0.0;
bivar __bivar = new program.bivar();
__bivar.run();
y = __bivar.y;
x = __bivar.x;
System.out.println(x);
System.out.println(y);

return ;
}
}

/* ERR : expressions in binary operators should be of equivalent type */
