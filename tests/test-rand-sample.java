import java.util.*;
import java.lang.*;

class program 
{
//input

//output

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

public static void main(String[] args){
__rand = new Random(1234);
int i = 0;
i = 0;
double avg = 0.0;
avg = 0.;
while (i < 1000) {
i = i + 1;
avg = avg + random_sample();
}
System.out.println("close to zero");
System.out.println(avg / 1000);

return ;
}
}
