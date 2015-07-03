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
Integer i = 0;
i = 0;
double avg = 0.0;
avg = 0.;
while (i < 1000) {
avg = avg + random_sample();
i = i + 1;
}
System.out.println("close to zero");
System.out.println(avg / 1000);

return ;
}
}

/* ERR : expressions in binary operators should be of equivalent type */
