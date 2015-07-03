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
private static double test_fun(double a, double b)
{
return a + b + Math.pow(2.718282,2);
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


System.out.println(test_fun(1., 0.));

return ;
}
}

/* ERR : expressions in binary operators should be of equivalent type */
