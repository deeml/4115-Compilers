import java.io.*;
import java.util.*;
import java.lang.*;

class program 
{
//input
public static Integer[] a = new Integer[2];
public static Double[] b = new Double[2];
public static Boolean[] c = new Boolean[2];
public static String[] d = new String[2];

//output

public static HashMap<String, ArrayList> VALUES_BY_NAME;
public static HashMap<String, String> TYPES_OF_VARS;
private static Random __rand = new Random(System.currentTimeMillis());

public static void main(String[] args){
VALUES_BY_NAME = new HashMap<String, ArrayList>();
TYPES_OF_VARS = new HashMap<String, String>();
VALUES_BY_NAME.put("a", new ArrayList<Integer >(Arrays.asList(a)));
VALUES_BY_NAME.put("b", new ArrayList<Double >(Arrays.asList(b)));
VALUES_BY_NAME.put("c", new ArrayList<Boolean >(Arrays.asList(c)));
VALUES_BY_NAME.put("d", new ArrayList<String >(Arrays.asList(d)));

TYPES_OF_VARS.put("a", "Integer");
TYPES_OF_VARS.put("b", "Double");
TYPES_OF_VARS.put("c", "Boolean");
TYPES_OF_VARS.put("d", "String");

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

a = (Integer[]) VALUES_BY_NAME.get("a").toArray(new Integer[0]);
b = (Double[]) VALUES_BY_NAME.get("b").toArray(new Double[0]);
c = (Boolean[]) VALUES_BY_NAME.get("c").toArray(new Boolean[0]);
d = (String[]) VALUES_BY_NAME.get("d").toArray(new String[0]);


return ;
}
}
