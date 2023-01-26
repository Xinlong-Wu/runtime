using System;

namespace HelloWorld
{
  class Program
  {
    static int Main(string[] args)
  	{
    //   Console.WriteLine("Hello");
      return test(args);
    }
		
		static int test(string[] args){
			Random rd = new Random();
			if(args.Length > 0)
				return rd.Next(0,10);
			else
				return 0;
		}
  }
}