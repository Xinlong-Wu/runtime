using System;
namespace HelloWorld
{
  class Program
  {
    static int Main()
    {
      Console.WriteLine($"IsMono: {Type.GetType("Mono.RuntimeStructs") != null}");
      Console.WriteLine("Hello RISC-V");
      return 0;
    }
  }
}
