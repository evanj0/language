using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interpreter
{
    public interface IVmOutput
    {
        public void WriteLine(string s);

        public void Write(string s);

        public void WriteBytes(byte[] bytes);
    }

    public class ConsoleOutput : IVmOutput
    {
        private readonly Stream _stream = Console.OpenStandardOutput();

        public void WriteLine(string s)
        {
            Console.WriteLine(s);
        }

        public void Write(string s)
        {
            Console.Write(s);
        }

        public void WriteBytes(byte[] bytes)
        {
            _stream.Write(bytes);
        }
    }

}
