using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interpreter
{
    public struct ProcInfo
    {
        public ProcInfo(int addr, int numArgs)
        {
            Addr = addr;
            NumArgs = numArgs;
        }

        public int Addr;

        public int NumArgs;
    }
}
