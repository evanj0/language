using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace Interpreter
{
    public struct Frame
    {
        public Frame(int returnAddr, int basePtr)
        {
            ReturnAddr = returnAddr;
            BasePtr = basePtr;
        }
        public int ReturnAddr;

        public int BasePtr;
    }
}
