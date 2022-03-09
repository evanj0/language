using Interpreter.Exceptions;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interpreter
{
    public struct Vm
    {
        public ValueStack Stack;
        public Stack<Frame> Frames;
        public int Ip;

        public string DebugState()
        {
            var sb = new StringBuilder();
            sb.AppendLine($"Stack Pointer: {Stack.Sp}");
            try
            {
                var s = Stack.Peek().ToI64();
                sb.AppendLine($"    [0]: {s}");
            }
            catch (VmException) { }
            try
            {
                var s = Stack.Peek(1).ToI64();
                sb.AppendLine($"    [1]: {s}");
            }
            catch (VmException) { }
            try
            {
                var s = Stack.Peek(2).ToI64();
                sb.AppendLine($"    [2]: {s}");
            }
            catch (VmException) { }
            try
            {
                var s = Stack.Peek(3).ToI64();
                sb.AppendLine($"    [3]: {s}");
            }
            catch (VmException) { }
            sb.AppendLine($"Instruction Pointer: {Ip}");
            return sb.ToString();
        }
    }
}
