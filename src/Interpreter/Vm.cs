using Interpreter.Exceptions;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interpreter
{
    public ref struct Vm
    {
        public ValueStack Stack;
        public Stack<Frame> Frames;
        public int Ip;

        public string Debug()
        {
            var sb = new StringBuilder();
            sb.AppendLine("Value Stack:");
            sb.AppendLine($"  Stack Pointer: {Stack.Sp}");
            try
            {
                var s = Stack.Peek().Debug();
                sb.AppendLine($"    [0]: {s}");
            }
            catch (VmException) { }
            try
            {
                var s = Stack.Peek(1).Debug();
                sb.AppendLine($"    [1]: {s}");
            }
            catch (VmException) { }
            try
            {
                var s = Stack.Peek(2).Debug();
                sb.AppendLine($"    [2]: {s}");
            }
            catch (VmException) { }
            try
            {
                var s = Stack.Peek(3).Debug();
                sb.AppendLine($"    [3]: {s}");
            }
            catch (VmException) { }
            sb.AppendLine($"Call Stack:");
            sb.AppendLine($"  Frames: {Frames.Count}");
            if (Frames.TryPeek(out var frame))
            {
                sb.AppendLine($"    [0]: Return Address = {frame.ReturnAddr}, Base Stack Pointer = {frame.BasePtr}");
            }
            sb.AppendLine($"Instruction Pointer: {Ip}");
            return sb.ToString();
        }
    }
}
