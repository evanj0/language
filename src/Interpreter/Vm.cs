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
            if (Frames.Count > 0 && Frames.Last().BaseSp >= 0)
            {
                sb.AppendLine("Current Frame:");
                var currentFrame = Frames.Last();
                sb.AppendLine("  [Procedure Args]:");
                var baseIndex = currentFrame.BaseSp;
                for (var i = currentFrame.BaseSp; i < Stack.Sp; i++)
                {
                    if (i == currentFrame.ClosureArgsSp)
                    {
                        sb.AppendLine("  [Closure Args]:");
                        baseIndex = currentFrame.ClosureArgsSp;
                    }
                    if (i == currentFrame.LocalsSp)
                    {
                        sb.AppendLine("  [Locals]:");
                        baseIndex = currentFrame.LocalsSp;
                    }
                    sb.Append($"    [{i - baseIndex}]: ");
                    sb.Append($"{Stack.Data[i].Debug()}");
                    sb.AppendLine();
                }
            }
            else
            {
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

            }
            sb.AppendLine($"Call Stack:");
            sb.AppendLine($"  Frames: {Frames.Count}");
            if (Frames.TryPeek(out var frame))
            {
                sb.AppendLine($"    [0]: Return Address = {frame.ReturnAddr}, Base Stack Pointer = {frame.BaseSp}");
            }
            sb.AppendLine($"Instruction Pointer: {Ip}");
            return sb.ToString();
        }
    }
}
