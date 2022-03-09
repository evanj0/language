using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interpreter.Exceptions
{
    internal class VmExitException : Exception 
    {
        public VmExitException(int exitCode) : base($"Program exited with status code {exitCode}.") { }
    }

    internal class VmException : Exception
    {
        public VmException(string message) : base(message) { }
    }

    internal class StackPointerOutOfRangeException : VmException
    {
        public StackPointerOutOfRangeException() : base("Stack pointer was out of range.") { }
    }

    internal class InstructionPointerOutOfRangeException : VmException
    {
        public InstructionPointerOutOfRangeException() : base("Instruction pointer was out of range.") { }
    }

    internal class ProcedureDoesNotExistException : VmException
    {
        public ProcedureDoesNotExistException(int index) : base($"Procedure at index `{index}` does not exist.") { }
    }

    internal class BlockDoesNotExistException : VmException
    {
        public BlockDoesNotExistException(int index) : base($"Block at index `{index}` does not exist.") { }
    }

    internal class CallStackUnderflowException : VmException
    {
        public CallStackUnderflowException() : base("The call stack has underflowed.") { }
    }

    internal class CallStackOverflowException : VmException
    {
        public CallStackOverflowException(int count) : base($"The call stack has overflowed ({count} calls).") { }
    }

    internal class InstructionNotSupportedException : VmException
    {
        public InstructionNotSupportedException(string opName) : base($"Instruction `{opName}` is not supported.") { }
    }
}
