using Interpreter.Memory;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interpreter.Exceptions
{
    public class VmExitException : Exception 
    {
        public VmExitException(int exitCode) : base($"Program exited with status code {exitCode}.") { }
    }

    public class VmException : Exception
    {
        public VmException(string message) : base(message) { }
    }

    public class StackPointerOutOfRangeException : VmException
    {
        public StackPointerOutOfRangeException() : base("Stack pointer was out of range.") { }
    }

    public class InstructionPointerOutOfRangeException : VmException
    {
        public InstructionPointerOutOfRangeException() : base("Instruction pointer was out of range.") { }
    }

    public class ProcedureDoesNotExistException : VmException
    {
        public ProcedureDoesNotExistException(int index) : base($"Procedure at index `{index}` does not exist.") { }
    }

    public class BlockDoesNotExistException : VmException
    {
        public BlockDoesNotExistException(int index) : base($"Block at index `{index}` does not exist.") { }
    }

    public class CallStackUnderflowException : VmException
    {
        public CallStackUnderflowException() : base("The call stack has underflowed.") { }
    }

    public class CallStackOverflowException : VmException
    {
        public CallStackOverflowException(int count) : base($"The call stack has overflowed ({count} calls).") { }
    }

    public class InstructionNotSupportedException : VmException
    {
        public InstructionNotSupportedException(string opName) : base($"Instruction `{opName}` is not supported.") { }
    }


    // Heap

    public class VmHeapException : Exception
    {
        public HeapPointer Pointer { get; }

        public VmHeapException(string message, HeapPointer pointer) : base(message)
        {
            Pointer = pointer;
        }
    }

    public class InvalidPointerException : VmHeapException
    {
        public InvalidPointerException(HeapPointer pointer) : base($"Pointer `{pointer.Debug()}` does not point to a valid memory location.", pointer) { }
    }

    public class TypeMismatchException : VmHeapException
    {
        public TypeMismatchException(ReferenceType expected, ReferenceType actual, HeapPointer pointer) 
            : base($"Expected type `{expected}` at location `{pointer.Debug()}`, but found type `{actual}`.", pointer) { }
    }
}
