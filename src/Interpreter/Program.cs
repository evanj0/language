using Interpreter;
using Interpreter.Exceptions;
using System.Runtime.Intrinsics.X86;

var program = new Op[]
{
    // header:
    new Op(OpCode.SetIp, Word.FromI32(1)),

    // setup:
    new Op(OpCode.NoOp, Word.Zero()),
    new Op(OpCode.Call, Word.Ptr(0)),
    new Op(OpCode.Exit, Word.FromI32(0)),

    // proc 0:
    new Op(OpCode.NoOp, Word.Zero()),
    new Op(OpCode.PushI64, Word.FromI64(1)),
    new Op(OpCode.PushI64, Word.FromI64(2)),
    new Op(OpCode.AddI64, Word.Zero()),
    new Op(OpCode.Return, Word.Zero()),
};

var procTable = new int[]
{
    4, // proc 0
};

var vm = new Vm()
{
    Ip = 0,
    Stack = new ValueStack(),
    Frames = new Stack<Frame>(),
};

try
{
    while (true)
    {
        if (vm.Ip >= program.Length)
        {
            throw new InstructionPointerOutOfRangeException();
        }

        var inst = program[vm.Ip];

        switch (inst.OpCode)
        {
            case OpCode.Exit:
                throw new VmExitException(inst.Data.ToI32());

            case OpCode.NoOp:
                break;

            case OpCode.SetIp:
                vm.Ip = inst.Data.ToI32();
                break;

            case OpCode.Call:
                var proc = inst.Data.ToI32();
                if (proc >= procTable.Length)
                {
                    throw new ProcedureDoesNotExistException(proc);
                }
                vm.Frames.Push(new Frame(vm.Ip, vm.Stack.Sp));
                vm.Ip = procTable[proc];
                break;

            case OpCode.Return:
                if (vm.Frames.Count == 0)
                {
                    throw new ProcedureStackUnderflowException();
                }
                var frame = vm.Frames.Pop();
                vm.Ip = frame.ReturnAddr;
                break;

            case OpCode.PushI64:
                vm.Stack.Push(inst.Data);
                break;

            case OpCode.AddI64:
                var val1 = vm.Stack.Pop().ToI64();
                var val2 = vm.Stack.Pop().ToI64();
                vm.Stack.Push(Word.FromI64(val1 + val2));
                break;

            case OpCode.Dump:
                Console.WriteLine(vm.DebugState());
                break;

            default: 
                throw new InstructionNotSupportedException(inst.OpCode.ToUserString());
        }

        vm.Ip++;
    }
}
catch (VmException e)
{
    Console.WriteLine($"Runtime Error: {e.Message}");
    Console.WriteLine("Debugging Info:");
    Console.WriteLine(vm.DebugState());
}
catch (VmExitException e)
{
    Console.WriteLine(e.Message);
}