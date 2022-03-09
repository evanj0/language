using Interpreter;
using Interpreter.Exceptions;
using System.Runtime.Intrinsics.X86;

var program = new Op[]
{
    // section code
    // header:
    new Op(OpCode.SetIp, Word.FromI32(1)), // 0

    // setup:
    new Op(OpCode.NoOp), // 1
    new Op(OpCode.Call, Word.Ptr(0)),
    new Op(OpCode.Exit, Word.FromI32(0)),

    // section proc
    // proc main:
    new Op(OpCode.NoOp), // 4
    new Op(OpCode.PushI64, Word.FromI64(1000)),
    new Op(OpCode.Call, Word.Ptr(1)),
    new Op(OpCode.SkipIfFalse, Word.Ptr(3)),
        // then:
        new Op(OpCode.PushI64, Word.FromI64(69)),
        new Op(OpCode.Dump),
    new Op(OpCode.Skip, Word.Ptr(2)),
        // else:
        new Op(OpCode.PushI64, Word.FromI64(420)),
        new Op(OpCode.Dump),
    new Op(OpCode.Return),

    // proc 1:
    new Op(OpCode.NoOp), // 14
    new Op(OpCode.PushI64, Word.FromI64(1000)),
    new Op(OpCode.CmpEqI64),
    new Op(OpCode.Return),
};

var procTable = new int[]
{
    4, // proc main
    14, // proc 1
};

var vm = new Vm()
{
    Ip = 0,
    Stack = new ValueStack(),
    Frames = new Stack<Frame>(),
};

var maxStack = 65_536 * 8;

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
            // Control Flow

            case OpCode.Exit:
                throw new VmExitException(inst.Data.ToI32());

            case OpCode.NoOp:
                break;

            case OpCode.SetIp:
                vm.Ip = inst.Data.ToI32();
                break;

            case OpCode.Call:
                {
                    var proc = inst.Data.ToI32();
                    if (proc >= procTable.Length)
                    {
                        throw new ProcedureDoesNotExistException(proc);
                    }
                    if (vm.Frames.Count >= maxStack)
                    {
                        throw new CallStackOverflowException(vm.Frames.Count);
                    }
                    vm.Frames.Push(new Frame(vm.Ip, vm.Stack.Sp));
                    vm.Ip = procTable[proc];
                    break;
                }

            case OpCode.Return:
                if (vm.Frames.Count == 0)
                {
                    throw new CallStackUnderflowException();
                }
                var frame = vm.Frames.Pop();
                vm.Ip = frame.ReturnAddr;
                break;

            // bool -> 
            case OpCode.SkipIfFalse:
                if (vm.Stack.Pop().ToBool() == false)
                {
                    vm.Ip += inst.Data.ToI32();
                }
                break;

            case OpCode.Skip:
                vm.Ip += inst.Data.ToI32();
                break;

            // Debugging

            case OpCode.Dump:
                Console.WriteLine(vm.Debug());
                break;

            // Math

            //  -> i64
            case OpCode.PushI64:
                vm.Stack.Push(inst.Data);
                break;

            //  -> bool
            case OpCode.PushBool:
                vm.Stack.Push(inst.Data);
                break;

            // i64 i64 -> i64
            case OpCode.AddI64:
                {
                    var val1 = vm.Stack.Pop().ToI64();
                    var val2 = vm.Stack.Pop().ToI64();
                    vm.Stack.Push(Word.FromI64(val1 + val2));
                    break;
                }

            // i64 i64 -> bool
            case OpCode.CmpEqI64:
                { 
                    var val1 = vm.Stack.Pop().ToI64();
                    var val2 = vm.Stack.Pop().ToI64();
                    vm.Stack.Push(Word.FromBool(val1 == val2));
                    break;
                }

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
    Console.WriteLine(vm.Debug());
}
catch (VmExitException e)
{
    Console.WriteLine(e.Message);
}