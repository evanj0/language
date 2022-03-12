using Interpreter;
using Interpreter.Exceptions;
using Interpreter.Memory;
using System.Diagnostics;
using static Interpreter.Interpreter;

var program1 = new Op[]
{
    // section code
    // header:
    new Op(OpCode.IpSet, Word.FromI32(1)), // 0

    // setup:
    new Op(OpCode.NoOp), // 1
    new Op(OpCode.Call, Word.Ptr(0)),
    new Op(OpCode.Exit, Word.FromI32(0)),

    // section proc
    // proc main:
    new Op(OpCode.NoOp), // 4
    new Op(OpCode.I64Push, Word.FromI64(1000)),
    new Op(OpCode.Call, Word.Ptr(1)),
    new Op(OpCode.JumpIfFalse, Word.Ptr(3)),
        // then:
        new Op(OpCode.I64Push, Word.FromI64(69)),
        new Op(OpCode.DebugDump),
        new Op(OpCode.Jump, Word.Ptr(2)),
        // else:
        new Op(OpCode.I64Push, Word.FromI64(420)),
        new Op(OpCode.DebugDump),
    new Op(OpCode.Return),

    // proc 1:
    new Op(OpCode.NoOp), // 14
    new Op(OpCode.NoOp),


    new Op(OpCode.RecordAlloc, Word.Ptr(128)),

    new Op(OpCode.LocalLoad, Word.Ptr(1)),
    new Op(OpCode.I64Push, Word.FromI64(523)),
    new Op(OpCode.RecordSetField, Word.Ptr(0)),

    new Op(OpCode.LocalLoad, Word.Ptr(1)),
    new Op(OpCode.I64Push, Word.FromI64(23476)),
    new Op(OpCode.RecordSetField, Word.FromI64(1)),

    new Op(OpCode.LocalLoad, Word.Ptr(1)),
    new Op(OpCode.RecordGetField, Word.Ptr(0)),

    new Op(OpCode.LocalLoad, Word.Ptr(1)),
    new Op(OpCode.RecordGetField, Word.Ptr(1)),

    new Op(OpCode.DebugDump),


    new Op(OpCode.I64Push, Word.FromI64(1000)),
    new Op(OpCode.LocalLoad, Word.Ptr(0)), // local 0
    new Op(OpCode.I64CmpEq),
    new Op(OpCode.Call, Word.Ptr(1)),
    new Op(OpCode.Return),
};

var procTable1 = new ProcInfo[]
{
    new ProcInfo() { Addr = 4, NumArgs = 0 }, // proc main
    new ProcInfo() { Addr = 14, NumArgs = 1}, // proc 1
};

var program2 = new Op[]
{
    // section code
    // header:
    new Op(OpCode.IpSet, Word.FromI32(1)), // 0

    // setup:
    new Op(OpCode.NoOp), // 1
    new Op(OpCode.RecordAlloc, Word.Ptr(0)), // (record-alloc 0)
    new Op(OpCode.RecordAlloc, Word.Ptr(0)), // (record-alloc 0)
    new Op(OpCode.RecordAlloc, Word.Ptr(0)), // (record-alloc 0)
    new Op(OpCode.Call, Word.Ptr(0)), // (call 'main')
    new Op(OpCode.ClosureApply), // (closure-apply)
    new Op(OpCode.Exit, Word.FromI32(0)),

    // section proc
    // proc main:
    new Op(OpCode.NoOp), // 8
    new Op(OpCode.ClosureAlloc, new Word().SetU32(0, value: 1).SetU16(4, value: 2)),

    new Op(OpCode.LocalLoad, Word.Ptr(0)),
    new Op(OpCode.I64Push, Word.FromI64(69)),
    new Op(OpCode.ClosureSetArg, Word.Ptr(0)),

    new Op(OpCode.LocalLoad, Word.Ptr(0)),
    new Op(OpCode.I64Push, Word.FromI64(420)),
    new Op(OpCode.ClosureSetArg, Word.Ptr(1)),

    new Op(OpCode.LocalLoad, Word.Ptr(0)),

    new Op(OpCode.DebugDump),

    new Op(OpCode.Return),

    // proc main[closure`0]
    new Op(OpCode.NoOp), // 18

    new Op(OpCode.DebugDump),

    new Op(OpCode.Return),
};

var procTable2 = new ProcInfo[] 
{
    new ProcInfo() { Addr = 8, NumArgs = 0 }, // proc main
    new ProcInfo() { Addr = 18, NumArgs = 1 }, // proc main[closure`0]
};

var vm = new Vm()
{
    Ip = 0,
    Stack = new ValueStack(),
    Frames = new Stack<Frame>(),
};

var heap = new Heap(blockSize: 256);

var maxStack = 65_536 * 16;

var output = new ConsoleOutput();

var sw = new Stopwatch();
try
{
    sw.Start();
    Run(ref vm, ref heap, maxStack, output, program2, procTable2, new string[] { });
}
catch (VmException e)
{
    Console.WriteLine($"Runtime Error: {e.Message}");
    Console.WriteLine("Debugging Info:");
    Console.WriteLine(vm.Debug());
}
catch (VmHeapException e)
{
    Console.WriteLine($"Runtime Memory Error: {e.Message}");
    Console.WriteLine("Debugging Info:");
    Console.WriteLine(vm.Debug());
    // TODO print object at pointer
}
catch (VmExitException e)
{
    Console.WriteLine(e.Message);
}
finally
{
    sw.Stop();
    Console.WriteLine($"Execution took {sw.ElapsedMilliseconds} milliseconds.");
}