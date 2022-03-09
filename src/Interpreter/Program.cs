﻿using Interpreter;
using Interpreter.Exceptions;
using static Interpreter.Interpreter;

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
    new Op(OpCode.JumpIfFalse, Word.Ptr(3)),
        // then:
        new Op(OpCode.PushI64, Word.FromI64(69)),
        new Op(OpCode.Dump),
        new Op(OpCode.Jump, Word.Ptr(2)),
        // else:
        new Op(OpCode.PushI64, Word.FromI64(420)),
        new Op(OpCode.Dump),
    new Op(OpCode.Return),

    // proc 1:
    new Op(OpCode.NoOp), // 14
    new Op(OpCode.SetLocalOffset, Word.Ptr(1)),
    new Op(OpCode.PushI64, Word.FromI64(1000)),
    new Op(OpCode.Local, Word.Ptr(0)), // local 0
    new Op(OpCode.CmpEqI64),
    // new Op(OpCode.Call, Word.Ptr(1)),
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

var maxStack = 65_536 * 16;

var output = new ConsoleOutput();

try
{
    Run(ref vm, maxStack, output, program, procTable);
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