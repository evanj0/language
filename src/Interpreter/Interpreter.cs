using Interpreter.Exceptions;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace Interpreter;

public static class Interpreter
{
    // TODO implement (jump-table)
    // TODO implement heap operations:
        // TODO allocation
        // TODO data types + runtime type info
        // TODO pattern matching
        // TODO gc?

    public static void Run(ref Vm vm, int maxStack, IVmOutput output, Op[] program, int[] procTable)
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
                    {
                        var frame = PopCurrentFrame(ref vm);
                        vm.Ip = frame.ReturnAddr;
                        var returnValue = vm.Stack.Pop();
                        vm.Stack.Data[frame.BasePtr] = returnValue;
                        vm.Stack.Sp = frame.BasePtr + 1;
                        break;
                    }

                // bool -> 
                case OpCode.JumpIfFalse:
                    if (vm.Stack.Pop().ToBool() == false)
                    {
                        vm.Ip += inst.Data.ToI32();
                    }
                    break;

                case OpCode.Jump:
                    vm.Ip += inst.Data.ToI32();
                    break;

                // Debugging

                case OpCode.Dump:
                    output.WriteLine(vm.Debug());
                    break;

                // Stack Ops

                case OpCode.Local:
                    {
                        var index = PeekCurrentFrame(ref vm).BasePtr + inst.Data.ToI32();
                        var value = vm.Stack.Index(index);
                        vm.Stack.Push(value);
                        break;
                    }

                case OpCode.SetLocalOffset:
                    {
                        var frame = vm.Frames.Pop();
                        frame.BasePtr -= inst.Data.ToI32();
                        vm.Frames.Push(frame);
                        break;
                    }

                //  -> i64
                case OpCode.PushI64:
                    vm.Stack.Push(inst.Data);
                    break;

                //  -> bool
                case OpCode.PushBool:
                    vm.Stack.Push(inst.Data);
                    break;


                // Math

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

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Frame PeekCurrentFrame(ref Vm vm)
    {
        if (vm.Frames.Count == 0)
        {
            throw new CallStackUnderflowException();
        }
        return vm.Frames.Peek();
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Frame PopCurrentFrame(ref Vm vm)
    {
        if (vm.Frames.Count == 0)
        {
            throw new CallStackUnderflowException();
        }
        return vm.Frames.Pop();
    }
}

