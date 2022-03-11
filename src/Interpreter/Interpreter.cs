using Interpreter.Exceptions;
using Interpreter.Memory;
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

    public static void Run(ref Vm vm, ref Heap heap, int maxStack, IVmOutput output, Op[] program, ProcInfo[] procTable, Span<byte> data, int[] dataTable)
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

                case OpCode.IpSet:
                    vm.Ip = inst.Data.ToI32();
                    break;

                case OpCode.Call:
                    {
                        var proc = inst.Data.ToI32();
                        Call(ref vm, maxStack, procTable, proc, 0);
                        break;
                    }

                case OpCode.Return:
                    {
                        var frame = PopCurrentFrame(ref vm);
                        vm.Ip = frame.ReturnAddr;
                        var returnValue = vm.Stack.Pop();
                        vm.Stack.Data[frame.BaseSp] = returnValue;
                        vm.Stack.Sp = frame.BaseSp + 1;
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

                case OpCode.DebugDump:
                    output.WriteLine("----------- Stack Dump -----------");
                    output.Write(vm.Debug());
                    output.WriteLine("--------- End Stack Dump ---------");
                    output.WriteLine("");
                    break;

                // Stack Ops

                //  -> i64
                case OpCode.I64Push:
                    vm.Stack.Push(inst.Data);
                    break;

                //  -> bool
                case OpCode.BoolPush:
                    vm.Stack.Push(inst.Data);
                    break;

                case OpCode.LocalArgLoad:
                    {
                        var index = PeekCurrentFrame(ref vm).BaseSp + inst.Data.ToI32();
                        var value = vm.Stack.Index(index);
                        vm.Stack.Push(value);
                        break;
                    }

                case OpCode.LocalClosureArgLoad:
                    {
                        var index = PeekCurrentFrame(ref vm).ClosureArgsSp + inst.Data.ToI32();
                        var value = vm.Stack.Index(index);
                        vm.Stack.Push(value);
                        break;
                    }

                case OpCode.LocalLoad:
                    {
                        var index = PeekCurrentFrame(ref vm).LocalsSp + inst.Data.ToI32();
                        var value = vm.Stack.Index(index);
                        vm.Stack.Push(value);
                        break;
                    }

                // Heap

                case OpCode.RecordAlloc:
                    { 
                        var pointer = heap.AllocRecord(inst.Data.ToI32());
                        vm.Stack.Push(pointer.ToWord());
                        break;
                    }

                case OpCode.RecordGetField:
                    {
                        var pointer = vm.Stack.Pop().ToHeapPointer();
                        var value = heap.GetField(pointer, inst.Data.ToI32());
                        vm.Stack.Push(value);
                        break;
                    }

                // ptr * -> 
                case OpCode.RecordSetField:
                    {
                        var value = vm.Stack.Pop();
                        var pointer = vm.Stack.Pop().ToHeapPointer();
                        heap.SetField(pointer, inst.Data.ToI32(), value);
                        break;
                    }

                case OpCode.ClosureAlloc:
                    {
                        var procPointer = inst.Data.ReadU32(0);
                        var numParams = inst.Data.ReadU16(4);
                        var pointer = heap.AllocClosure((int)procPointer, numParams);
                        vm.Stack.Push(pointer.ToWord());
                        break;
                    }

                case OpCode.ClosureSetArg:
                    {
                        var value = vm.Stack.Pop();
                        var pointer = vm.Stack.Pop().ToHeapPointer();
                        var paramIdx = inst.Data.ReadU16(0);
                        heap.SetClosureArg(pointer, paramIdx, value);
                        break;
                    }

                // -- Locals Sp
                // closure arg_n
                // ...
                // closure arg_0
                // -- Closure Args Sp
                // procedure arg_n
                // ...
                // procedure arg_1
                // -- Base Pointer
                case OpCode.ClosureApply:
                    {
                        var pointer = vm.Stack.Pop().ToHeapPointer();
                        var header = heap.GetClosureHeader(pointer);
                        Call(ref vm, maxStack, procTable, header.Pointer, header.NumArgs);
                        for (var i = 0; i < header.NumArgs; i++)
                        {
                            vm.Stack.Push(heap.GetClosureArg(pointer, i));
                        }
                        break;
                    }

                // Math

                // i64 i64 -> i64
                case OpCode.I64Add:
                    {
                        var val1 = vm.Stack.Pop().ToI64();
                        var val2 = vm.Stack.Pop().ToI64();
                        vm.Stack.Push(Word.FromI64(val1 + val2));
                        break;
                    }

                // i64 i64 -> bool
                case OpCode.I64CmpEq:
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
    private static void Call(ref Vm vm, int maxStack, ProcInfo[] procTable, int proc, int closureArgs)
    {
        if (proc >= procTable.Length)
        {
            throw new ProcedureDoesNotExistException(proc);
        }
        if (vm.Frames.Count >= maxStack)
        {
            throw new CallStackOverflowException(vm.Frames.Count);
        }
        if (vm.Stack.Sp - procTable[proc].NumArgs < 0)
        {
            throw new VmException("Base stack pointer was moved out of range.");
        }
        vm.Frames.Push(new Frame(
            returnAddr: vm.Ip, 
            baseSp: vm.Stack.Sp - procTable[proc].NumArgs, // baseSp at first argument
            closureArgsOffset: procTable[proc].NumArgs, // closure args after arguments
            localsOffset: procTable[proc].NumArgs + closureArgs // locals after closure args and procedure args
            ));
        vm.Ip = procTable[proc].Addr;
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

