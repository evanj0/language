using NUnit.Framework;
using System.Diagnostics;

using Interpreter;
using Interpreter.Exceptions;
using Interpreter.Memory;
using TextFormat;
using System.Collections.Generic;
using System;
using System.IO;

namespace Interpreter.Tests;

public class Tests
{

    private static void Run(string program)
    {

        var vm = new Vm()
        {
            Ip = 0,
            Stack = new ValueStack(),
            Frames = new Stack<Frame>(),
        };

        var heap = new Heap(blockSize: 256);

        var maxStack = 65_536 * 16;

        var output = new ConsoleOutput();

        var sw1 = new Stopwatch();
        sw1.Start();
        var (instructions, procTable, strings) = Assembler.Compile(program);
        sw1.Stop();
        Console.WriteLine($"Assembly took {sw1.ElapsedMilliseconds} milliseconds.");

        var sw = new Stopwatch();
        try
        {
            sw.Start();
            Interpreter.Run(ref vm, ref heap, maxStack, output, instructions, procTable, strings);
        }
        catch (VmException e)
        {

            TestContext.WriteLine($"Runtime Error: {e.Message}");
            TestContext.WriteLine("Debugging Info:");
            TestContext.WriteLine(vm.Debug());
            TestContext.WriteLine($"Current Instruction: `{instructions[vm.Ip].OpCode.ToUserString()}`");
        }
        catch (VmHeapException e)
        {
            TestContext.WriteLine($"Runtime Memory Error: {e.Message}");
            TestContext.WriteLine("Debugging Info:");
            TestContext.WriteLine(vm.Debug());
            TestContext.WriteLine($"Current Instruction: `{instructions[vm.Ip].OpCode.ToUserString()}`");
            // TODO print object at pointer
        }
        catch (VmExitException e)
        {
            TestContext.WriteLine(e.Message);
        }
        finally
        {
            sw.Stop();
            TestContext.WriteLine($"Execution took {sw.ElapsedMilliseconds} milliseconds.");
        }
    }

    [Test]
    public void Test1()
    {
        var program = File.ReadAllText("files/test1.asmtxt");
        Run(program);
        Assert.Pass();
    }

    [Test]
    public void Fibonacci()
    {
        var program = File.ReadAllText("files/fib.asmtxt");
        Run(program);
        Assert.Pass();
    }
}
