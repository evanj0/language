using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace Interpreter
{
    public enum OpCode
    {
        // Control Flow
        Exit,
        NoOp,
        SetIp,
        Call,
        Return,

        // Debugging
        Dump,

        // Stack Ops
        PushI64,
        PushF64,
        PushChar,
        PushBool,
        PushPtr,
        Local,

        // Math
        AddI64,
    }

    public static class OpCodeExtensions 
    {
        public static string ToUserString(this OpCode opCode)
        {
            var pascalCase = new Regex(@"(?<=[A-Z])(?=[A-Z][a-z])|(?<=[^A-Z])(?=[A-Z])|(?<=[A-Za-z])(?=[^A-Za-z])");
            var s = pascalCase.Replace(opCode.ToString(), "-").ToLower();
            return $"({s})";
        }
    }

    public struct Op
    {
        public Op(OpCode opCode, Word data)
        {
            OpCode = opCode;
            Data = data;
        }
        public OpCode OpCode;
        public Word Data;
    }
}
