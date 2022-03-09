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

        /// <summary>
        /// i32
        /// </summary>
        Exit,

        NoOp,

        /// <summary>
        /// ptr
        /// </summary>
        SetIp,

        /// <summary>
        /// ptr
        /// </summary>
        Call,

        Return,

        /// <summary>
        /// ptr
        /// </summary>
        JumpIfFalse,

        /// <summary>
        /// ptr
        /// </summary>
        Jump,

        /// <summary>
        /// ptr
        /// </summary>
        JumpTable,

        // Debugging

        Dump,

        // Stack Ops

        /// <summary>
        /// i64
        /// </summary>
        PushI64,

        /// <summary>
        /// f64
        /// </summary>
        PushF64,

        /// <summary>
        /// char
        /// </summary>
        PushChar,

        /// <summary>
        /// bool
        /// </summary>
        PushBool,

        /// <summary>
        /// ptr
        /// </summary>
        Local,


        // Math

        AddI64,

        CmpEqI64,
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

        public Op(OpCode opCode)
        {
            OpCode = opCode;
            Data = Word.Zero();
        }

        public OpCode OpCode;

        public Word Data;
    }
}
