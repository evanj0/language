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
        /// <code>(call proc-ptr:u32)</code>
        /// </summary>
        Call,

        /// <summary>
        /// <code>(return)</code>
        /// Copies the top value to localoffset + 0. Lowers stack pointer to localoffset + 1.
        /// </summary>
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
        /// <code>(local idx)</code>
        /// Pushes the value at the index to the stack.
        /// </summary>
        Local,

        /// <summary>
        /// ptr
        /// </summary>
        SetLocalOffset,

        /// <summary>
        /// <code>(local-arg-load index:u16)</code>
        /// <code> -> *</code>
        /// Pushes the argument at <c>index</c> to the stack.
        /// </summary>
        LocalArgLoad,

        /// <summary>
        /// <code>(local-closure-load index:u16)</code>
        /// <code> -> *</code>
        /// Pushes the closure argument at <c>index</c> to the stack.
        /// </summary>
        LocalClosureLoad,

        /// <summary>
        /// <code>(local-load index:u16)</code>
        /// <code> -> *</code>
        /// Pushes the local value at <c>index</c> to the stack.
        /// </summary>
        LocalLoad,

        // Heap

        /// <summary>
        /// <code>(string idx)</code>
        /// Allocates the the UTF-16 string contained in the data section at the index and pushes the pointer to the stack.
        /// </summary>
        String,

        Record,

        GetField,

        SetField,

        /// <summary>
        /// <code>(closure-alloc proc-ptr:u32 num-closure-args:u16)</code>
        /// <code> -> *</code>
        /// </summary>
        ClosureAlloc,

        /// <summary>
        /// <code>(closure-set-arg arg-index:u16)</code>
        /// <code>ptr * -> </code>
        /// </summary>
        ClosureSetArg,

        /// <summary>
        /// <code>(closure-apply)</code>
        /// <code>* ptr -> *</code>
        /// </summary>
        ClosureApply,


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
