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
        /// <code>(exit status-code:i32)</code>
        /// </summary>
        Exit,

        /// <summary>
        /// <code>(no-op)</code>
        /// </summary>
        NoOp,

        /// <summary>
        /// <code>(ip-set ip:u64)</code>
        /// </summary>
        IpSet,

        /// <summary>
        /// <code>(call proc-ptr:u32)</code>
        /// </summary>
        Call,

        /// <summary>
        /// <code>(return)</code>
        /// Copies the top value to baseptr + 0. Lowers stack pointer to baseptr + 1.
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

        DebugDump,

        // Stack Ops

        /// <summary>
        /// i64
        /// </summary>
        I64Push,

        /// <summary>
        /// f64
        /// </summary>
        F64Push,

        /// <summary>
        /// char
        /// </summary>
        CharPush,

        /// <summary>
        /// bool
        /// </summary>
        BoolPush,

        /// <summary>
        /// <code>(local-arg-load index:u16)</code>
        /// <code> -> *</code>
        /// Pushes the argument at <c>index</c> to the stack.
        /// </summary>
        LocalArgLoad,

        /// <summary>
        /// <code>(local-closure-arg-load index:u16)</code>
        /// <code> -> *</code>
        /// Pushes the closure argument at <c>index</c> to the stack.
        /// </summary>
        LocalClosureArgLoad,

        /// <summary>
        /// <code>(local-load index:u16)</code>
        /// <code> -> *</code>
        /// Pushes the local value at <c>index</c> to the stack.
        /// </summary>
        LocalLoad,

        // Heap

        /// <summary>
        /// <code>(string location:u64)</code>
        /// Allocates the the UTF-16 string contained in the data section at the index and pushes the pointer to the stack.
        /// </summary>
        StringLoad,

        RecordAlloc,

        RecordGetField,

        RecordSetField,

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

        I64Add,

        I64CmpEq,
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
