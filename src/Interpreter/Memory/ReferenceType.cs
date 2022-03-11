using Microsoft.FSharp.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace Interpreter.Memory;

// +----------------+------------------------+-------------+
// | Generic Header | Object-Specific Header | Object Data |
// |   Descriptor   |                        |             |
// |   GC Flags     |                        |             |
// |   Size         |                        |             |
// +----------------+------------------------+-------------+

public enum ReferenceType : byte
{
    Record,
    Union,
    Closure,
    String,
}

[StructLayout(LayoutKind.Explicit)]
public struct Header
{
    [FieldOffset(0)]
    public ReferenceType Type;

    [FieldOffset(4)]
    public int Size;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Word ToWord()
    {
        unsafe
        {
            fixed (Header* ptr = &this) 
            {
                Word* wordPtr = (Word*)ptr;
                return *wordPtr;
            }

        }
    }
}

[StructLayout(LayoutKind.Explicit)]
public struct RecordHeader
{
    [FieldOffset(0)]
    public int NumFields;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Word ToWord()
    {
        unsafe
        {
            fixed (RecordHeader* ptr = &this)
            {
                Word* wordPtr = (Word*)ptr;
                return *wordPtr;
            }

        }
    }
}

[StructLayout(LayoutKind.Explicit)]
public struct ClosureHeader
{
    [FieldOffset(0)]
    public int Pointer;

    [FieldOffset(4)]
    public int NumArgs;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Word ToWord() 
    {
        unsafe
        {
            fixed (ClosureHeader* ptr = &this)
            {
                Word* wordPtr = (Word*)ptr;
                return *wordPtr;
            }
        }
    }
}

public static class Word_Header_Extensions 
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Header ToHeader(this Word word)
    {
        unsafe
        {
            Word* ptr = &word;
            Header* headerPtr = (Header*)ptr;
            return *headerPtr;
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static RecordHeader ToProductHeader(this Word word)
    {
        unsafe
        {
            Word* ptr = &word;
            RecordHeader* headerPtr = (RecordHeader*)ptr;
            return *headerPtr;
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ClosureHeader ToClosureHeader(this Word word)
    {
        unsafe
        {
            Word* ptr = &word;
            ClosureHeader* headerPtr = (ClosureHeader*)ptr;
            return *headerPtr;
        }
    }
}