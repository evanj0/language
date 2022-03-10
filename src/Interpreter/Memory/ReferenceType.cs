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
    Product,
    Sum,
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
public struct ProductHeader
{
    [FieldOffset(0)]
    public int NumFields;


    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Word ToWord()
    {
        unsafe
        {
            fixed (ProductHeader* ptr = &this)
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
    public int ProcPointer;

    [FieldOffset(4)]
    public int NumVariables;
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
    public static ProductHeader ToProductHeader(this Word word)
    {
        unsafe
        {
            Word* ptr = &word;
            ProductHeader* headerPtr = (ProductHeader*)ptr;
            return *headerPtr;
        }
    }
}