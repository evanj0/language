using Interpreter.Exceptions;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace Interpreter.Memory;

public struct Heap
{
    public Heap(int blockSize)
    {
        BlockSize = blockSize;
        Blocks = new List<Block>();
        Blocks.Add(new Block(BlockSize));
        Block = 0;
    }

    public static readonly int HeaderSize = 2;

    public int BlockSize;

    public List<Block> Blocks;

    public int Block;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public ref Word GetWordRef(HeapPointer pointer)
    {
        if (pointer.Block >= Blocks.Count)
        {
            throw new InvalidPointerException(pointer);
        }
        if (pointer.Index >= Blocks[pointer.Block].Data.Length)
        {
            throw new InvalidPointerException(pointer);
        }
        return ref Blocks[pointer.Block].Data[pointer.Index];
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Word Deref(HeapPointer pointer)
    {
        if (pointer.Block >= Blocks.Count || pointer.Index >= Blocks[pointer.Block].Data.Length)
        {
            throw new InvalidPointerException(pointer);
        }
        return Blocks[pointer.Block].Data[pointer.Index];
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Header GetHeader(HeapPointer pointer)
    {
        unsafe
        {
            fixed (Word* wordPtr = &GetWordRef(pointer))
            {
                Header* headerPtr = (Header*)wordPtr;
                return *headerPtr;
            }
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public HeapPointer Alloc(int size)
    {
        Debug.Assert(Block < Blocks.Count);
        if (Blocks[Block].Top + size > Blocks[Block].MaxSize)
        {
            Block++;
            Blocks.Add(new Block(BlockSize));
        }

        var block = Block;
        var index = Blocks[Block].Top;
        Blocks[Block].SetTop(Blocks[Block].Top + size);
        return new HeapPointer()
        {
            Block = block,
            Index = index,
        };
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void Write(HeapPointer pointer, Word value)
    {
        if (pointer.Block >= Blocks.Count || pointer.Index >= Blocks[pointer.Block].Data.Length)
        {
            throw new InvalidPointerException(pointer);
        }
        Blocks[pointer.Block].Data[pointer.Index] = value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public HeapPointer AllocRecord(int fields)
    {
        var pointer = Alloc(fields + HeaderSize); // fields + headers
        var header = new Header()
        {
            Size = fields + HeaderSize,
            Type = ReferenceType.Record,
        };
        var recordHeader = new RecordHeader()
        {
            NumFields = fields,
        };
        GetWordRef(pointer) = header.ToWord();
        GetWordRef(pointer + 1) = recordHeader.ToWord();
        return pointer;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void CheckRecord(HeapPointer pointer, int field)
    {
        var header = Deref(pointer).ToHeader();
        var recordHeader = Deref(pointer + 1).ToProductHeader();
        if (header.Type != ReferenceType.Record)
        {
            throw new TypeMismatchException(ReferenceType.Record, header.Type, pointer);
        }
        if (field >= recordHeader.NumFields)
        {
            throw new VmHeapException($"Record did not contain at least {field + 1} field(s).", pointer);
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Word GetField(HeapPointer pointer, int field)
    {
        CheckRecord(pointer, field);
        return Deref(pointer + HeaderSize + field);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void SetField(HeapPointer pointer, int field, Word value)
    {
        CheckRecord(pointer, field);
        GetWordRef(pointer + HeaderSize + field) = value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public HeapPointer AllocClosure(int procPointer, int numParams)
    {
        var pointer = Alloc(numParams + HeaderSize);
        var header = new Header()
        {
            Size = numParams + HeaderSize,
            Type = ReferenceType.Closure,
        };
        var closureHeader = new ClosureHeader()
        {
            NumArgs = numParams,
            Pointer = procPointer,
        };
        Write(pointer, header.ToWord());
        Write(pointer + 1, closureHeader.ToWord());
        return pointer;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void CheckClosureType(HeapPointer pointer)
    {
        var header = Deref(pointer).ToHeader();
        if (header.Type != ReferenceType.Closure)
        {
            throw new TypeMismatchException(ReferenceType.Closure, header.Type, pointer);
        }
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void CheckClosureArgs(HeapPointer pointer, int args)
    {
        var closureHeader = Deref(pointer + 1).ToClosureHeader();
        if (args > closureHeader.NumArgs)
        {
            throw new VmHeapException($"Closure did not contain at least {args} arguments(s).", pointer);
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public ClosureHeader GetClosureHeader(HeapPointer pointer)
    {
        CheckClosureType(pointer);
        return Deref(pointer + 1).ToClosureHeader();
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void SetClosureArg(HeapPointer pointer, int param, Word value)
    {
        CheckClosureType(pointer);
        CheckClosureArgs(pointer, param + 1); // number of args
        Write(pointer + HeaderSize + param, value);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Word GetClosureArg(HeapPointer pointer, int param)
    {
        CheckClosureType(pointer);
        CheckClosureArgs(pointer, param + 1);
        return Deref(pointer + HeaderSize + param); 
    }
}

public struct Block
{
    public Block(int size)
    {
        Data = new Word[size];
        Top = 0;
        MaxSize = size;
    }

    public Word[] Data;

    public int Top;

    public int MaxSize;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void SetTop(int top)
    {
        Top = top;
    }
}

[StructLayout(LayoutKind.Explicit)]
public struct HeapPointer
{
    [FieldOffset(0)]
    public int Block;

    [FieldOffset(4)]
    public int Index;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static HeapPointer operator +(HeapPointer a, int b)
    {
        return new HeapPointer { Block = a.Block, Index = a.Index + b };
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Word ToWord()
    {
        unsafe
        {
            fixed (HeapPointer* heapPointerPtr = &this)
            {
                Word* wordPtr = (Word*)heapPointerPtr;
                return *wordPtr;
            }
        }
    }

    public string Debug()
    {
        return $"{Block}{Index}";
    }
}

public static class Word_HeapPointer_Extensions 
{
    public static HeapPointer ToHeapPointer(this Word word)
    {
        unsafe
        {
            Word* wordPtr = &word;
            HeapPointer* heapPointerPtr = (HeapPointer*)wordPtr;
            return *heapPointerPtr;
        }
    }
}
