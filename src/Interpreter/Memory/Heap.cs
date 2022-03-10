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
    public Word GetWord(HeapPointer pointer)
    {
        return GetWordRef(pointer);
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
    public HeapPointer AllocProduct(int fields)
    {
        var pointer = Alloc(fields + 2); // fields + headers
        var header = new Header()
        {
            Size = fields + 2,
            Type = ReferenceType.Product,
        };
        var prodHeader = new ProductHeader()
        {
            NumFields = fields,
        };
        GetWordRef(pointer) = header.ToWord();
        GetWordRef(pointer + 1) = prodHeader.ToWord();
        return pointer;
    }

    private void CheckProduct(HeapPointer pointer, int field)
    {
        var header = GetWord(pointer).ToHeader();
        var prodHeader = GetWord(pointer + 1).ToProductHeader();
        if (header.Type != ReferenceType.Product)
        {
            throw new TypeMismatchException(ReferenceType.Product, header.Type, pointer);
        }
        if (field >= prodHeader.NumFields)
        {
            throw new VmException($"Object at {pointer.Debug()} does not contain at least {field} field(s).");
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Word GetField(HeapPointer pointer, int field)
    {
        CheckProduct(pointer, field);
        return GetWord(pointer + 2 + field);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void SetField(HeapPointer pointer, int field, Word value)
    {
        CheckProduct(pointer, field);
        GetWordRef(pointer + 2 + field) = value;
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
