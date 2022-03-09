using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace Interpreter
{
    [StructLayout(LayoutKind.Explicit)]
    public struct Word
    {
        [FieldOffset(0)]
        public byte Byte0 = 0;

        [FieldOffset(1)]
        public byte Byte1 = 0;

        [FieldOffset(2)]
        public byte Byte2 = 0;

        [FieldOffset(3)]
        public byte Byte3 = 0;

        [FieldOffset(4)]
        public byte Byte4 = 0;

        [FieldOffset(5)]
        public byte Byte5 = 0;

        [FieldOffset(6)]
        public byte Byte6 = 0;

        [FieldOffset(7)]
        public byte Byte7 = 0;

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static Word Ptr(ulong value)
        {
            unsafe
            {
                ulong* ptr = &value;
                Word* wordPtr = (Word*)ptr;
                return *wordPtr;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static Word FromI64(long value)
        {
            unsafe
            {
                long* ptr = &value;
                Word* wordPtr = (Word*)ptr;
                return *wordPtr;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static Word FromI32(int value)
        {
            unsafe
            {
                int* ptr = &value;
                byte* bytePtr = (byte*)ptr;
                var word = new Word();
                word.Byte0 = bytePtr[0];
                word.Byte1 = bytePtr[1];
                word.Byte2 = bytePtr[2];
                word.Byte3 = bytePtr[3];
                return word;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static Word FromBool(bool value)
        {
            var word = new Word();
            if (value)
            {
                word.Byte0 = 1;
            }
            return word;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static Word Zero() => FromI64(0);

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public long ToI64()
        {
            unsafe
            {
                fixed (Word* wordPtr = &this)
                {
                    long* longPtr = (long*)wordPtr;
                    return *longPtr;
                }
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int ToI32()
        {
            unsafe
            {
                fixed (Word* wordPtr = &this)
                {
                    int* intPtr = (int*)wordPtr;
                    return *intPtr;
                }
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool ToBool()
        {
            return Byte0 == 1;
        }

        public string Debug()
        {
            return $"Int64 = {ToI64()}, Bool = {ToBool()}";
        }
    }
}
