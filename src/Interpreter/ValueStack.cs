using Interpreter.Exceptions;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace Interpreter
{
    public struct ValueStack
    {
        public List<Word> Data;
        public int Sp;

        public ValueStack()
        {
            Data = new List<Word>();
            Sp = 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public Word Index(int index)
        {
            if (index >= Data.Count || index < 0)
            {
                throw new StackPointerOutOfRangeException();
            }

            return Data[index];
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public Word Peek()
        {
            return Index(Sp - 1);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public Word Peek(int index)
        {
            return Index(Sp - 1 - index);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Push(Word word)
        {
            if (Sp >= Data.Count)
            {
                Data.Add(word);
                Sp++;
            }
            else
            {
                Data[Sp] = word;
                Sp++;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public Word Pop()
        {
            var word = Peek();
            Sp -= 1;
            return word;
        }
    }
}
