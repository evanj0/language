using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace Interpreter;

public struct Frame
{
    public Frame(int returnAddr, int baseSp, int closureArgsOffset, int localsOffset)
    {
        ReturnAddr = returnAddr;
        BaseSp = baseSp;
        ClosureArgsOffset = closureArgsOffset;
        LocalsOffset = localsOffset;
    }
    public int ReturnAddr;

    public int BaseSp;

    /// <summary>
    /// Offset of start of closure args from base stack pointer.
    /// </summary>
    public int ClosureArgsOffset;

    /// <summary>
    /// Offset of start of locals from base stack pointer.
    /// </summary>
    public int LocalsOffset;

    public int ClosureArgsSp 
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get => BaseSp + ClosureArgsOffset; 
    }

    public int LocalsSp
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get => BaseSp + LocalsOffset;
    }
}
