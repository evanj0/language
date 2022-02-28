module Range

[<StructuredFormatDisplay("{Display}")>]
type Position = { index: int64; line: int64; column: int64 }
with 
    override this.ToString() = sprintf "%i:%i(%i)" this.line this.column this.index
    member this.Display with get() = this.ToString()

[<StructuredFormatDisplay("{Display}")>]
type Range = { start: Position; stop: Position }
with 
    override this.ToString() = sprintf "[%s..%s]" (this.start.ToString()) (this.stop.ToString())
    member this.Display with get() = this.ToString()

[<RequireQualifiedAccess>]
module Range =

    let create start stop = { start = start; stop = stop }