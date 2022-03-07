module Range

[<StructuredFormatDisplay("{Display}")>]
type Position = { index: int64; line: int64; column: int64; file: string }
with 
    override this.ToString() = sprintf "%s@%i:%i" this.file this.line this.column
    member this.Display with get() = this.ToString()

[<StructuredFormatDisplay("{Display}")>]
type Range = { start: Position; stop: Position }
with 
    override this.ToString() = sprintf "[ %s .. %i:%i ]" (this.start.ToString()) this.stop.line this.stop.column
    member this.Display with get() = this.ToString()

[<RequireQualifiedAccess>]
module Range =

    let create start stop = { start = start; stop = stop }