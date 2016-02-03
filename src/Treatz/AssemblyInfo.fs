namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Treatz")>]
[<assembly: AssemblyProductAttribute("Treatz")>]
[<assembly: AssemblyDescriptionAttribute("A ridiculous game brought to you by Dragon Treats Ltd")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
