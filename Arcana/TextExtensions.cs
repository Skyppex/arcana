namespace Arcana;

public static class TextExtensions
{
    public static string ToLiteral(this string input) => 
        Microsoft.CodeAnalysis.CSharp.SymbolDisplay.FormatLiteral(input, false);
    
    public static string ToLiteral(this char input) => 
        Microsoft.CodeAnalysis.CSharp.SymbolDisplay.FormatLiteral(input, false);
    
    public static string AppendParens(this string input) => $"{input}()";
}