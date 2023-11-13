using MyLang;
using MyLang.Runtime;

Console.WriteLine("Program started. Type 'read' to read from file, 'q' to exit.");

string text = File.ReadAllText(@"D:\Software\JetBrains Rider 2023.1.2\Projects\MyLangSolution\Examples\example-code.txt");

var parser = new Parser();

Scope cliScope = CreateScope();
var typeChecker = new TypeChecker();
TypeEnvironment typeEnvironment = TypeEnvironment.Global;

while (true)
{
    string? input = Console.ReadLine();

    if (input is null or "q")
        break;

    Scope targetScope = cliScope;
    
    if (input is "read")
    {
        input = text;
        targetScope = CreateScope();
    }
    
    MyLang.Program program = parser.CreateAST(input);

    program.Traverse(node =>
    {
        if (node is IExpression exp and not DropExpression)
            typeChecker.CheckType(exp, typeEnvironment);
    });
    
    IRuntimeValue evaluation = Interpreter.Evaluate(program, targetScope);
    Console.WriteLine(evaluation.ToString());
    
    int indent = 0;
    Console.WriteLine(program.GetNodeTree(ref indent));
    Console.WriteLine();
}

Console.WriteLine("Program ended.");

static Scope CreateScope()
{
    Scope scope = new();
    scope.MakeBool("true", true);
    scope.MakeBool("false", false);
    scope.DeclareType("f32");
    scope.DeclareType("i32");
    return scope;
}