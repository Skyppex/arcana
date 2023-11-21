using MyLang;
using MyLang.Runtime;

Console.WriteLine("Program started. Type 'read' to read from file, 'q' to exit.");

string text = File.ReadAllText(@"..\..\..\..\Examples\example-code.txt");

var parser = new Parser();

Scope cliScope = CreateScope();
var typeChecker = new TypeChecker();
TypeEnvironment typeEnvironment = TypeEnvironment.Create();

while (true)
{
    string? input = Console.ReadLine();

    if (input is null or "q")
        break;

    Scope targetScope = cliScope;
    TypeEnvironment targetTypeEnvironment = typeEnvironment;
    
    if (input is "read")
    {
        input = text;
        targetScope = CreateScope();
        targetTypeEnvironment = TypeEnvironment.Create();
    }
    
    MyLang.Program program = parser.CreateAst(input);

    program.Traverse(node =>
    {
        if (node is IStatement statement)
            typeChecker.CheckType(statement, targetTypeEnvironment);
    });
    
    IRuntimeValue evaluation = Interpreter.Evaluate(program, targetScope, targetTypeEnvironment);
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
    return scope;
}
