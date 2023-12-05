using MyLang;
using MyLang.Runtime;
using Environment = MyLang.Runtime.Environment;

Console.WriteLine("Program started. Type 'read' to read from file, 'q' to exit, 'notree' to disable the AST print, 'tree' to toggle it back on.");

string text = File.ReadAllText(@"..\..\..\..\Examples\example-code.txt");

var parser = new Parser();

Environment cliEnvironment = CreateScope();
var typeChecker = new TypeChecker();
TypeEnvironment typeEnvironment = TypeEnvironment.Create();
bool printTree = true;

while (true)
{
    string? input = Console.ReadLine();

    if (input is null or "q")
        break;

    switch (input)
    {
        case "notree":
            printTree = false;
            continue;
        
        case "tree":
            printTree = true;
            continue;
    }

    Environment targetEnvironment = cliEnvironment;
    TypeEnvironment targetTypeEnvironment = typeEnvironment;
    
    if (input is "read")
    {
        input = text;
        targetEnvironment = CreateScope();
        targetTypeEnvironment = TypeEnvironment.Create();
    }
    
    MyLang.Program program = parser.CreateAst(input);

    try
    {
        program.Traverse(node =>
        {
            if (node is IStatement statement)
                typeChecker.CheckType(statement, targetTypeEnvironment);
        });
        
        IRuntimeValue evaluation = Interpreter.Evaluate(program, targetEnvironment);
        Console.WriteLine(evaluation.ToString());
        Console.WriteLine();
        
        foreach (KeyValuePair<string, IRuntimeValue> variable in targetEnvironment.Variables)
            Console.WriteLine($"{variable.Key} = {variable.Value}");
    }
    catch (Exception e)
    {
        Console.WriteLine(e.Message);
    }
    
    int indent = 0;
    
    if (printTree)
    {
        Console.WriteLine(program.GetNodeTree(ref indent));
        Console.WriteLine();
    }
}

Console.WriteLine("Program ended.");

static Environment CreateScope()
{
    Environment environment = new();
    environment.MakeBool("true", true);
    environment.MakeBool("false", false);
    return environment;
}
