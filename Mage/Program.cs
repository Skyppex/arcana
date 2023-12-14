using System.Text;
using Arcana;
using Mage.Interpreter;
using Arcana.Parser;
using Arcana.TypeChecker;

using Environment = Mage.Interpreter.Environment;

Console.WriteLine("Program started. Type 'read' to read from file, 'q' to exit, 'notree' to disable the AST print, 'tree' to toggle it back on.");

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

    const string EXAMPLE_CODE_PATH = @"..\..\..\..\Examples\example-code.txt";

    if (input is "readonly")
    {
        input = File.ReadAllText(EXAMPLE_CODE_PATH);
        targetEnvironment = CreateScope("ReadOnlyGlobal");
        targetTypeEnvironment = TypeEnvironment.Create();
    }

    if (input is "read")
        input = File.ReadAllText(EXAMPLE_CODE_PATH);

    if (input.StartsWith("read "))
    {
        string[] fileNames = input.Remove(0, 5).Split(' ');

        if (fileNames.Length is 1 && fileNames[0] is "lib" or "library")
        {
            string[] files = Directory.GetFiles(@"..\..\..\..\Library");
            input = ParseLibrary(files.Select(Path.GetFileNameWithoutExtension).ToArray()!);
        }
        else
            input = ParseLibrary(fileNames);
    }
    
    Arcana.Parser.ProgramStatement programStatement = parser.CreateAst(input);

    try
    {
        programStatement.Traverse(node =>
        {
            if (node is IStatement statement)
                typeChecker.CheckType(statement, targetTypeEnvironment);
        });
        
        IRuntimeValue evaluation = Interpreter.Evaluate(programStatement, targetEnvironment);
        Console.WriteLine(evaluation.ToString());
        Console.WriteLine();

        PrintVariables(targetEnvironment);
        targetEnvironment.Children.Clear();
    }
    catch (Exception e)
    {
        Console.WriteLine(e.Message);
    }
    
    int indent = 0;
    
    if (printTree)
    {
        Console.Write(programStatement.GetNodeTree(ref indent));
    }
}

Console.WriteLine("Program ended.");

static Environment CreateScope(string name = "Global")
{
    Environment environment = new(name: name);
    // environment.MakeBool("true", true);
    // environment.MakeBool("false", false);
    return environment;
}

string ParseLibrary(string[] strings)
{
    StringBuilder stringBuilder = new();

    foreach (string fileName in strings)
    {
        Console.WriteLine("Reading file: " + fileName);
        stringBuilder.AppendLine(File.ReadAllText($@"..\..\..\..\Library\{fileName}.txt"));
    }

    return stringBuilder.ToString();
}

static void PrintVariables(Environment environment)
{
    PrintVariablesRecurse("", environment);
    return;
    
    static void PrintVariablesRecurse(string? parentName, Environment environment)
    {
        foreach (KeyValuePair<string, IRuntimeValue> variable in environment.ScopeVariables)
        {
            var scope = parentName is null ? environment.Name : string.Join('.', parentName, environment.Name);
            Console.WriteLine($"Scope: {scope} | {variable.Key} = {variable.Value}");
        }
        
        PrintChildren(environment.Name, environment.Children);
        return;
        
        static void PrintChildren(string parentName, List<Environment> children)
        {
            foreach (var child in children)
                PrintVariablesRecurse(parentName, child);
        }
    }
}
