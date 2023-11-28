namespace MyLang.Models;

public readonly struct UnionMember
{
    public UnionMember(string typeName, string memberName)
    {
        TypeName = typeName;
        MemberName = memberName;
    }

    public string TypeName { get; }
    public string MemberName { get; }

    public override string ToString() => $"{TypeName}.{MemberName}";
}