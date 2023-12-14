namespace Arcana.Models;

public readonly struct UnionMember(string typeName, string memberName)
{
    public string TypeName { get; } = typeName;
    public string MemberName { get; } = memberName;

    public override string ToString() => $"{TypeName}.{MemberName}";
}