using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace ActionResultAnalyzer.Analyzers;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public class ActionResultTypeAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "ACT001";
    
    private static readonly DiagnosticDescriptor Rule = new(
        DiagnosticId,
        "Invalid return type for ActionResult<T>",
        "Method returning ActionResult<{0}> cannot return '{1}'",
        "TypeSafety",
        DiagnosticSeverity.Error,
        true,
        "ActionResult<T> methods should only return T or ActionResult-derived types");

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics 
        => [Rule];

    public override void Initialize(AnalysisContext context)
    {
        context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
        context.EnableConcurrentExecution();
        
        context.RegisterSyntaxNodeAction(AnalyzeMethodDeclaration, SyntaxKind.MethodDeclaration);
    }

    private void AnalyzeMethodDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not MethodDeclarationSyntax methodDeclaration)
            return;

        if (methodDeclaration.ReturnType is not GenericNameSyntax { Identifier.Text: "ActionResult" } genericName)
            return;

        var typeArgument = genericName.TypeArgumentList.Arguments.FirstOrDefault();
        if (typeArgument is null) return;

        var expectedTypeSymbol = context.SemanticModel.GetTypeInfo(typeArgument).Type;
        if (expectedTypeSymbol is null) return;

        foreach (var returnStatement in methodDeclaration.DescendantNodes().OfType<ReturnStatementSyntax>())
        {
            if (returnStatement.Expression is null) continue;

            var returnedExpressionType = context.SemanticModel.GetTypeInfo(returnStatement.Expression).Type;
            if (returnedExpressionType is null) continue;

            if (!IsTypeCompatible(returnedExpressionType, expectedTypeSymbol, context))
            {
                context.ReportDiagnostic(Diagnostic.Create(
                    Rule,
                    returnStatement.Expression.GetLocation(),
                    expectedTypeSymbol.Name,
                    returnedExpressionType.Name));
            }
        }
    }

    private static bool IsTypeCompatible(ITypeSymbol returnedType, ITypeSymbol expectedType, SyntaxNodeAnalysisContext context)
    {
        if (SymbolEqualityComparer.Default.Equals(returnedType, expectedType))
            return true;

        var actionResultType = context.Compilation.GetTypeByMetadataName("Microsoft.AspNetCore.Mvc.ActionResult");
        var actionResultTType = context.Compilation.GetTypeByMetadataName("Microsoft.AspNetCore.Mvc.ActionResult`1");
        
        if (actionResultType is not null && returnedType.InheritsFrom(actionResultType))
            return true;

        if (actionResultTType is not null && 
            SymbolEqualityComparer.Default.Equals(returnedType.OriginalDefinition, actionResultTType) &&
            returnedType is INamedTypeSymbol { TypeArguments: [var actualTypeArgument] })
        {
            return SymbolEqualityComparer.Default.Equals(actualTypeArgument, expectedType);
        }

        return false;
    }
}

public static class SymbolExtensions
{
    public static bool InheritsFrom(this ITypeSymbol type, ITypeSymbol possibleBaseType)
    {
        var current = type;
        while (current is not null)
        {
            if (SymbolEqualityComparer.Default.Equals(current, possibleBaseType))
                return true;
            current = current.BaseType;
        }
        return false;
    }
}