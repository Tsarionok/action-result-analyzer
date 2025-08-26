using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System.Collections.Immutable;
using System.Linq;

namespace ActionResultAnalyzer
{
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
            "ActionResult<T> methods should only return T, ActionResult, Task<T>, Task<ActionResult>, etc.");

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics 
            => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();
            
            context.RegisterSyntaxNodeAction(AnalyzeMethodDeclaration, SyntaxKind.MethodDeclaration);
        }

        private void AnalyzeMethodDeclaration(SyntaxNodeAnalysisContext context)
        {
            var methodDeclaration = (MethodDeclarationSyntax)context.Node;
            
            if (methodDeclaration.ReturnType is not GenericNameSyntax genericName ||
                genericName.Identifier.Text != "ActionResult")
            {
                return;
            }
            
            var typeArgument = genericName.TypeArgumentList.Arguments.FirstOrDefault();
            if (typeArgument == null) return;

            var expectedTypeSymbol = context.SemanticModel.GetTypeInfo(typeArgument).Type;
            if (expectedTypeSymbol == null) return;
            
            var returnStatements = methodDeclaration.DescendantNodes()
                .OfType<ReturnStatementSyntax>();

            foreach (var returnStatement in returnStatements)
            {
                if (returnStatement.Expression == null) continue;

                var returnedExpressionType = context.SemanticModel.GetTypeInfo(returnStatement.Expression).Type;
                if (returnedExpressionType == null) continue;
                
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

        private bool IsTypeCompatible(ITypeSymbol returnedType, ITypeSymbol expectedType, SyntaxNodeAnalysisContext context)
        {
            if (SymbolEqualityComparer.Default.Equals(returnedType, expectedType))
                return true;
            
            if (returnedType.IsTaskWrapper(expectedType, context))
                return true;
            
            var actionResultTypeName = "Microsoft.AspNetCore.Mvc.ActionResult";
            var actionResultTTypeName = "Microsoft.AspNetCore.Mvc.ActionResult`1";
            
            if (returnedType.InheritsFrom(actionResultTypeName))
                return true;

            if (returnedType.IsGenericActionResult(actionResultTTypeName, expectedType))
                return true;

            return false;
        }
    }

    public static class SymbolExtensions
    {
        public static bool InheritsFrom(this ITypeSymbol type, string fullTypeName)
        {
            var current = type;
            while (current is not null)
            {
                if (current.ToDisplayString() == fullTypeName)
                    return true;
                current = current.BaseType;
            }
            return false;
        }

        public static bool IsGenericActionResult(this ITypeSymbol type, string actionResultTTypeName, ITypeSymbol expectedType)
        {
            if (type is not INamedTypeSymbol namedType || !namedType.IsGenericType)
                return false;

            var originalDefinition = namedType.OriginalDefinition;
            if (originalDefinition.ToDisplayString() != actionResultTTypeName)
                return false;

            return namedType.TypeArguments.Length == 1 && 
                   SymbolEqualityComparer.Default.Equals(namedType.TypeArguments[0], expectedType);
        }

        public static bool IsTaskWrapper(this ITypeSymbol type, ITypeSymbol expectedType, SyntaxNodeAnalysisContext context)
        {
            if (type is INamedTypeSymbol namedType && namedType.IsGenericType)
            {
                var originalDefinition = namedType.OriginalDefinition;
                var originalDefinitionName = originalDefinition.ToDisplayString();
                
                if (originalDefinitionName is "System.Threading.Tasks.Task<T>" or "System.Threading.Tasks.ValueTask<T>")
                {
                    if (namedType.TypeArguments.Length == 1)
                    {
                        var taskResultType = namedType.TypeArguments[0];
                        
                        return IsTypeCompatible(taskResultType, expectedType, context);
                    }
                }
            }
            
            return false;
        }

        private static bool IsTypeCompatible(ITypeSymbol returnedType, ITypeSymbol expectedType, SyntaxNodeAnalysisContext context)
        {
            var actionResultTypeName = "Microsoft.AspNetCore.Mvc.ActionResult";
            var actionResultTTypeName = "Microsoft.AspNetCore.Mvc.ActionResult`1";
            
            if (SymbolEqualityComparer.Default.Equals(returnedType, expectedType))
                return true;

            if (returnedType.InheritsFrom(actionResultTypeName))
                return true;

            if (returnedType.IsGenericActionResult(actionResultTTypeName, expectedType))
                return true;

            return false;
        }
    }
}