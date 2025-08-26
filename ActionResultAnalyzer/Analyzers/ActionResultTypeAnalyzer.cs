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
            "ActionResult<T> methods should only return T or ActionResult-derived types");

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
            
            ITypeSymbol expectedTypeSymbol = null;
            bool isAsync = false;

            if (methodDeclaration.ReturnType is GenericNameSyntax genericName &&
                genericName.Identifier.Text == "ActionResult")
            {
                var typeArgument = genericName.TypeArgumentList.Arguments.FirstOrDefault();
                if (typeArgument == null) return;

                expectedTypeSymbol = context.SemanticModel.GetTypeInfo(typeArgument).Type;
            }
            else if (methodDeclaration.ReturnType is GenericNameSyntax taskGenericName &&
                     (taskGenericName.Identifier.Text == "Task" || taskGenericName.Identifier.Text == "ValueTask"))
            {
                if (taskGenericName.TypeArgumentList.Arguments.FirstOrDefault() is GenericNameSyntax actionResultGeneric &&
                    actionResultGeneric.Identifier.Text == "ActionResult")
                {
                    var typeArgument = actionResultGeneric.TypeArgumentList.Arguments.FirstOrDefault();
                    if (typeArgument == null) return;

                    expectedTypeSymbol = context.SemanticModel.GetTypeInfo(typeArgument).Type;
                    isAsync = true;
                }
            }

            if (expectedTypeSymbol == null) return;

            var returnStatements = methodDeclaration.DescendantNodes()
                .OfType<ReturnStatementSyntax>();

            foreach (var returnStatement in returnStatements)
            {
                if (returnStatement.Expression == null) continue;

                var expressionType = context.SemanticModel.GetTypeInfo(returnStatement.Expression).Type;
                if (expressionType == null) continue;

                if (isAsync)
                {
                    AnalyzeAsyncReturnExpression(returnStatement.Expression, expectedTypeSymbol, context, returnStatement.GetLocation());
                }
                else
                {
                    AnalyzeReturnExpression(returnStatement.Expression, expectedTypeSymbol, context, returnStatement.GetLocation());
                }
            }
        }

        private void AnalyzeAsyncReturnExpression(ExpressionSyntax expression, ITypeSymbol expectedType, SyntaxNodeAnalysisContext context, Location location)
        {
            var expressionType = context.SemanticModel.GetTypeInfo(expression).Type;
            
            if (expressionType is INamedTypeSymbol namedType && namedType.IsGenericType)
            {
                var originalDefinition = namedType.OriginalDefinition?.ToDisplayString();
                
                if (originalDefinition is "System.Threading.Tasks.Task<T>" or "System.Threading.Tasks.ValueTask<T>")
                {
                    if (namedType.TypeArguments.Length == 1)
                    {
                        var taskResultType = namedType.TypeArguments[0];
                        AnalyzeReturnExpressionType(taskResultType, expectedType, context, location);
                        return;
                    }
                }
            }

            AnalyzeReturnExpression(expression, expectedType, context, location);
        }

        private void AnalyzeReturnExpression(ExpressionSyntax expression, ITypeSymbol expectedType, SyntaxNodeAnalysisContext context, Location location)
        {
            var expressionType = context.SemanticModel.GetTypeInfo(expression).Type;
            AnalyzeReturnExpressionType(expressionType, expectedType, context, location);
        }

        private void AnalyzeReturnExpressionType(ITypeSymbol expressionType, ITypeSymbol expectedType, SyntaxNodeAnalysisContext context, Location location)
        {
            if (expressionType == null) return;

            if (!IsTypeCompatible(expressionType, expectedType, context))
            {
                context.ReportDiagnostic(Diagnostic.Create(
                    Rule,
                    location,
                    expectedType.Name,
                    expressionType.Name));
            }
        }

        private bool IsTypeCompatible(ITypeSymbol returnedType, ITypeSymbol expectedType, SyntaxNodeAnalysisContext context)
        {
            if (SymbolEqualityComparer.Default.Equals(returnedType, expectedType))
                return true;

            var actionResultType = context.Compilation.GetTypeByMetadataName("Microsoft.AspNetCore.Mvc.ActionResult");
            if (actionResultType != null && returnedType.InheritsFrom(actionResultType))
                return true;

            var actionResultTType = context.Compilation.GetTypeByMetadataName("Microsoft.AspNetCore.Mvc.ActionResult`1");
            if (actionResultTType != null && 
                returnedType.OriginalDefinition?.Equals(actionResultTType, SymbolEqualityComparer.Default) == true &&
                returnedType is INamedTypeSymbol namedType && 
                namedType.TypeArguments.Length == 1)
            {
                return SymbolEqualityComparer.Default.Equals(namedType.TypeArguments[0], expectedType);
            }

            return false;
        }
    }

    public static class SymbolExtensions
    {
        public static bool InheritsFrom(this ITypeSymbol type, ITypeSymbol possibleBaseType)
        {
            var current = type;
            while (current != null)
            {
                if (current.Equals(possibleBaseType, SymbolEqualityComparer.Default))
                    return true;
                current = current.BaseType;
            }
            return false;
        }
    }
}