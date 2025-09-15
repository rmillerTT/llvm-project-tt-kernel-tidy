#include "CBPushPopBalanceCheck.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Lex/Lexer.h"
#include <optional>
#include <string>

using namespace clang;
using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace tt_kernel {

namespace {

std::optional<long long> evaluateInt(const Expr *E, ASTContext &Ctx) {
	Expr::EvalResult Result;
	if (E->EvaluateAsInt(Result, Ctx)) {
		return Result.Val.getInt().getSExtValue();
	}
	return std::nullopt;
}

std::string getExprTokenText(const Expr *E, const SourceManager &SM, const LangOptions &LangOpts) {
	CharSourceRange Range = CharSourceRange::getTokenRange(E->getSourceRange());
	return std::string(Lexer::getSourceText(Range, SM, LangOpts));
}

class FunctionWalker : public RecursiveASTVisitor<FunctionWalker> {
public:
	FunctionWalker(const SourceManager &SM, const LangOptions &LangOpts, SourceLocation StartLoc,
	              std::string TargetIdText, std::optional<long long> RequiredQty)
		: SM(SM), LangOpts(LangOpts), StartLoc(StartLoc), TargetIdText(std::move(TargetIdText)), RequiredQty(RequiredQty) {}

	bool VisitCallExpr(CallExpr *CE) {
		if (SM.isBeforeInTranslationUnit(StartLoc, CE->getExprLoc())) {
			const FunctionDecl *FD = CE->getDirectCallee();
			if (!FD)
				return true;
			StringRef Name = FD->getName();
			if (Name == "cb_pop_front" && CE->getNumArgs() >= 2) {
				const Expr *IdArg = CE->getArg(0)->IgnoreParenImpCasts();
				const Expr *QtyArg = CE->getArg(1)->IgnoreParenImpCasts();
				if (getExprTokenText(IdArg, SM, LangOpts) == TargetIdText) {
					// Accumulate quantity if known; otherwise mark unknown.
					auto Q = evaluateInt(QtyArg, *Context);
					if (Q.has_value()) {
						AccumulatedQty += Q.value();
					} else {
						HasUnknownPopQty = true;
					}
				}
			} else if (Name == "cb_push_back" && CE->getNumArgs() >= 2) {
				const Expr *IdArg = CE->getArg(0)->IgnoreParenImpCasts();
				if (getExprTokenText(IdArg, SM, LangOpts) == TargetIdText) {
					if (!isSatisfied()) {
						SawNextPushBeforeSatisfied = true;
					}
					// Either way, we stop further accounting once a new push for the same ID occurs.
					StopTraversal = true;
				}
			}
		}
		return !StopTraversal;
	}

	bool VisitReturnStmt(ReturnStmt *RS) {
		if (SM.isBeforeInTranslationUnit(StartLoc, RS->getBeginLoc())) {
			if (!isSatisfied()) {
				SawReturnBeforeSatisfied = true;
				StopTraversal = true;
			}
		}
		return !StopTraversal;
	}

	void setContext(ASTContext *C) { Context = C; }

	long long getAccumulatedQty() const { return AccumulatedQty; }
	bool hasUnknownPopQty() const { return HasUnknownPopQty; }
	bool sawReturnBeforeSatisfied() const { return SawReturnBeforeSatisfied; }
	bool sawNextPushBeforeSatisfied() const { return SawNextPushBeforeSatisfied; }
	bool isSatisfied() const {
		if (!RequiredQty.has_value()) return false;
		return AccumulatedQty == RequiredQty.value();
	}

private:
	const SourceManager &SM;
	const LangOptions &LangOpts;
	SourceLocation StartLoc;
	std::string TargetIdText;
	std::optional<long long> RequiredQty;
	ASTContext *Context = nullptr;

	long long AccumulatedQty = 0;
	bool HasUnknownPopQty = false;
	bool SawReturnBeforeSatisfied = false;
	bool SawNextPushBeforeSatisfied = false;
	bool StopTraversal = false;
};

} // namespace

CBPushPopBalanceCheck::CBPushPopBalanceCheck(StringRef Name, ClangTidyContext *Context)
	: ClangTidyCheck(Name, Context) {}

void CBPushPopBalanceCheck::registerMatchers(MatchFinder *Finder) {
	Finder->addMatcher(
		callExpr(
			callee(functionDecl(hasName("cb_push_back"))),
			hasAncestor(functionDecl(isDefinition()).bind("func")),
			hasArgument(0, expr().bind("id")),
			hasArgument(1, expr().bind("qty"))
		).bind("push"),
		this);
}

void CBPushPopBalanceCheck::check(const MatchFinder::MatchResult &Result) {
	const auto *PushCE = Result.Nodes.getNodeAs<CallExpr>("push");
	const auto *Func = Result.Nodes.getNodeAs<FunctionDecl>("func");
	const auto *IdExpr = Result.Nodes.getNodeAs<Expr>("id");
	const auto *QtyExpr = Result.Nodes.getNodeAs<Expr>("qty");
	if (!PushCE || !Func || !IdExpr || !QtyExpr)
		return;

	ASTContext &Ctx = *Result.Context;
	const SourceManager &SM = *Result.SourceManager;
	const LangOptions &LangOpts = Ctx.getLangOpts();

	auto RequiredQty = evaluateInt(QtyExpr->IgnoreParenImpCasts(), Ctx);
	std::string IdText = getExprTokenText(IdExpr->IgnoreParenImpCasts(), SM, LangOpts);

	if (!Func->hasBody()) return;

	FunctionWalker Walker(SM, LangOpts, PushCE->getExprLoc(), IdText, RequiredQty);
	Walker.setContext(&Ctx);
	Walker.TraverseStmt(Func->getBody());

	if (!RequiredQty.has_value()) {
		// Cannot verify without a known push quantity.
		diag(PushCE->getExprLoc(),
		     "cannot verify cb_push_back/cb_pop_front balance: push quantity is non-constant for id '%0'")
			<< IdText;
		return;
	}

	if (Walker.sawReturnBeforeSatisfied()) {
		diag(PushCE->getExprLoc(),
		     "return encountered before cb_pop_front total quantity (%0) matched cb_push_back quantity (%1) for id '%2'")
			<< Walker.getAccumulatedQty() << RequiredQty.value() << IdText;
		return;
	}

	if (Walker.sawNextPushBeforeSatisfied()) {
		diag(PushCE->getExprLoc(),
		     "another cb_push_back for id '%0' encountered before cb_pop_front total quantity (%1) matched required (%2)")
			<< IdText << Walker.getAccumulatedQty() << RequiredQty.value();
		return;
	}

	if (!Walker.isSatisfied()) {
		diag(PushCE->getExprLoc(),
		     "cb_pop_front total quantity (%0) does not match cb_push_back quantity (%1) for id '%2' by end of function")
			<< Walker.getAccumulatedQty() << RequiredQty.value() << IdText;
	}
}

} // namespace tt_kernel
} // namespace tidy
} // namespace clang
