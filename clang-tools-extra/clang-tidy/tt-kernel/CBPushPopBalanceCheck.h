#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_TT_KERNEL_CBPUSHPOPBALANCECHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_TT_KERNEL_CBPUSHPOPBALANCECHECK_H

#include "../ClangTidyCheck.h"

namespace clang {
namespace tidy {
namespace tt_kernel {

// Ensures that for each cb_push_back(ID, Qty), the downstream code
// contains cb_pop_front(ID, q1) + cb_pop_front(ID, q2) + ... such that
// sum(qi) == Qty before a return statement or another cb_push_back for the
// same ID is encountered.
class CBPushPopBalanceCheck : public ClangTidyCheck {
public:
  CBPushPopBalanceCheck(llvm::StringRef Name, ClangTidyContext *Context);

  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
};

} // namespace tt_kernel
} // namespace tidy
} // namespace clang

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_TT_KERNEL_CBPUSHPOPBALANCECHECK_H
