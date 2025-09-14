#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_TT_KERNEL_CBRESERVEPUSHBALANCECHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_TT_KERNEL_CBRESERVEPUSHBALANCECHECK_H

#include "../ClangTidyCheck.h"
//#include "clang/ASTMatchers/ASTMatchFinder.h"
//#include "llvm/ADT/StringRef.h"

namespace clang {
namespace tidy {
namespace tt_kernel {

// Ensures that for each cb_reserve_back(ID, Qty), the downstream code
// contains cb_push_back(ID, q1) + cb_push_back(ID, q2) + ... such that
// sum(qi) == Qty before a return statement or another cb_reserve_back for the
// same ID is encountered.
class CBReservePushBalanceCheck : public ClangTidyCheck {
public:
  CBReservePushBalanceCheck(llvm::StringRef Name, ClangTidyContext *Context);

  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
};

} // namespace tt_kernel
} // namespace tidy
} // namespace clang

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MISC_CBRESERVEPUSHBALANCECHECK_H 

